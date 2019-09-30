/*
 *	NMH's Simple C Compiler, 2014--2016
 *	Syntax tree construction
 */

#include "defs.h"
#include "data.h"
#include "decl.h"

// Level and Bars[] is used by the dumpXXX()
// functions to print out the location of
// the vertical bars in the output.
#define	MAXBARS	100

int	Level = 0;
char	Bars[MAXBARS];

static void emittree1(node *a);

// A node in the abstract syntax tree has an operation, an optional
// operation value, and two pointers to child nodes: left and right.
// In a binary operation node, both pointers point to child nodes.
// In a unary operation node, only the left pointer has a child node.
// A leaf node represents a value, and the "operation" is a misnomer.

// Create a new AST node and add it to the Nodes[] storage area.
// args points at the arguments in the node. na is the number of
// argument pointers.
// Return a pointer to the node.
static node *mknode(int op, int na, int *args, node *left, node *right) {
	node	*n;
	int	hdrlen;

	// Check that we have enough room in Nodes[] for a new node
	hdrlen = sizeof(node) / sizeof(int) - 1;
	if (Ndtop + hdrlen + na >= NODEPOOLSZ)
		fatal("expression too complex (out of nodes)");

	// Get a pointer to the new node
	n = (node *) &Nodes[Ndtop];

	// Move Ndtop past the node just allocated
	Ndtop += hdrlen + na;

	// If we go past the end, reset Ndmax to indicate we're at the end
	if (Ndtop > Ndmax) Ndmax = Ndtop;

	// Copy the operation and pointers
	n->op = op;
	n->left = left,
	n->right = right;

	// Copy the array of argument pointers into the node
	// and return the node pointer
	memcpy(n->args, args, na * sizeof(int));
	return n;
}

// Make a leaf node with one argument. Operations are:
// OP_ADDR, OP_IDENT, OP_LDLAB and OP_LIT
node *mkleaf(int op, int n) {
	int	a[1];

	a[0] = n;
	return mknode(op, 1, a, NULL, NULL);
}

// Make a unary operation node with no arguments & one child.
// Operations are: OP_BOOL, OP_LOGNOT, OP_NEG, OP_NOT and OP_SCALE
node *mkunop(int op, node *left) {
	return mknode(op, 0, NULL, left, NULL);
}

// Make a unary operation node with one argument & one child.
// Operations are: OP_ADDR, OP_IFELSE, OP_LAB and OP_SCALEBY
node *mkunop1(int op, int n, node *left) {
	int	a[1];

	a[0] = n;
	return mknode(op, 1, a, left, NULL);
}

// Make a unary operation node with two arguments & one child.
// Operations are: OP_CALL, OP_CALR, OP_POSTDEC, OP_POSTINC,
// OP_PREDEC, OP_PREINC and OP_RVAL
node *mkunop2(int op, int n1, int n2, node *left) {
	int	a[2];

	a[0] = n1;
	a[1] = n2;
	return mknode(op, 2, a, left, NULL);
}

// Make a binary operation node with no arguments.
// Operations are OP_ADD, OP_COMMA, OP_GLUE, OP_LSHIFT and OP_RSHIFT
// as well as the operations returned by binop():
// OP_BINAND, OP_BINXOR, OP_EQUAL, OP_GREATER, OP_GTEQ, OP_LESS, OP_LSHIFT, OP_LTEQ,
// OP_SUB, OP_MOD, OP_NOTEQ, OP_BINIOR, OP_PLUS, OP_RSHIFT, OP_DIV and OP_MUL
node *mkbinop(int op, node *left, node *right) {
	return mknode(op, 0, NULL, left, right);
}

// Make a binary operation node with one argument. Operations are OP_BRFALSE
// and OP_BRTRUE as well as the operations returned by binop():
// OP_BINAND, OP_BINXOR, OP_EQUAL, OP_GREATER, OP_GTEQ, OP_LESS, OP_LSHIFT, OP_LTEQ,
// OP_SUB, OP_MOD, OP_NOTEQ, OP_BINIOR, OP_PLUS, OP_RSHIFT, OP_DIV and OP_MUL
node *mkbinop1(int op, int n, node *left, node *right) {
	int	a[1];

	a[0] = n;
	return mknode(op, 1, a, left, right);
}

// Make a binary operation node with two arguments.
// Operations are OP_ASSIGN as well as the operations returned by binop():
// OP_BINAND, OP_BINXOR, OP_EQUAL, OP_GREATER, OP_GTEQ, OP_LESS, OP_LSHIFT, OP_LTEQ,
// OP_SUB, OP_MOD, OP_NOTEQ, OP_BINIOR, OP_PLUS, OP_RSHIFT, OP_DIV and OP_MUL
node *mkbinop2(int op, int n1, int n2, node *left, node *right) {
	int	a[2];

	a[0] = n1;
	a[1] = n2;
	return mknode(op, 2, a, left, right);
}

// The following dumpXXX() functions
// are used to print out the contents
// of the AST.

static void dumpleaf(char *s, node *a) {
	switch (a->op) {
		case OP_LIT:  printf("%s %d\n", s, a->args[0]); break;
		default: printf("%s %s\n", s, Syms[ a->args[0] ].name);
	}
}

static void dumpunop1(char *s, node *a) {
	switch (a->op) {
		case OP_LDLAB: printf("%s L%d\n", s, a->args[0]); break;
		default:  printf("%s %d\n", s, a->args[0]);
	}
	Level++;
	dumptree(a->left);
	Level--;
}

static void dumpunop2(char *s, node *a) {
	switch (a->op) {
		case OP_RVAL: printf(" %s %d %s\n", s, a->args[0], Syms[ a->args[1] ].name); break;
		case OP_CALL: printf(" %s() %d\n", Syms[ a->args[0] ].name, a->args[1]); break;
		default:      printf("%s %d %d\n", s, a->args[0], a->args[1]);
	}
	Level++;
	dumptree(a->left);
	Level--;
}

static void dumpunop(char *s, node *a) {
	printf("%s\n", s);
	Level++;
	dumptree(a->left);
	Level--;
}

static void dumpbinop(char *s, node *a) {
	printf("%s\n", s);
	Level++;
	if (Level < MAXBARS) Bars[Level] = 1;
	dumptree(a->left);
	if (Level < MAXBARS) Bars[Level] = 0;
	dumptree(a->right);
	Level--;
}

static void dumpbinop1(char *s, node *a) {
	printf("%s %d\n", s, a->args[0]);
	Level++;
	if (Level < MAXBARS) Bars[Level] = 1;
	dumptree(a->left);
	if (Level < MAXBARS) Bars[Level] = 0;
	dumptree(a->right);
	Level--;
}

static void dumpbinop2(char *s, node *a) {
	printf("%s %d %d\n", s, a->args[0], a->args[1]);
	Level++;
	if (Level < MAXBARS) Bars[Level] = 1;
	dumptree(a->left);
	if (Level < MAXBARS) Bars[Level] = 0;
	dumptree(a->right);
	Level--;
}

void dumptree(node *a) {
	int	i;

	if (NULL == a) return;
	for (i=0; i<Level; i++)
		printf(Level > MAXBARS || !Bars[i]? "  ": "| ");
	if (Level) printf("`-");
	switch (a->op) {
	case OP_ADDR:	dumpleaf("addr", a); break;
	case OP_IDENT:	dumpleaf("id", a); break;
	case OP_LIT:	dumpleaf("lit", a); break;
	case OP_BOOL:	dumpunop("!!x", a); break;
	case OP_PREINC:	dumpunop2("++x", a); break;
	case OP_PREDEC:	dumpunop2("--x", a); break;
	case OP_POSTINC:dumpunop2("x++", a); break;
	case OP_POSTDEC:dumpunop2("x--", a); break;
	case OP_RVAL:	dumpunop2("*x", a); break;
	case OP_LAB:	dumpunop1("label", a); break;
	case OP_LDLAB:	dumpunop1("ldlab", a); break;
	case OP_BRFALSE:dumpbinop1("jump/false", a); break;
	case OP_BRTRUE:	dumpbinop1("jump/true", a); break;
	case OP_IFELSE:	dumpunop1("?:", a); break;
	case OP_LOGNOT:	dumpunop("!x", a); break;
	case OP_NEG:	dumpunop("-x", a); break;
	case OP_NOT:	dumpunop("~x", a); break;
	case OP_SCALE:	dumpunop("scale", a); break;
	case OP_SCALEBY:dumpunop1("scaleby", a); break;
	case OP_ADD:	dumpbinop("x+y (int,int)", a); break;
	case OP_PLUS:	dumpbinop2("x+y", a); break;
	case OP_MUL:	dumpbinop("x*y", a); break;
	case OP_SUB:	dumpbinop("x-y", a); break;
	case OP_BINAND:	dumpbinop("x&y", a); break;
	case OP_BINIOR:	dumpbinop("x|y", a); break;
	case OP_BINXOR:	dumpbinop("x^y", a); break;
	case OP_DIV:	dumpbinop("x/y", a); break;
	case OP_EQUAL:	dumpbinop("x==y", a); break;
	case OP_GLUE:	dumpbinop("glue", a); break;
	case OP_COMMA:	dumpbinop("x,y", a); break;
	case OP_GREATER:dumpbinop("x>y", a); break;
	case OP_GTEQ:	dumpbinop("x>=y", a); break;
	case OP_LESS:	dumpbinop("x<y", a); break;
	case OP_LSHIFT:	dumpbinop("x<<y", a); break;
	case OP_LTEQ:	dumpbinop("x<=y", a); break;
	case OP_MOD:	dumpbinop("x%y", a); break;
	case OP_NOTEQ:	dumpbinop("x!=y", a); break;
	case OP_RSHIFT:	dumpbinop("x>>y", a); break;
	case OP_ASSIGN:	dumpbinop2("x=y", a); break;
	case OP_CALL:	dumpunop2("x()", a); break;
	case OP_CALR:	dumpunop2("(*x)()", a); break;
	}
}

void emitcond(node *a, int ex) {
	genstoreregs();
	if (OP_GLUE == a->left->left->op)
		emitcond(a->left->left, ex);
	emittree1(a->left->left);
	genbrfalse(a->left->args[0]);
	genrestoreregs(0);
	emittree1(a->left->right);
	genjump(ex);
	commit();
	genlab(a->left->args[0]);
	genrestoreregs(1);
	emittree1(a->right);
}

void emitargs(node *a) {
	if (NULL == a) return;
	emittree1(a->right);
	genpush();
	emitargs(a->left);
}

static void emittree1(node *a) {
	struct lvalue lv;
	int	ptr;

	if (NULL == a) return;
	switch (a->op) {
	case OP_IDENT:	/* ignore */ break;
	case OP_ADDR:	genaddr(a->args[0]); break;
	case OP_LIT:	genlit(a->args[0]); break;
	case OP_PREINC:	/* fallthru */
	case OP_PREDEC:	/* fallthru */
	case OP_POSTINC:/* fallthru */
	case OP_POSTDEC:lv.prim = a->args[0];
			lv.sym = a->args[1];
			emittree1(a->left);
			switch (a->op) {
			case OP_PREINC:	geninc(&lv, 1, 1); break;
			case OP_PREDEC:	geninc(&lv, 0, 1); break;
			case OP_POSTINC:geninc(&lv, 1, 0); break;
			case OP_POSTDEC:geninc(&lv, 0, 0); break;
			}
			break;
	case OP_SCALEBY:emittree1(a->left);
			genscaleby(a->args[0]);
			break;
	case OP_LAB:	emittree1(a->left);
			commit();
			genlab(a->args[0]);
			break;
	case OP_LDLAB:	genldlab(a->args[0]); break;
	case OP_RVAL:	emittree1(a->left);
			lv.prim = a->args[0];
			lv.sym = a->args[1];
			genrval(&lv);
			break;
	case OP_BOOL:	emittree1(a->left);
			genbool();
			break;
	case OP_LOGNOT:	/* fallthru */
	case OP_NEG:	/* fallthru */
	case OP_NOT:	/* fallthru */
	case OP_SCALE:	emittree1(a->left);
			switch (a->op) {
			case OP_BOOL:	genbool(); break;
			case OP_LOGNOT:	genlognot(); break;
			case OP_NEG:	genneg(); break;
			case OP_NOT:	gennot(); break;
			case OP_SCALE:	genscale(); break;
			}
			break;
	case OP_BRFALSE:/* fallthru */
	case OP_BRTRUE:	emittree1(a->left);
			commit();
			a->op == OP_BRTRUE?
				genbrtrue(a->args[0]):
				genbrfalse(a->args[0]);
			freeregs(0);
			emittree1(a->right);
			break;
	case OP_IFELSE:
			emitcond(a->left, a->args[0]);
			commit();
			genlab(a->args[0]);
			break;
	case OP_COMMA:	emittree1(a->left);
			commit();
			freeregs(0);
			emittree1(a->right);
			break;
	case OP_EQUAL:	/* fallthru */
	case OP_NOTEQ:	/* fallthru */
	case OP_LESS:	/* fallthru */
	case OP_GREATER:/* fallthru */
	case OP_LTEQ:	/* fallthru */
	case OP_GTEQ:	emittree1(a->left);
			emittree1(a->right);
			commit();
			ptr = !inttype(a->args[0]);
			switch(a->op) {
			case OP_EQUAL:	queue_cmp(equal); break;
			case OP_NOTEQ:	queue_cmp(not_equal); break;
			case OP_LESS:	queue_cmp(ptr? below: less); break;
			case OP_GREATER:queue_cmp(ptr? above: greater); break;
			case OP_LTEQ:	queue_cmp(ptr? below_equal:
						less_equal);
					break;
			case OP_GTEQ:	queue_cmp(ptr? above_equal:
						greater_equal);
					break;
			}
			break;
	case OP_MOD:	/* fallthru */
	case OP_LSHIFT:	/* fallthru */
	case OP_RSHIFT:	/* fallthru */
	case OP_DIV:	/* fallthru */
	case OP_BINAND:	/* fallthru */
	case OP_BINIOR:	/* fallthru */
	case OP_BINXOR:	/* fallthru */
	case OP_MUL:	/* fallthru */
	case OP_SUB:	/* fallthru */
	case OP_PLUS:	/* fallthru */
	case OP_ADD:	emittree1(a->left);
			emittree1(a->right);
			commit();
			switch(a->op) {
			case OP_LSHIFT:	genshl(1); break;
			case OP_RSHIFT:	genshr(1); break;
			case OP_DIV:	gendiv(0); break;
			case OP_MOD:	genmod(0); break;
			case OP_BINAND:	genand(); break;
			case OP_BINIOR:	genior(); break;
			case OP_BINXOR:	genxor(); break;
			case OP_MUL:	genmul(); break;
			case OP_ADD:	genadd(PINT, PINT, 1); break;
			case OP_PLUS:	genadd(a->args[0], a->args[1], 0);
					break;
			case OP_SUB:	gensub(a->args[0], a->args[1], 1);
					break;
			}
			break;
	case OP_CALL:	genspillregs();
			emitargs(a->left);
			commit();
			gencall(a->args[0]);
			genstack((a->args[1]) * BPW);
			genunspillregs();
			break;
	case OP_CALR:	genspillregs();
			emitargs(a->left);
			commit();
			lv.prim = FUNPTR;
			lv.sym = a->args[0];
			genrval(&lv);
			gencalr();
			genstack((a->args[1]) * BPW);
			genunspillregs();
			break;
	case OP_ASSIGN: if (OP_IDENT == a->left->op) {
				emittree1(a->right);
				commit();
				emittree1(a->left);
			}
			else {
				emittree1(a->left);
				commit();
				emittree1(a->right);
				commit();
			}
			lv.prim = a->args[0];
			lv.sym = a->args[1];
			genstore(&lv);
			break;
	case OP_RETURN: emittree1(a->left);
			commit();
			genretvalue();
			break;
	}
}

void emittree(node *a) {
	a = optimize(a);
	if (O_dumptree) {
		puts("Tree\n----");
		dumptree(a);
		puts("");
	}
	emittree1(a);
}
