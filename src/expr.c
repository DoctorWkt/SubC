/*
 *	NMH's Simple C Compiler, 2011--2016
 *	Expression parser
 */

#include "defs.h"
#include "data.h"
#include "decl.h"
#include "prec.h"

static node *asgmnt(struct lvalue *lv);
static node *cast(struct lvalue *lv);
static node *exprlist(struct lvalue *lv, int ckvoid);

// Convert an lvalue AST node into an rvalue node. If this really is an
// lvalue, generate an OP_RVAL AST node to get the symbol's value which
// is of type prim. Otherwise, just return this node as an rvalue.
static node *rvalue(node *n, struct lvalue *lv) {
	if (lv->addr) {
		lv->addr = 0;
		return mkunop2(OP_RVAL, lv->prim, lv->sym, n);
	}
	else {
		return n;
	}
}

/*
 * primary :=
 *	  IDENT
 *	| INTLIT
 *	| string
 *	| ARGC
 *	| ( expr )
 *
 * string :=
 *	  STRLIT
 *	| STRLIT string
 */

// Given an lvalue node, parse a primary factor and return an
// AST node representing it.
static node *primary(struct lvalue *lv) {
	node	*n = NULL;
	int	y, lab, k;
	char	name[NAMELEN+1];

	// Start with it empty
	lv->prim = lv->sym = lv->addr = 0;

	switch (Token) {
	case IDENT:
		// Find the identifier in the symbol table,
		// then copy it so we can scan the next token
		y = findsym(Text);
		copyname(name, Text);
		Token = scan();

		// No symbol found. If the next token is a '(',
		// assume this is a function declaration. Add
		// it as an extern int function to the symbol table.
		// Otherwise, it's an undeclared variable. It's still
		// added to the symbol table as an auto int, to reduce
		// the number of further errors when the variable is used.
		if (!y) {
			if (LPAREN == Token) {
				y = addglob(name, PINT, TFUNCTION, CEXTERN,
					-1, 0, NULL, 0);
			}
			else {
				error("undeclared variable: %s", name);
				y = addloc(name, PINT, TVARIABLE, CAUTO,
					0, 0, 0);
			}
		}

		// Save the symbol and primary type of the symbol
		lv->sym = y;
		lv->prim = Prims[y];

		// If the next token isn't a '(', it's a function
		// pointer, so make a function pointer node
		// for the given symbol
		if (TFUNCTION == Types[y]) {
			if (LPAREN != Token) {
				lv->prim = FUNPTR;
				n = mkleaf(OP_ADDR, y);
			}
			return n;
		}

		// Constant: make an OP_LIT node with the value
		if (TCONSTANT == Types[y]) {
			return mkleaf(OP_LIT, Vals[y]);
		}

		// Array: make an OP_ADDR node with the
		// type being a pointer to the array's primitive type
		if (TARRAY == Types[y]) {
			n = mkleaf(OP_ADDR, y);
			lv->prim = pointerto(lv->prim);
			return n;
		}

		// If it's a struct or union,
		// make an OP_ADDR node to the struct/union
		if (comptype(Prims[y])) {
			n = mkleaf(OP_ADDR, y);
			lv->sym = 0;	// XXX: Why this?
			return n;
		}

		// An ordinary scalar variable, so make an IDENT node
		// for the symbol, and mark it as a true lvalue
		n = mkleaf(OP_IDENT, y);
		lv->addr = 1;
		return n;
	case INTLIT:
		// Literal integer, make an OP_LIT node with the value
		// and an int primary type. Move up to the next token.
		n = mkleaf(OP_LIT, Value);
		Token = scan();
		lv->prim = PINT;
		return n;
	case STRLIT:
		gendata();			// Switch to the data section
		lab = label();			// Make a new label and
		genlab(lab);			// output it
		k = 0;
		while (STRLIT == Token) {	// Allow successive "x" "y"
			gendefs(Text, Value);	// string literals
			k += Value-2;		// XXX: length - ""?
			Token = scan();		// until we hit something else
		}
		gendefb(0);			// Output the NUL terminator
		genalign(k+1);			// Align on a word boundary
		n = mkleaf(OP_LDLAB, lab);	// Node refers to the label
		lv->prim = CHARPTR;		// and is a char pointer
						// Not marked as addr=1
						// as the string is mutable
		return n;
	case LPAREN:
		Token = scan();			// Skip the '('
		n = exprlist(lv, 0);		// Parse the expression list
		rparen();			// Match the ')' and
		return n;			// return the node
	default:
		// It's a syntax error, skip tokens up to the next semicolon
		error("syntax error at: %s", Text);
		Token = synch(SEMI);
		return NULL;
	}
}

// Return true if the two types match.
// If they are identical, they match.
// Two int types (int and char) match.
// Any pointer matches a void pointer.
int typematch(int p1, int p2) {
	if (p1 == p2) return 1;
	if (inttype(p1) && inttype(p2)) return 1;
	if (!inttype(p1) && VOIDPTR == p2) return 1;
	if (VOIDPTR == p1 && !inttype(p2)) return 1;
	return 0;
}

/*
 * fnargs :=
 *	  asgmnt
 *	| asgmnt , fnargs
 */

static node *fnargs(int fn, int *na) {
	struct lvalue lv;
	int	*types;
	char	msg[100];
	int	sgn[MAXFNARGS+1];
	node	*n = NULL, *n2;

	types = (int *) (fn? Mtext[fn]: NULL);
	*na = 0;
	while (RPAREN != Token) {
		n2 = asgmnt(&lv);
		n2 = rvalue(n2, &lv);
		n = mkbinop(OP_GLUE, n, n2);
		if (comptype(lv.prim)) {
			error("struct/union passed by value", NULL);
			lv.prim = pointerto(lv.prim);
		}
		if (types && *types) {
			if (!typematch(*types, lv.prim)) {
				sprintf(msg, "wrong type in argument %d"
					" of call to: %%s",
					*na+1);
				error(msg, Names[fn]);
			}
			types++;
		}
		if (*na < MAXFNARGS) sgn[*na] = lv.prim, sgn[*na+1] = 0;
		(*na)++;
		if (COMMA == Token) {
			Token = scan();
			if (RPAREN == Token)
				error("trailing ',' in function call", NULL);
		}
		else
			break;
	}
	if (fn && TFUNCTION == Types[fn] && !Mtext[fn]) {
		Mtext[fn] = galloc((*na+1) * sizeof(int), 1);
		memcpy(Mtext[fn], sgn, (*na+1) * sizeof(int));
	}
	rparen();
	return n;
}

int deref(int p) {
	int	y;

	switch (p) {
	case INTPP:	return INTPTR;
	case INTPTR:	return PINT;
	case CHARPP:	return CHARPTR;
	case CHARPTR:	return PCHAR;
	case VOIDPP:	return VOIDPTR;
	case VOIDPTR:	return PCHAR;
	case FUNPTR:	return PCHAR;
	}
	y = p & ~STCMASK;
	switch (p & STCMASK) {
	case STCPP:	return STCPTR | y;
	case STCPTR:	return PSTRUCT | y;
	case UNIPP:	return UNIPTR | y;
	case UNIPTR:	return PUNION | y;
	}
	return -1;
}

static node *indirection(node *n, struct lvalue *lv) {
	int	p;

	n = rvalue(n, lv);
	if (VOIDPTR == lv->prim)
		error("dereferencing void pointer", NULL);
	if ((p = deref(lv->prim)) < 0) {
		if (lv->sym)
			error("indirection through non-pointer: %s",
				Names[lv->sym]);
		else
			error("indirection through non-pointer", NULL);
		p = lv->prim;
	}
	lv->prim = p;
	lv->sym = 0;
	return n;
}

static void badcall(struct lvalue *lv) {
	if (lv->sym)
		error("call of non-function: %s",
			Names[lv->sym]);
	else
		error("call of non-function", NULL);
}

static int argsok(int na, int nf) {
	return na == nf || (nf < 0 && na >= -nf-1);
}

static node *stc_access(node *n, struct lvalue *lv, int ptr) {
	int	y, p;
	node	*n2;

	n2 = n;
	p = lv->prim & STCMASK;
	lv->addr = 1;
	if (IDENT != Token) {
		Token = scan();
		error("struct/union member name expected after '%s'",
			ptr? "->": ".");
		return NULL;
	}
	y = findmem(lv->prim & ~STCMASK, Text);
	if (0 == y)
		error("struct/union has no such member: %s", Text);
	if ((PSTRUCT == p || STCPTR == p) && Vals[y]) {
		n2 = mkleaf(OP_LIT, Vals[y]);
		n2 = mkbinop(OP_ADD, n, n2);
	}
	Token = scan();
	p = Prims[y];
	if (TARRAY == Types[y]) {
		p = pointerto(p);
		lv->addr = 0;
	}
	lv->prim = p;
	return n2;
}

/*
 * postfix :=
 *	  primary
 *	| postfix [ expr ]
 *	| postfix ( )
 *	| postfix ( fnargs )
 *	| postfix ++
 *	| postfix --
 *	| postfix . identifier
 *	| postfix -> identifier
 */

static node *postfix(struct lvalue *lv) {
	node	*n = NULL, *n2;
	int	p, na;
	struct lvalue lv2;

	n = primary(lv);
	for (;;) {
		switch (Token) {
		case LBRACK:
			while (LBRACK == Token) {
				n = indirection(n, lv);
				Token = scan();
				n2 = exprlist(&lv2, 1);
				n2 = rvalue(n2, &lv2);
				p = lv->prim;
				if (PINT != lv2.prim)
					error("non-integer subscript", NULL);
				if (    PINT == p || INTPTR == p ||
					CHARPTR == p || VOIDPTR == p ||
					STCPTR == (p & STCMASK) ||
					UNIPTR == (p & STCMASK)
				) {
					n2 = mkunop(OP_SCALE, n2);
				}
				else if (comptype(p)) {
					n2 = mkunop1(OP_SCALEBY,
						objsize(p, TVARIABLE, 1), n2);
				}
				n = mkbinop(OP_ADD, n, n2);
				rbrack();
				lv->sym = 0;
				lv->addr = 1;
			}
			break;
		case LPAREN:
			Token = scan();
			n = fnargs(lv->sym, &na);
			if (lv->sym && TFUNCTION == Types[lv->sym]) {
				if (!argsok(na, Sizes[lv->sym]))
					error("wrong number of arguments: %s",
						Names[lv->sym]);
				n = mkunop2(OP_CALL, lv->sym, na, n);
			}
			else {
				if (lv->prim != FUNPTR) badcall(lv);
				n = mkunop2(OP_CALR, lv->sym, na, n);
				lv->prim = PINT;
			}
			lv->addr = 0;
			break;
		case INCR:
		case DECR: 
			if (lv->addr) {
				if (INCR == Token)
					n = mkunop2(OP_POSTINC, lv->prim,
						lv->sym, n);
				else
					n = mkunop2(OP_POSTDEC, lv->prim,
						lv->sym, n);
			}
			else
				error("lvalue required before '%s'", Text);
			Token = scan();
			lv->addr = 0;
			break;
		case DOT:
			Token = scan();
			if (comptype(lv->prim))
				n = stc_access(n, lv, 0);
			else
				error("struct/union expected before '.'",
					NULL);
			break;
		case ARROW:
			Token = scan();
			p = lv->prim & STCMASK;
			if (p == STCPTR || p == UNIPTR) {
				n = rvalue(n, lv);
				n = stc_access(n, lv, 1);
			}
			else
				error(
				 "struct/union pointer expected before '->'",
				 NULL);
			lv->sym = 0;
			break;
		default:
			return n;
		}
	}
}

static node *prefix(struct lvalue *lv);

static node *comp_size(void) {
	int	k = 0, y;
	struct lvalue lv;

	if (	CHAR == Token || INT == Token || VOID == Token ||
		STRUCT == Token || UNION == Token
	) {
		switch (Token) {
		case CHAR:	k = CHARSIZE; break;
		case INT:	k = INTSIZE; break;
		case STRUCT:
		case UNION:	k = primtype(Token, NULL);
				k = objsize(k, TVARIABLE, 1);
				break;
		}
		Token = scan();
		if (STAR == Token) {
			k = PTRSIZE;
			Token = scan();
			if (STAR == Token) Token = scan();
		}
		else if (0 == k) {
			error("sizeof(void) is unknown", NULL);
		}
	}
	else {
		prefix(&lv);
		y = lv.sym? lv.sym: 0;
		k = y? objsize(Prims[y], Types[y], Sizes[y]):
			objsize(lv.prim, TVARIABLE, 1);
		if (0 == k)
			error("cannot compute sizeof: %s",
				Text);
	}
	return mkleaf(OP_LIT, k);
}

/*
 * prefix :=
 *	  postfix
 *	| ++ prefix
 *	| -- prefix
 *	| & cast
 *	| * cast
 *	| + cast
 *	| - cast
 *	| ~ cast
 *	| ! cast
 *	| SIZEOF ( type )
 *	| SIZEOF ( type * )
 *	| SIZEOF ( type * * )
 *	| SIZEOF ( IDENT )
 *
 * type :=
 *	  INT
 *	| CHAR
 *	| VOID
 *	| STRUCT IDENT
 *	| UNION IDENT
 */

static node *prefix(struct lvalue *lv) {
	node	*n;
	int	t;

	switch (Token) {
	case INCR:
	case DECR:
		t = Token;
		Token = scan();
		n = prefix(lv);
		if (lv->addr) {
			if (INCR == t)
				n = mkunop2(OP_PREINC, lv->prim,
					lv->sym, n);
			else
				n = mkunop2(OP_PREDEC, lv->prim,
					lv->sym, n);
		}
		else {
			error("lvalue expected after '%s'",
				t == INCR? "++": "--");
		}
		lv->addr = 0;
		return n;
	case STAR:
		Token = scan();
		n = cast(lv);
		n = indirection(n, lv);
		lv->addr = 1;
		return n;
	case PLUS:
		Token = scan();
		n = cast(lv);
		n = rvalue(n, lv); /* XXX really? */
		if (!inttype(lv->prim))
			error("bad operand to unary '+'", NULL);
		lv->addr = 0;
		return n;
	case MINUS:
		Token = scan();
		n = cast(lv);
		n = rvalue(n, lv);
		if (!inttype(lv->prim))
			error("bad operand to unary '-'", NULL);
		n = mkunop(OP_NEG, n);
		lv->addr = 0;
		return n;
	case TILDE:
		Token = scan();
		n = cast(lv);
		n = rvalue(n, lv);
		if (!inttype(lv->prim))
			error("bad operand to '~'", NULL);
		n = mkunop(OP_NOT, n);
		lv->addr = 0;
		return n;
	case XMARK:
		Token = scan();
		n = cast(lv);
		n = rvalue(n, lv);
		n = mkunop(OP_LOGNOT, n);
		lv->prim = PINT;
		lv->addr = 0;
		return n;
	case AMPER:
		Token = scan();
		n = cast(lv);
		if (lv->addr) {
			if (lv->sym) n = mkunop1(OP_ADDR, lv->sym, n);
		}
		else if ((0 == lv->sym || Types[lv->sym] != TARRAY) &&
			 !comptype(lv->prim)
		) {
			error("lvalue expected after unary '&'", NULL);
		}
		lv->prim = pointerto(lv->prim);
		lv->addr = 0;
		return n;
	case SIZEOF:
		Token = scan();
		lparen();
		n = comp_size();
		rparen();
		lv->prim = PINT;
		lv->addr = 0;
		return n;
	default:
		return postfix(lv);
	}
}

/*
 * cast :=
 *	  prefix
 *	| ( type ) prefix
 *	| ( type * ) prefix
 *	| ( type * * ) prefix
 *	| ( INT ( * ) ( ) ) prefix
 */

static node *cast(struct lvalue *lv) {
	int	t;
	node	*n;

	if (LPAREN == Token) {
		Token = scan();
		if (	INT == Token || CHAR == Token || VOID == Token ||
			STRUCT == Token || UNION == Token
		) {
			t = primtype(Token, NULL);
			Token = scan();
		}
		else {
			reject();
			Token = LPAREN;
			strcpy(Text, "(");
			return prefix(lv);
		}
		if (PINT == t && LPAREN == Token) {
			Token = scan();
			match(STAR, "int(*)()");
			rparen();
			lparen();
			rparen();
			t = FUNPTR;
		}
		else if (STAR == Token) {
			t = pointerto(t);
			Token = scan();
			if (STAR == Token) {
				t = pointerto(t);
				Token = scan();
			}
		}
		rparen();
		n = prefix(lv);
		lv->prim = t;
		return n;
	}
	else {
		return prefix(lv);
	}
}

// Convert operator tokens into AST operators
int binop(int tok) {
	switch(tok) {
	case AMPER:	return OP_BINAND;
	case CARET:	return OP_BINXOR;
	case EQUAL:	return OP_EQUAL;
	case GREATER:	return OP_GREATER;
	case GTEQ:	return OP_GTEQ;
	case LESS:	return OP_LESS;
	case LSHIFT:	return OP_LSHIFT;
	case LTEQ:	return OP_LTEQ;
	case MINUS:	return OP_SUB;
	case MOD:	return OP_MOD;
	case NOTEQ:	return OP_NOTEQ;
	case PIPE:	return OP_BINIOR;
	case PLUS:	return OP_PLUS;
	case RSHIFT:	return OP_RSHIFT;
	case SLASH:	return OP_DIV;
	case STAR:	return OP_MUL;
	default:	fatal("internal: unknown binop");
			return 0; /* notreached */
	}
}

node *mkop(int op, int p1, int p2, node *l, node *r) {
	if (PLUS == op || MINUS == op) {
		return mkbinop2(binop(op), p1, p2, l, r);
	}
	else if (EQUAL == op || NOTEQ == op || LESS == op ||
		 GREATER == op || LTEQ == op || GTEQ == op)
	{
		return mkbinop1(binop(op), p1, l, r);
	}
	else {
		return mkbinop(binop(op), l, r);
	}
}

/*
 * term :=
 *	  cast
 *	| term * cast
 *	| term / cast
 *	| term % cast
 *
 * sum :=
 *	  term
 *	| sum + term
 *	| sum - term
 *
 * shift :=
 *	  sum
 *	| shift << sum
 *	| shift >> sum
 *
 * relation :=
 *	  shift
 *	| relation < shift
 *	| relation > shift
 *	| relation <= shift
 *	| relation >= shift
 *
 * equation :=
 *	  relation
 *	| equation == relation
 *	| equation != relation
 *
 * binand :=
 *	  equation
 *	| binand & equation
 *
 * binxor :=
 *	  binand
 *	| binxor ^ binand
 *
 * binor :=
 *	  binxor
 *	| binor '|' binxor
 *
 * binexpr :=
 *	  binor
 */

static node *binexpr(struct lvalue *lv) {
	int	ops[9];
	int	prims[10];
	int	sp = 0;
	int	a;
	struct lvalue lv2;
	node	*tree[10];

	tree[0] = cast(lv);
	a = lv->addr;
	prims[0] = lv->prim;
	while (SLASH == Token || STAR == Token || MOD == Token ||
		PLUS == Token || MINUS == Token || LSHIFT == Token ||
		RSHIFT == Token || GREATER == Token || GTEQ == Token ||
		LESS == Token || LTEQ == Token || EQUAL == Token ||
		NOTEQ == Token || AMPER == Token || CARET == Token ||
		PIPE == Token
	) {
		tree[0] = rvalue(tree[0], lv);
		while (sp > 0 && Prec[Token] <= Prec[ops[sp-1]]) {
			tree[sp-1] = mkop(ops[sp-1], prims[sp-1], prims[sp],
					tree[sp-1], tree[sp]);
			prims[sp-1] = binoptype(ops[sp-1], prims[sp-1],
					prims[sp]);
			sp--;
		}
		ops[sp++] = Token;
		Token = scan();
		tree[sp] = cast(&lv2);
		tree[sp] = rvalue(tree[sp], &lv2);
		prims[sp] = lv2.prim;
		a = 0;
	}
	while (sp > 0) {
		tree[sp-1] = mkop(ops[sp-1], prims[sp-1], prims[sp],
				tree[sp-1], tree[sp]);
		prims[sp-1] = binoptype(ops[sp-1], prims[sp-1], prims[sp]);
		sp--;
	}
	lv->prim = prims[0];
	lv->addr = a;
	return tree[0];
}

/*
 * logand :=
 *	  binexpr
 *	| logand && binexpr
 *
 * logor :=
 *	  logand
 *	| logor '||' logand
 */

static node *cond2(struct lvalue *lv, int op) {
	struct lvalue lv2;
	int	lab = 0;
	node	*n, *n2 = NULL;
	int	tv = 1;

	n = op == LOGOR? cond2(lv, LOGAND): binexpr(lv);
	while (Token == op) {
		if (!lab) lab = label();
		if (tv) notvoid(lv->prim), tv = 0;
		n = rvalue(n, lv);
		Token = scan();
		n2 = op == LOGOR? cond2(&lv2, LOGAND): binexpr(&lv2);
		n2 = rvalue(n2, &lv2);
		if (op == LOGOR)
			n = mkbinop1(OP_BRTRUE, lab, n, n2);
		else
			n = mkbinop1(OP_BRFALSE, lab, n, n2);
	}
	if (lab) {
		n = mkunop1(OP_LAB, lab, n);
		n = mkunop(OP_BOOL, n);
		lv->prim = PINT;
		lv->addr = 0;
	}
	return n;
}

/*
 * condexpr :=
 *	  logor
 *	| logor ? expr : condexpr
 */

static node *cond3(struct lvalue *lv) {
	node	*n, *n2;
	int	p;
	struct lvalue lv2;
	int	l1 = 0, l2 = 0, tv = 1;

	n = cond2(lv, LOGOR);
	p = 0;
	while (QMARK == Token) {
		n = rvalue(n, lv);
		if (tv) notvoid(lv->prim), tv = 0;
		l1 = label();
		if (!l2) l2 = label();
		Token = scan();
		n2 = exprlist(&lv2, 0);
		n2 = rvalue(n2, &lv2);
		n = mkbinop1(OP_BRFALSE, l1, n, n2);
		if (!p) p = lv2.prim;
		if (!typematch(p, lv2.prim))
			error("incompatible types in '?:'", NULL);
		colon();
		n2 = cond2(&lv2, LOGOR);
		n2 = rvalue(n2, &lv2);
		n = mkbinop(OP_GLUE, n, n2);
		if (QMARK != Token)
			if (!typematch(p, lv2.prim))
				error("incompatible types in '?:'", NULL);
	}
	if (l2) {
		n = mkunop1(OP_IFELSE, l2, n);
		lv->prim = p;
		lv->addr = 0;
	}
	return n;
}

int arithop(int tok) {
	switch(tok) {
	case ASPLUS:	return PLUS;
	case ASMINUS:	return MINUS;
	case ASAND:	return AMPER;
	case ASOR:	return PIPE;
	case ASXOR:	return CARET;
	case ASMUL:	return STAR;
	case ASMOD:	return MOD;
	case ASDIV:	return SLASH;
	case ASLSHIFT:	return LSHIFT;
	case ASRSHIFT:	return RSHIFT;
	default:	fatal("internal: unknown assignment operator");
			return 0; /* notreached */
	}
}

/*
 * asgmnt :=
 *	  condexpr
 *	| condexpr = asgmnt
 *	| condexpr *= asgmnt
 *	| condexpr /= asgmnt
 *	| condexpr %= asgmnt
 *	| condexpr += asgmnt
 *	| condexpr -= asgmnt
 *	| condexpr <<= asgmnt
 *	| condexpr >>= asgmnt
 *	| condexpr &= asgmnt
 *	| condexpr ^= asgmnt
 *	| condexpr |= asgmnt
 */

// Parse the possible assignment expressions
static node *asgmnt(struct lvalue *lv) {
	node	*n, *n2, *src;
	struct lvalue lv2, lvs;
	int	op;

	// Parse the conditional expression
	n = cond3(lv);

	// If it's followed by one of these tokens
	if (ASSIGN == Token || ASDIV == Token || ASMUL == Token ||
		ASMOD == Token || ASPLUS == Token || ASMINUS == Token ||
		ASLSHIFT == Token || ASRSHIFT == Token || ASAND == Token ||
		ASXOR == Token || ASOR == Token
	) {
		// Keep the token and scan the next token
		op = Token;
		Token = scan();

		// Parse the following expression
		// and convert it into a proper rvalue
		n2 = asgmnt(&lv2);
		n2 = rvalue(n2, &lv2);

		// If this is a straight assignment (no operation),
		// check that the lvalue/rvalue types match,
		// and generate an OP_ASSIGN node: sym = rvalue
		if (ASSIGN == op) {
			if (!typematch(lv->prim, lv2.prim))
				error("assignment from incompatible type",
					NULL);
			n = mkbinop2(OP_ASSIGN, lv->prim, lv->sym, n, n2);
		}
		else {
			// Copy the node from the left-hand side
			// and convert it into an rvalue to get its value
			memcpy(&lvs, lv, sizeof(lvs));
			src = rvalue(n, &lvs);

			// Build an node to perform the operation
			// on the copied rvalue and the second operand,
			// which generates a new rvalue n2
			n2 = mkop(arithop(op), lv->prim, lv2.prim,
				src, n2);
			// Generate an OP_ASSIGN node: sym = n2
			n = mkbinop2(OP_ASSIGN, lv->prim, lv->sym, n, n2);
		}
		// If the lvalue wasn't actually an lvalue, error
		if (!lv->addr)
			error("lvalue expected in assignment", Text);
		// The node we return holds the result of the assignment,
		// so it's no longer an lvalue
		lv->addr = 0;
	}
	return n;
}

/*
 * expr :=
 *	  asgmnt
 *	| asgmnt , expr
 */

static node *exprlist(struct lvalue *lv, int ckvoid) {
	node	*n, *n2 = NULL;
	int	p;

	n = asgmnt(lv);
	p = lv->prim;
	if (COMMA == Token) n = rvalue(n, lv);
	while (COMMA == Token) {
		Token = scan();
		n2 = asgmnt(lv);
		n2 = rvalue(n2, lv);
		p = lv->prim;
		n = mkbinop(OP_COMMA, n, n2);
	}
	if (ckvoid) notvoid(p);
	return n;
}

void expr(struct lvalue *lv, int ckvoid) {
	node	*n;

	Ndtop = 1;
	n = exprlist(lv, ckvoid);
	n = rvalue(n, lv);
	emittree(n);
}

void rexpr(void) {
	struct lvalue lv;

	expr(&lv, 1);
}

int constexpr(void) {
	node	*n;
	struct lvalue lv;

	Ndtop = 1;
	n = binexpr(&lv);
	notvoid(lv.prim);
	n = fold_reduce(n);
	if (NULL == n || OP_LIT != n->op) {
		error("constant expression expected", NULL);
		return 0;
	}
	return n->args[0];
}
