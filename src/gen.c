/*
 *	NMH's Simple C Compiler, 2011--2016
 *	Code generator (emitter)
 */

#include "defs.h"
#include "data.h"
#include "decl.h"
#include "cgen.h"

int	Acc = 0;

// Mark the accumulator as unoccupied.
// Mark the instruction queues as
// empty if q is true.
void clear(int q) {
	Acc = 0;
	if (q) {
		Q_type = empty;
		Q_cmp = cnone;
		Q_bool = bnone;
	}
}

// Mark the accumulator as occupied.
void load(void) {
	Acc = 1;
}

// Generate and return a new label number
int label(void) {
	static int id = 1;

	return id++;
}

// Spil the register by pushing it
// on the stack
void spill(void) {
	if (Acc) {
		gentext();
		cgpush();
	}
}

// Generate an assembly line as-is
void genraw(char *s) {
	if (NULL == Outfile) return;
	fprintf(Outfile, "%s", s);
}

// Generate an assembly line with
// a leading tab and a newline
void gen(char *s) {
	if (NULL == Outfile) return;
	fprintf(Outfile, "\t%s\n", s);
}

// Generate assembly with a format string,
// an instruction and a number
void ngen(char *s, char *inst, int n) {
	if (NULL == Outfile) return;
	fputc('\t', Outfile);
	fprintf(Outfile, s, inst, n);
	fputc('\n', Outfile);
}

// Generate assembly with a format string,
// an instruction and two numbers
void ngen2(char *s, char *inst, int n, int a) {
	if (NULL == Outfile) return;
	fputc('\t', Outfile);
	fprintf(Outfile, s, inst, n, a);
	fputc('\n', Outfile);
}

// Generate an instruction with a label argument
void lgen(char *s, char *inst, int n) {
	if (NULL == Outfile) return;
	fputc('\t', Outfile);
	fprintf(Outfile, s, inst, LPREFIX, n);
	fputc('\n', Outfile);
}

// Generate an instruction with two arguments,
// one being a constant and the other a label
void lgen2(char *s, int v1, int v2) {
	if (NULL == Outfile) return;
	fputc('\t', Outfile);
	fprintf(Outfile, s, v1, LPREFIX, v2);
	fputc('\n', Outfile);
}

// Generate an instruction with a textual
// argument, e.g. an identifier
void sgen(char *s, char *inst, char *s2) {
	if (NULL == Outfile) return;
	fputc('\t', Outfile);
	fprintf(Outfile, s, inst, s2);
	fputc('\n', Outfile);
}

// Generate an instruction with two textual
// arguments, e.g. identifiers
void sgen2(char *s, char *inst, int v, char *s2) {
	if (NULL == Outfile) return;
	fputc('\t', Outfile);
	fprintf(Outfile, s, inst, v, s2);
	fputc('\n', Outfile);
}

// Generate a label in assembly
void genlab(int id) {
	if (NULL == Outfile) return;
	fprintf(Outfile, "%c%d:\n", LPREFIX, id);
}

// Return a label's name as a string
char *labname(int id) {
	static char	name[100];

	sprintf(name, "%c%d", LPREFIX, id);
	return name;
}

// Return a SubC-specific symbol name by prefixing
// it with PREFIX, to separate SubC's namespace
// from the host system
char *gsym(char *s) {
	static char	name[NAMELEN+2];

	name[0] = PREFIX;
	copyname(&name[1], s);
	return name;
}

/* administrativa */

// Switch to the data segment
// in the assembly output
void gendata(void) {
	if (Textseg) cgdata();
	Textseg = 0;
}

// Switch to the data segment
// in the assembly output
void gentext(void) {
	if (!Textseg) cgtext();
	Textseg = 1;
}

// Generate the assembly prelude
void genprelude(void) {
	cgprelude();
	Textseg = 0;
	gentext();
}

// Generate the assembly postlude
void genpostlude(void) {
	cgpostlude();
}

// Generate a label for a symbol
void genname(char *name) {
	genraw(gsym(name));
	genraw(":");
}

// Generate a label for a public symbol
void genpublic(char *name) {
	cgpublic(gsym(name));
}

/* loading values */

// Commit the instruction in the queue
// and generate an instruction to load
// the accumulator with a value
void commit(void) {
	// Generate any outstanding
	// comparisons or boolean
	// operations
	if (Q_cmp != cnone) {
		commit_cmp();
		return;
	}
	if (Q_bool != bnone) {
		commit_bool();
		return;
	}
	if (empty == Q_type) return;

	// Spill the accumulator so we
	// can load a new value into it.
	// The case values should be obvious.
	// XXX: why cgclear() on byte loads?
	spill();
	switch (Q_type) {
	case addr_auto:		cgldla(Q_val); break;
	case addr_static:	cgldsa(Q_val); break;
	case addr_globl:	cgldga(gsym(Q_name)); break;
	case addr_label:	cgldlab(Q_val); break;
	case literal:		cglit(Q_val); break;
	case auto_byte:		cgclear(); cgldlb(Q_val); break;
	case auto_word:		cgldlw(Q_val); break;
	case static_byte:	cgclear(); cgldsb(Q_val); break;
	case static_word:	cgldsw(Q_val); break;
	case globl_byte:	cgclear(); cgldgb(gsym(Q_name)); break;
	case globl_word:	cgldgw(gsym(Q_name)); break;
	default:		fatal("internal: unknown Q_type");
	}

	// Mark the accumulator as occupied
	// and the queue as empty
	load();
	Q_type = empty;
}

// Queue a load instruction of given type, which
// either loads the value or the symbol name
void queue(int type, int val, char *name) {
	commit();
	Q_type = type;
	Q_val = val;
	if (name) copyname(Q_name, name);
}

void genaddr(int y) {
	gentext();
	if (CAUTO == Syms[y].stcls)
		queue(addr_auto, Syms[y].val, NULL);
	else if (CLSTATC == Syms[y].stcls)
		queue(addr_static, Syms[y].val, NULL);
	else
		queue(addr_globl, 0, Syms[y].name);
}

void genldlab(int id) {
	gentext();
	queue(addr_label, id, NULL);
}

void genlit(int v) {
	gentext();
	queue(literal, v, NULL);
}

/* binary ops */

void genand(void) {
	gentext();
	cgand();
}

void genior(void) {
	gentext();
	cgior();
}

void genxor(void) {
	gentext();
	cgxor();
}

void genshl(int swapped) {
	gentext();
	if (cgload2() || !swapped) cgswap();
	cgshl();
}

void genshr(int swapped) {
	gentext();
	if (cgload2() || !swapped) cgswap();
	cgshr();
}

static int ptr(int p) {
	int	sp;

	sp = p & STCMASK;
	return INTPTR == p || INTPP == p ||
		CHARPTR == p || CHARPP == p ||
		VOIDPTR == p || VOIDPP == p ||
		STCPTR == sp || STCPP == sp ||
		UNIPTR == sp || UNIPP == sp ||
		FUNPTR == p;
}

static int needscale(int p) {
	int	sp;

	sp = p & STCMASK;
	return INTPTR == p || INTPP == p || CHARPP == p || VOIDPP == p ||
		STCPTR == sp || STCPP == sp || UNIPTR == sp || UNIPP == sp;
}

int genadd(int p1, int p2, int swapped) {
	int	rp = PINT, t;

	gentext();
	if (cgload2() || !swapped) {
		t = p1;
		p1 = p2;
		p2 = t;
	}
	if (ptr(p1)) {
		if (needscale(p1)) {
			if (	(p1 & STCMASK) == STCPTR ||
				(p1 & STCMASK) == UNIPTR
			)
				cgscale2by(objsize(deref(p1), TVARIABLE, 1));
			else
				cgscale2();
		}
		rp = p1;
	}
	else if (ptr(p2)) {
		if (needscale(p2)) {
			if (	(p2 & STCMASK) == STCPTR ||
				(p2 & STCMASK) == UNIPTR
			)
				cgscaleby(objsize(deref(p2), TVARIABLE, 1));
			else
				cgscale();
		}
		rp = p2;
	}
	cgadd();
	return rp;
}

int gensub(int p1, int p2, int swapped) {
	int	rp = PINT;

	gentext();
	if (cgload2() || !swapped) cgswap();
	if (!inttype(p1) && !inttype(p2) && p1 != p2)
		error("incompatible pointer types in binary '-'", NULL);
	if (ptr(p1) && !ptr(p2)) {
		if (needscale(p1)) {
			if (	(p1 & STCMASK) == STCPTR ||
				(p1 & STCMASK) == UNIPTR
			)
				cgscale2by(objsize(deref(p1), TVARIABLE, 1));
			else
				cgscale2();
		}
		rp = p1;
	}
	cgsub();
	if (needscale(p1) && needscale(p2)) {
		if (	(p1 & STCMASK) == STCPTR ||
			(p1 & STCMASK) == UNIPTR
		)
			cgunscaleby(objsize(deref(p1), TVARIABLE, 1));
		else
			cgunscale();
	}
	return rp;
}

void genmul(void) {
	gentext();
	cgload2();
	cgmul();
}

void gendiv(int swapped) {
	gentext();
	if (cgload2() || !swapped) cgswap();
	cgdiv();
}

void genmod(int swapped) {
	gentext();
	if (cgload2() || !swapped) cgswap();
	cgmod();
}

// Check that a binary operation can be
// performed on primitive types p1 and p2.
// Report an error if it can't be performed.
static void binopchk(int op, int p1, int p2) {

	// Rationalise ASPLUS and ASMINUS ops
	if (ASPLUS == op)
		op = PLUS;
	else if (ASMINUS == op)
		op = MINUS;

	// Both are integers, OK
	if (inttype(p1) && inttype(p2))
		return;

	// Error if either are void or composite types
	else if (comptype(p1) || comptype(p2))
		/* fail */;
	else if (PVOID == p1 || PVOID == p2)
		/* fail */;

	// OK if PLUS or MINUS and one integer type
	else if (PLUS == op && (inttype(p1) || inttype(p2)))
		return;
	else if (MINUS == op && (!inttype(p1) || inttype(p2)))
		return;

	// Comparison operators are tricky. It's OK if both
	// types are the same, or one is a void pointer and
	// the other is (by elimination?) also a pointer
	else if ((EQUAL == op || NOTEQ == op || LESS == op ||
		 GREATER == op || LTEQ == op || GTEQ == op)
		&&
		(p1 == p2 ||
		 (VOIDPTR == p1 && !inttype(p2)) ||
		 (VOIDPTR == p2 && !inttype(p1)))
	)
		return;
	error("invalid operands to binary operator", NULL);
}

// Generate a queued comparison instruction
void commit_cmp(void) {
	switch (Q_cmp) {
	case equal:		cgeq(); break;
	case not_equal:		cgne(); break;
	case less:		cglt(); break;
	case greater:		cggt(); break;
	case less_equal:	cgle(); break;
	case greater_equal:	cgge(); break;
	case below:		cgult(); break;
	case above:		cgugt(); break;
	case below_equal:	cgule(); break;
	case above_equal:	cguge(); break;
	}
	Q_cmp = cnone;
}

// Queue a comparison instruction
void queue_cmp(int op) {
	commit();
	Q_cmp = op;
}

// Determine the type of an expression with
// a binary operator.
int binoptype(int op, int p1, int p2) {

	// Check that the operation is permitted
	binopchk(op, p1, p2);

	// PLUS is commutative, so return any
	// non-integer type
	if (PLUS == op) {
		if (!inttype(p1)) return p1;
		if (!inttype(p2)) return p2;
	}

	// MINUS is not commutative, so favour
	// p1's type if p2 is integer, otherwise int
	else if (MINUS == op) {
		if (!inttype(p1)) {
			if (!inttype(p2)) return PINT;
			return p1;
		}
	}

	// Return int if none of the above matched
	return PINT;
}

/* unary ops */

// Commit a queued boolean operation
void commit_bool(void) {
	switch (Q_bool) {
	case lognot:	cglognot(); break;
	case normalize:	cgbool(); break;
	}
	Q_bool = bnone;
}

// Queue a boolean operation
void queue_bool(int op) {
	commit();
	Q_bool = op;
}

// Generate a boolean operation
// by queueing it
void genbool(void) {
	queue_bool(normalize);
}

void genlognot(void) {
	queue_bool(lognot);
}

void genind(int p) {
	gentext();
	commit();
	if (PCHAR == p)
		cgindb();
	else
		cgindw();
}

void genneg(void) {
	gentext();
	commit();
	cgneg();
}

void gennot(void) {
	gentext();
	commit();
	cgnot();
}

void genscale(void) {
	gentext();
	commit();
	cgscale();
}

void genscale2(void) {
	gentext();
	commit();
	cgscale2();
}

void genscaleby(int v) {
	gentext();
	commit();
	cgscaleby(v);
}

/* jump/call/function ops */

void genjump(int dest) {
	gentext();
	commit();
	cgjump(dest);
}

// Generate a branch instruction.
// Generate the opposite one when
// inv is true
void genbranch(int dest, int inv) {
	if (inv) {
		switch (Q_cmp) {
		case equal:		cgbrne(dest); break;
		case not_equal:		cgbreq(dest); break;
		case less:		cgbrge(dest); break;
		case greater:		cgbrle(dest); break;
		case less_equal:	cgbrgt(dest); break;
		case greater_equal:	cgbrlt(dest); break;
		case below:		cgbruge(dest); break;
		case above:		cgbrule(dest); break;
		case below_equal:	cgbrugt(dest); break;
		case above_equal:	cgbrult(dest); break;
		}
	}
	else {
		switch (Q_cmp) {
		case equal:		cgbreq(dest); break;
		case not_equal:		cgbrne(dest); break;
		case less:		cgbrlt(dest); break;
		case greater:		cgbrgt(dest); break;
		case less_equal:	cgbrle(dest); break;
		case greater_equal:	cgbrge(dest); break;
		case below:		cgbrult(dest); break;
		case above:		cgbrugt(dest); break;
		case below_equal:	cgbrule(dest); break;
		case above_equal:	cgbruge(dest); break;
		}
	}
	Q_cmp = cnone;
}

void genlogbr(int dest, int inv) {
	if (normalize == Q_bool) {
		if (inv)
			cgbrfalse(dest);
		else
			cgbrtrue(dest);
	}
	else if (lognot == Q_bool) {
		if (inv)
			cgbrtrue(dest);
		else
			cgbrfalse(dest);
	}
	Q_bool = bnone;
}

void genbrfalse(int dest) {
	gentext();
	if (Q_cmp != cnone) {
		genbranch(dest, 0);
		return;
	}
	if (Q_bool != bnone) {
		genlogbr(dest, 1);
		return;
	}
	commit();
	cgbrfalse(dest);
}

void genbrtrue(int dest) {
	gentext();
	if (Q_cmp != cnone) {
		genbranch(dest, 1);
		return;
	}
	if (Q_bool != bnone) {
		genlogbr(dest, 0);
		return;
	}
	commit();
	cgbrtrue(dest);
}

void gencall(int y) {
	gentext();
	commit();
	cgcall(gsym(Syms[y].name));
	load();
}

void gencalr(void) {
	gentext();
	commit();
	cgcalr();
	load();
}

void genentry(void) {
	gentext();
	cgentry();
}

void genexit(void) {
	gentext();
	cgexit();
}

void genpush(void) {
	gentext();
	commit();
	cgpush();
}

void genpushlit(int n) {
	gentext();
	commit();
	spill();
	cgpushlit(n);
}

void genstack(int n) {
	if (n) {
		gentext();
		cgstack(n);
	}
}

void genlocinit(void) {
	int	i;

	gentext();
	for (i=0; i<Nli; i++)
		cginitlw(LIval[i], LIaddr[i]);
}

/* data definitions */

void genbss(char *name, int len, int statc) {
	gendata();
	if (statc)
		cglbss(name, (len + INTSIZE-1) / INTSIZE * INTSIZE);
	else
		cggbss(name, (len + INTSIZE-1) / INTSIZE * INTSIZE);
}

void genalign(int k) {
	gendata();
	while (k++ % INTSIZE)
		cgdefb(0);
}

void genaligntext() {
	cgalign();
}

void gendefb(int v) {
	gendata();
	cgdefb(v);
}

void gendefp(int v) {
	gendata();
	cgdefp(v);
}

void gendefs(char *s, int len) {
	int	i;

	gendata();
	for (i=1; i<len-1; i++) {
		if (isalnum(s[i]))
			cgdefc(s[i]);
		else
			cgdefb(s[i]);
	}
}

void gendefw(int v) {
	gendata();
	cgdefw(v);
}

/* increment ops */

static void genincptr(struct lvalue *lv, int inc, int pre) {
	int	y, size;

	size = objsize(deref(lv->prim), TVARIABLE, 1);
	gentext();
	y = lv->sym;
	commit();
	if (!y && !pre) cgldinc();
	if (!pre) {
		genrval(lv);
		commit();
	}
	if (!y) {
		if (pre)
			if (inc)
				cginc1pi(size);
			else
				cgdec1pi(size);
		else
			if (inc)
				cginc2pi(size);
			else
				cgdec2pi(size);
	}
	else if (CAUTO == Syms[y].stcls) {
		if (inc)
			cgincpl(Syms[y].val, size);
		else
			cgdecpl(Syms[y].val, size);
	}
	else if (CLSTATC == Syms[y].stcls) {
		if (inc)
			cgincps(Syms[y].val, size);
		else
			cgdecps(Syms[y].val, size);
	}
	else {
		if (inc)
			cgincpg(gsym(Syms[y].name), size);
		else
			cgdecpg(gsym(Syms[y].name), size);
	}
	if (pre) genrval(lv);
}

void geninc(struct lvalue *lv, int inc, int pre) {
	int	y, b;

	gentext();
	y = lv->sym;
	if (needscale(lv->prim)) {
		genincptr(lv, inc, pre);
		return;
	}
	b = PCHAR == lv->prim;
	/* will duplicate move to aux register in (*char)++ */
	commit();
	if (!y && !pre) cgldinc();
	if (!pre) {
		genrval(lv);
		commit();
	}
	if (!y) {
		if (pre)
			if (inc)
				b? cginc1ib(): cginc1iw();
			else
				b? cgdec1ib(): cgdec1iw();
		else
			if (inc)
				b? cginc2ib(): cginc2iw();
			else
				b? cgdec2ib(): cgdec2iw();
	}
	else if (CAUTO == Syms[y].stcls) {
		if (inc)
			b? cginclb(Syms[y].val): cginclw(Syms[y].val);
		else
			b? cgdeclb(Syms[y].val): cgdeclw(Syms[y].val);
	}
	else if (CLSTATC == Syms[y].stcls) {
		if (inc)
			b? cgincsb(Syms[y].val): cgincsw(Syms[y].val);
		else
			b? cgdecsb(Syms[y].val): cgdecsw(Syms[y].val);
	}
	else {
		if (inc)
			b? cgincgb(gsym(Syms[y].name)):
			   cgincgw(gsym(Syms[y].name));
		else
			b? cgdecgb(gsym(Syms[y].name)):
			   cgdecgw(gsym(Syms[y].name));
	}
	if (pre) genrval(lv);
}

/* switch table generator */

void genswitch(int *vals, int *labs, int nc, int dflt) {
	int	i, ltbl;

	ltbl = label();
	gentext();
	cgldswtch(ltbl);
	cgcalswtch();
	genlab(ltbl);
	cgdefw(nc);
	for (i = 0; i < nc; i++)
		cgcase(vals[i], labs[i]);
	cgdefl(dflt);
}

/* assigments */

void genstore(struct lvalue *lv) {
	if (NULL == lv) return;
	gentext();
	if (!lv->sym) {
		cgpopptr();
		if (PCHAR == lv->prim)
			cgstorib();
		else
			cgstoriw();

	}
	else if (CAUTO == Syms[lv->sym].stcls) {
		if (PCHAR == lv->prim)
			cgstorlb(Syms[lv->sym].val);
		else
			cgstorlw(Syms[lv->sym].val);
	}
	else if (CLSTATC == Syms[lv->sym].stcls) {
		if (PCHAR == lv->prim)
			cgstorsb(Syms[lv->sym].val);
		else
			cgstorsw(Syms[lv->sym].val);
	}
	else {
		if (PCHAR == lv->prim)
			cgstorgb(gsym(Syms[lv->sym].name));
		else
			cgstorgw(gsym(Syms[lv->sym].name));
	}
}

/* genrval computation */

void genrval(struct lvalue *lv) {
	if (NULL == lv) return;
	gentext();
	if (!lv->sym) {
		genind(lv->prim);
	}
	else if (CAUTO == Syms[lv->sym].stcls) {
		if (PCHAR == lv->prim)
			queue(auto_byte, Syms[lv->sym].val, NULL);
		else
			queue(auto_word, Syms[lv->sym].val, NULL);
	}
	else if (CLSTATC == Syms[lv->sym].stcls) {
		if (PCHAR == lv->prim)
			queue(static_byte, Syms[lv->sym].val, NULL);
		else
			queue(static_word, Syms[lv->sym].val, NULL);
	}
	else {
		if (PCHAR == lv->prim)
			queue(globl_byte, 0, Syms[lv->sym].name);
		else
			queue(globl_word, 0, Syms[lv->sym].name);
	}
}
