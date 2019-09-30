/*
 *	NMH's Simple C Compiler, 2012-2014
 *	x86-64 target description
 */

#include "defs.h"
#include "data.h"
#include "decl.h"
#include "cgen.h"

		                // There are four available registers to allocate
                                // numbered 1 to 4
#define NUMREGS  4
static char *Reg[NUMREGS+1];    // Names of the registers, filled in later
static char *Breg[NUMREGS+1];   // Names of the byte registers, filled in later
static int R = 0;               // R is the primary register. Not allocated yet
static int SR = 0;              // SR is the secondary register. Not allocated yet
static int S = 0;               // Depth of register stack

static void gens(char *s) {
	if (NULL == Outfile) return;
        fputs(s, Outfile);
}

static void genfs(char *fmt, char *s) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s);
}

static void genfss(char *fmt, char *s1, char *s2) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, s2);
}

static void genfsss(char *fmt, char *s1, char *s2, char *s3) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, s2, s3);
}

static void genfsc(char *fmt, char *s1, char c) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, c);
}

static void genfsd(char *fmt, char *s1, int d) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, d);
}

static void genfsl(char *fmt, char *s1, int l) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, LPREFIX, l);
}

static void genfdl(char *fmt, int d, int l) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, d, LPREFIX, l);
}

static void genfssl(char *fmt, char *s1, char *s2, int l) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, s2, LPREFIX, l);
}

static void genfsds(char *fmt, char *s1, int d, char *s2) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, d, s2);
}

static void genfsls(char *fmt, char *s1, int l, char *s2) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, LPREFIX, l, s2);
}

static void genfsdd(char *fmt, char *s1, int d1, int d2) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, d1, d2);
}

static void genfssd(char *fmt, char *s1, char *s2, int d) {
	if (NULL == Outfile) return;
        fprintf(Outfile, fmt, s1, s2, d);
}

// **********************************
// *** Cyclic Register Allocation ***
// **********************************

// This is an implementation of cyclic register allocation
// including the use of the stack to spill registers.
// The two most important functions are cgallocreg() and cgfreereg().

// Allocate a register by setting R to
// the next available register. Normally,
// just increment R and use that register.
// We can only do this if we haven't
// allocated it before (S==0, R<NUMREGS).

// When R>=NUMREGS, we've run out of registers.
// Spill (push) Reg R onto the stack and record the
// spill by increasing the stack depth in S.
//
// If we have spilled registers on the stack
// (i.e. S>0, we've looped around the circle
// of registers at least once), spill and
// allocate the next register. Record the
// spill by increasing the stack depth in S.
//
// Set the secondary register to the one
// before the one we allocated.
static void cgallocreg(void) {
  if (R >= NUMREGS) {
    R = 1;
    genfs("\tpushq\t%s\n", Reg[R]);
    S++;
  } else if (S) {
    R++;
    genfs("\tpushq\t%s\n", Reg[R]);
    S++;
  } else {
    R++;
  }
  SR = R < 2 ? NUMREGS : R - 1;
}

// Free up a previously allocated register R.
// Normally, just decrement R.
// We can only do this if we haven't
// allocated it before (S==0).
//
// Otherwise, we need to retrieve the old
// register value from the stack. Then we
// can move down to the register below that
// and lower the stack depth.
//
// Set the secondary register to the one
// before the one now allocated.
static void cgfreereg(int dopop) {
  if (S) {
    if (dopop) genfs("\tpopq\t%s\n", Reg[R]);
    S--;
    R--;
    if (R==0) R = NUMREGS;
  } else {
    R--;
  }
  if (R < 0) fatal("cgfreereg set R negative");
  SR = R < 2 ? NUMREGS : R - 1;
}

// Free up all the registers
void cgfreeregs() {
  R = 0; S = 0; SR = 0;
}


// We have to allocate registers when emitting a condition.
// Keep track of what registers were allocated before, so
// we can get back to them after. We need a stack, as
// emitcond() is recursive.
#define MAXCONDREGS 50
static int condR[MAXCONDREGS];
static int condS[MAXCONDREGS];
static int condposn = 0;

void cgstoreregs() {
	if (MAXCONDREGS == condposn)
		fatal("out of condR stack");
	condR[condposn]= R;
	condS[condposn++]= S;
}

void cgrestoreregs(int pop) {
	if (0 == condposn)
		fatal("empty condR stack");
	R= condR[condposn-1];
	S= condS[condposn-1];
  	SR = R < 2 ? NUMREGS : R - 1;
	if (pop) condposn--;
}


// Switch to data/text section
void cgdata(void)	{ gens("\t.data\n"); }
void cgtext(void)	{ gens("\t.text\n"); }

// Generate prelude/postlude
void cgprelude(void)
{
	// SubC can't parse array initialisation, so we do it here
	Reg[1]= "%r8"; Reg[2]= "%r9"; Reg[3]= "%r10"; Reg[4]= "%r11";
	Breg[1]= "%r8b"; Breg[2]= "%r9b"; Breg[3]= "%r10b"; Breg[4]= "%r11b";
}

void cgpostlude(void)	{ }
void cgpublic(char *s)	{ genfs("\t.globl\t%s\n", s); }

void cgsynth(char *op) {
	int	n;
	char	*s;

	n = Q_val;
	s = gsym(Q_name);
	switch (Q_type) {
	case addr_auto:		genfsds("\t%s\t%d(%%rbp),%s\n", "leaq", n,  Reg[SR]);
				genfsss("\t%s\t%s,%s\n",        op, Reg[R], Reg[SR]);
				break;
	case addr_static:	genfsls("\t%s\t$%c%d,%s\n",     op, n, Reg[R]); break;
	case addr_globl:	genfsss("\t%s\t$%s,%s\n",       op, s, Reg[R]); break;
	case addr_label:	genfsls("\t%s\t$%c%d,%s\n",     op, n, Reg[R]); break;
	case literal: 		genfsds("\t%s\t$%d,%s\n",       op, n, Reg[R]); break;
	case auto_word:		genfsds("\t%s\t%d(%%rbp),%s\n", op, n, Reg[R]); break;
	case static_word:	genfsls("\t%s\t%c%d,%s\n",      op, n, Reg[R]); break;
	case globl_word:	genfsss("\t%s\t%s,%s\n",        op, s, Reg[R]); break;
	case auto_byte:
	case static_byte:
	case globl_byte:	cgload2();
				genfsss("\t%s\t%s,%s\n",        op, Reg[SR],Reg[R]);
				break;
	case empty:		genfsss("\t%s\t%s,%s\n",        op, Reg[R], Reg[SR]);
				break;
	default:		fatal("internal: bad type in cgsynth()");
	}
	Q_type = empty;
}

int cgload2(void) {
	int	n, q;
	char	*s, *op, *opb;

	op = "movq";
	opb = "movb";
	n = Q_val;
	s = gsym(Q_name);
	switch (Q_type) {
	case addr_auto:		genfsds("\t%s\t%d(%%rbp),%s\n", "leaq", n, Reg[SR]);
				break;
	case addr_static:	genfsls("\t%s\t$%c%d,%s\n",     op,  n, Reg[SR]); break;
	case addr_globl:	genfsss("\t%s\t$%s,%s\n",       op,  s, Reg[SR]); break;
	case addr_label:	genfsls("\t%s\t$%c%d,%s\n",     op,  n, Reg[SR]); break;
	case literal: 		genfsds("\t%s\t$%d,%s\n",       op,  n, Reg[SR]); break;
	case auto_byte:		cgclear2();
				genfsds("\t%s\t%d(%%rbp),%s\n", opb, n, Breg[SR]);
				break;
	case auto_word:		genfsds("\t%s\t%d(%%rbp),%s\n", op,  n, Reg[SR]); break;
	case static_byte:	cgclear2();
				genfsls("\t%s\t%c%d,%s\n",      opb, n, Breg[SR]); break;
				break;
	case static_word:	genfsls("\t%s\t%c%d,%s\n",      op,  n, Reg[SR]); break;
	case globl_byte:	cgclear2();
				genfsss("\t%s\t%s,%s\n",        opb, s, Breg[SR]); break;
				break;
	case globl_word:	genfsss("\t%s\t%s,%s\n",        op,  s, Reg[SR]); break;
	case empty:		break;
	default:		fatal("internal: bad type in cgload2()");
	}
	q = Q_type;
	Q_type = empty;
	// return empty == q;
	return 0;
}

void cglit(int v)	{ cgallocreg(); genfsds("\t%s\t$%d,%s\n", "movq", v, Reg[R]); }
void cgclear(void)	{ genfss("\txorq\t%s,%s\t\t# cgclear\n", Reg[R], Reg[R]); }
void cgclear2(void)	{ genfss("\txorq\t%s,%s\t\t# cgclear\n", Reg[SR], Reg[SR]); }
void cgldgb(char *s)	{ cgallocreg(); genfss("\txorq\t%s,%s\n", Reg[R], Reg[R]);
			  genfsss("\t%s\t%s,%s\n", "movb", s, Breg[R]); }
void cgldgw(char *s)	{ cgallocreg(); genfsss("\t%s\t%s,%s\n", "movq", s, Reg[R]); }
void cgldlb(int n)	{ cgallocreg(); genfss("\txorq\t%s,%s\n", Reg[R], Reg[R]);
			  genfsds("\t%s\t%d(%%rbp),%s\n", "movb", n, Breg[R]); }
void cgldlw(int n)	{ cgallocreg(); genfsds("\t%s\t%d(%%rbp),%s\n", "movq", n, Reg[R]); }
void cgldsb(int n)	{ cgallocreg(); genfss("\txorq\t%s,%s\n", Reg[R], Reg[R]);
			  genfsls("\t%s\t%c%d,%s\n", "movb", n, Breg[R]); }
void cgldsw(int n)	{ cgallocreg(); genfsls("\t%s\t%c%d,%s\n", "movq", n, Reg[R]); }
void cgldla(int n)	{ cgallocreg(); genfsds("\t%s\t%d(%%rbp),%s\n", "leaq", n, Reg[R]); }
void cgldsa(int n)	{ cgallocreg(); genfsls("\t%s\t$%c%d,%s\n", "movq", n, Reg[R]); }
void cgldga(char *s)	{ cgallocreg(); genfsss("\t%s\t$%s,%s\n", "movq", s, Reg[R]); }
void cgindb(void)	{ genfss("\tmovb\t(%s),%s\n", Reg[R], Breg[R]);
			  genfs("\tand\t$255,%s\n", Reg[R]);
			}
void cgindw(void)	{ genfss("\tmovq\t(%s),%s\n", Reg[R], Reg[R]); }
void cgldlab(int id)	{ cgallocreg(); genfsls("\t%s\t$%c%d,%s\n", "movq", id, Reg[R]); }

void cgpush(void)	{ genfs("\tpushq\t%s\n", Reg[R]); cgfreereg(0); }
void cgpushlit(int n)	{ genfsd("\t%s\t$%d\n", "pushq", n); }
void cgswap(void)	{ genfss("\txchgq\t%s,%s\n", Reg[R], Reg[SR]); }

void cgand(void)	{ cgsynth("andq"); cgfreereg(1); }
void cgior(void)	{ cgsynth("orq");  cgfreereg(1); }
void cgxor(void)	{ cgsynth("xorq");  cgfreereg(1); }
void cgadd(void)	{ genfss("\taddq\t%s,%s\n", Reg[R], Reg[SR]); cgfreereg(1); }
void cgmul(void)	{ genfss("\timulq\t%s,%s\n", Reg[R], Reg[SR]); cgfreereg(1); }
void cgsub(void)	{ genfss("\tsubq\t%s,%s\n", Reg[R], Reg[SR]);  cgfreereg(1); }
void cgdiv(void)	{ genfs("\tmovq\t%s,%%rax\n", Reg[R]);
			  gens("\tcqo\n");
			  genfs("\tidivq\t%s\n", Reg[SR]);
			  genfs("\tmovq\t%%rax,%s\n", Reg[SR]); cgfreereg(1);
			}
void cgmod(void)	{ genfs("\tmovq\t%s,%%rax\n", Reg[R]);
                          gens("\tcqo\n");
                          genfs("\tidivq\t%s\n", Reg[SR]);
			  genfs("\tmovq\t%%rdx,%s\n", Reg[SR]); cgfreereg(1);
			}
void cgshl(void)	{ genfs("\tmovb\t%s,%%cl\n", Breg[R]);
			  genfs("\tshlq\t%%cl,%s\n", Reg[SR]); cgfreereg(1);
			}
void cgshr(void)	{ genfs("\tmovb\t%s,%%cl\n", Breg[R]);
			  genfs("\tsarq\t%%cl,%s\n", Reg[SR]); cgfreereg(1);
			}

void cgcmp(char *inst)	{ int lab;
			  lab = label();
			  gens("\txorq\t%rdx,%rdx\n");
			  if (empty == Q_type) {
				genfss("\tcmpq\t%s,%s\n", Reg[R], Reg[SR]);
			  }
			  else {
				cgsynth("cmpq");
			  }
			  genfsl("\t%s\t%c%d\n", inst, lab);
			  gens("\tincq\t%rdx\n");
			  cgfreeregs(); cgallocreg();
			  genlab(lab);
			  genfs("\tmovq\t%%rdx,%s\n", Reg[R]); }
void cgeq()		{ cgcmp("jne"); }
void cgne()		{ cgcmp("je"); }
void cglt()		{ cgcmp("jge"); }
void cggt()		{ cgcmp("jle"); }
void cgle()		{ cgcmp("jg"); }
void cgge()		{ cgcmp("jl"); }
void cgult()		{ cgcmp("jae"); }
void cgugt()		{ cgcmp("jbe"); }
void cgule()		{ cgcmp("ja"); }
void cguge()		{ cgcmp("jb"); }

void cgbrcond(char *i, int n)	{ int lab;
				  lab = label();
				  if (empty == Q_type) {
					genfss("\tcmpq\t%s,%s\n", Reg[R], Reg[SR]);
				  }
				  else {
					cgsynth("cmpq");
				  }
				  genfsl("\t%s\t%c%d\n", i, lab);
				  genfsl("\t%s\t%c%d\n", "jmp", n);
				  genlab(lab); }
void cgbreq(int n)		{ cgbrcond("je", n); }
void cgbrne(int n)		{ cgbrcond("jne", n); }
void cgbrlt(int n)		{ cgbrcond("jl", n); }
void cgbrgt(int n)		{ cgbrcond("jg", n); }
void cgbrle(int n)		{ cgbrcond("jle", n); }
void cgbrge(int n)		{ cgbrcond("jge", n); }
void cgbrult(int n)		{ cgbrcond("jb", n); }
void cgbrugt(int n)		{ cgbrcond("ja", n); }
void cgbrule(int n)		{ cgbrcond("jbe", n); }
void cgbruge(int n)		{ cgbrcond("jae", n); }

void cgneg(void)	{ genfs("\tnegq\t%s\n", Reg[R]); }
void cgnot(void)	{ genfs("\tnotq\t%s\n", Reg[R]); }
void cglognot(void)	{ genfs("\tnegq\t%s\n", Reg[R]);
			  genfss("\tsbbq\t%s,%s\n", Reg[R], Reg[R]);
			  genfs("\tincq\t%s\n", Reg[R]); }
void cgscale(void)	{ genfs("\tshlq\t$3,%s\n", Reg[R]); }
void cgscale2(void)	{ genfs("\tshlq\t$3,%s\n", Reg[SR]); }
void cgunscale(void)	{ genfs("\tshrq\t$3,%s\n", Reg[R]); }
void cgscaleby(int v)	{ genfs("\tmovq\t%s,%%rax\n", Reg[R]);
			  genfsds("\t%s\t$%d,%s\n", "movq", v, "%rcx");
			  gens("\tmulq\t%rcx\n");
			  genfs("\tmovq\t%%rax,%s\n", Reg[R]);
			}
void cgscale2by(int v)	{ genfs("\tmovq\t%s,%%rax\n", Reg[SR]);
			  genfsds("\t%s\t$%d,%s\n", "movq", v, "%rcx");
			  gens("\tmulq\t%rcx\n");
			  genfs("\tmovq\t%%rax,%s\n", Reg[SR]);
			}
void cgunscaleby(int v)	{ genfs("\tmovq\t%s,%%rax\n", Reg[R]);
                          gens("\tcqo\n");
			  genfsds("\t%s\t$%d,%s\n", "movq", v, "%rcx");
			  gens("\tidivq\t%rcx\n");
			  genfs("\tmovq\t%%rax,%s\n", Reg[R]);
			}
void cgbool(void)	{ genfs("\tnegq\t%s\n", Reg[R]);
			  genfss("\tsbbq\t%s,%s\n", Reg[R], Reg[R]);
			  genfs("\tnegq\t%s\n", Reg[R]); }

void cgldinc(void)	{ cgallocreg(); genfss("\tmovq\t%s,%s\n", Reg[SR], Reg[R]); }
void cgunldinc(void)	{ cgfreereg(1); }
void cginc1pi(int v)	{ genfsds("\t%s\t$%d,(%s)\n", "addq", v, Reg[R]); }
void cgdec1pi(int v)	{ genfsds("\t%s\t$%d,(%s)\n", "subq", v, Reg[R]); }
void cginc2pi(int v)	{ genfsds("\t%s\t$%d,(%s)\n", "addq", v, Reg[SR]); }
void cgdec2pi(int v)	{ genfsds("\t%s\t$%d,(%s)\n", "subq", v, Reg[SR]); }
void cgincpl(int a, int v)	{ genfsdd("\t%s\t$%d,%d(%%rbp)\n", "addq", v, a); }
void cgdecpl(int a, int v)	{ genfsdd("\t%s\t$%d,%d(%%rbp)\n", "subq", v, a); }
void cgincps(int a, int v)	{ genfdl("\taddq\t$%d,%c%d\n", v, a); }
void cgdecps(int a, int v)	{ genfdl("\tsubq\t$%d,%c%d\n", v, a); }
void cgincpg(char *s, int v)	{ genfsds("\t%s\t$%d,%s\n", "addq", v, s); }
void cgdecpg(char *s, int v)	{ genfsds("\t%s\t$%d,%s\n", "subq", v, s); }
void cginc1iw(void)	{ genfss("\t%s\t(%s)\n", "incq", Reg[R]); }
void cgdec1iw(void)	{ genfss("\t%s\t(%s)\n", "decq", Reg[R]); }
void cginc2iw(void)	{ genfs("\tincq\t(%s)\n", Reg[SR]); }
void cgdec2iw(void)	{ genfs("\tdecq\t(%s)\n", Reg[SR]); }
void cginclw(int a)	{ genfsd("\t%s\t%d(%%rbp)\n", "incq", a); }
void cgdeclw(int a)	{ genfsd("\t%s\t%d(%%rbp)\n", "decq", a); }
void cgincsw(int a)	{ genfsl("\t%s\t%c%d\n", "incq", a); }
void cgdecsw(int a)	{ genfsl("\t%s\t%c%d\n", "decq", a); }
void cgincgw(char *s)	{ genfss("\t%s\t%s\n", "incq", s); }
void cgdecgw(char *s)	{ genfss("\t%s\t%s\n", "decq", s); }
void cginc1ib(void)	{ genfss("\t%s\t(%s)\n", "incb", Reg[R]); }
void cgdec1ib(void)	{ genfss("\t%s\t(%s)\n", "decb", Reg[R]); }
void cginc2ib(void)	{ genfs("\tincb\t(%s)\n", Reg[SR]); }
void cgdec2ib(void)	{ genfs("\tdecb\t(%s)\n", Reg[SR]); }
void cginclb(int a)	{ genfsd("\t%s\t%d(%%rbp)\n", "incb", a); }
void cgdeclb(int a)	{ genfsd("\t%s\t%d(%%rbp)\n", "decb", a); }
void cgincsb(int a)	{ genfsl("\t%s\t%c%d\n", "incb", a); }
void cgdecsb(int a)	{ genfsl("\t%s\t%c%d\n", "decb", a); }
void cgincgb(char *s)	{ genfss("\t%s\t%s\n", "incb", s); }
void cgdecgb(char *s)	{ genfss("\t%s\t%s\n", "decb", s); }

void cgbr(char *how, int n)	{ int lab;
				  lab = label();
				  genfss("\torq\t%s,%s\n", Reg[R], Reg[R]);
				  genfsl("\t%s\t%c%d\n", how, lab);
				  genfsl("\t%s\t%c%d\n", "jmp", n);
				  genlab(lab); }
void cgbrtrue(int n)	{ cgbr("jz", n); }
void cgbrfalse(int n)	{ cgbr("jnz", n); }
void cgjump(int n)	{ genfsl("\t%s\t%c%d\n", "jmp", n); }
void cgldswtchval(void) { genfs("\tmovq\t%s,%%rax\n", Reg[R]); }
void cgldswtch(int n)	{ cgallocreg(); genfsl("\t%s\t$%c%d,%%rdx\n", "movq", n); }
void cgcalswtch(void)	{ gens("\tjmp\tswitch\n"); }
void cgcase(int v, int l)	{ genfdl("\t.quad\t%d,%c%d\n", v, l); }

void cgstorib(void)	{ genfss("\tmovb\t%s,(%s)\n", Breg[R], Reg[SR]); }
void cgstoriw(void)	{ genfss("\tmovq\t%s,(%s)\n", Reg[R], Reg[SR]); }
void cgstorlb(int n)	{ genfsd("\tmovb\t%s,%d(%%rbp)\n", Breg[R], n); }
void cgstorlw(int n)	{ genfsd("\tmovq\t%s,%d(%%rbp)\n", Reg[R], n); }
void cgstorsb(int n)	{ genfssl("\t%s\t%s,%c%d\n", "movb", Breg[R], n); }
void cgstorsw(int n)	{ genfssl("\t%s\t%s,%c%d\n", "movq", Reg[R], n); }
void cgstorgb(char *s)	{ genfsss("\t%s\t%s,%s\n", "movb", Breg[R], s); }
void cgstorgw(char *s)	{ genfsss("\t%s\t%s,%s\n", "movq", Reg[R], s); }

void cginitlw(int v, int a)	{ genfsdd("\t%s\t$%d,%d(%%rbp)\n", "movq", v, a); }

// Cached values of S and R. Holds their values before
// a function call, so we know which regs to pull back
// after the function call.
#define MAXSPILLREGS 50
static int spillR[MAXSPILLREGS];
static int spillS[MAXSPILLREGS];
static int spillposn = 0;

// Spill any currently allocated registers
// on the stack before a function call.
void cgspillregs()
{
	int i;

	if (MAXSPILLREGS == spillposn)
		fatal("out of spillR stack");

	// Cache the S and R values
	spillR[spillposn] = R; spillS[spillposn++] = S;

  	// Do all when we've done a cycle
	if (S) {
		for (i= 1; i <= NUMREGS; i++) 
			genfs("\tpushq\t%s\n", Reg[i]);
  	} else {
	// Do all up to R
		for (i= 1; i <= R; i++) 
			genfs("\tpushq\t%s\n", Reg[i]);
  	}
}

// On the return, do it all backwards.
void cgunspillregs()
{
	int i;

	if (0 == spillposn)
		fatal("empty spillR stack");
	spillposn--;

  	// Do all when we've done a cycle
	if (spillS[spillposn]) {
		for (i= NUMREGS; i >= 1; i--) 
			genfs("\tpopq\t%s\n", Reg[i]);
  	} else {
	// Do all up to spillR
		for (i= spillR[spillposn]; i >= 1; i--) 
			genfs("\tpopq\t%s\n", Reg[i]);
  	}
}

void cgcall(char *s)	{ cgallocreg(); genfss("\t%s\t%s\n", "call", s);
			  genfs("\tmovq\t%%rax,%s\n", Reg[R]);
			}
void cgcalr(void)	{ cgallocreg(); genfs("\tcall\t*%s\n", Reg[R]);
			  genfs("\tmovq\t%%rax,%s\n", Reg[R]);
			}
void cgstack(int n)	{ genfsd("\t%s\t$%d,%%rsp\n", "addq", n); }
void cgentry(void)	{ gens("\tpushq\t%rbp\n");
			  gens("\tmovq\t%rsp,%rbp\n"); }
void cgexit(void)	{ gens("\tpopq\t%rbp\n");
			  gens("\tret\n"); }
void cgretvalue(void)	{ genfs("\tmovq\t%s,%%rax\n", Reg[R]); }
void cgdefb(int v)	{ genfsd("\t%s\t%d\n", ".byte", v); }
void cgdefw(int v)	{ genfsd("\t%s\t%d\n", ".quad", v); }
void cgdefp(int v)	{ genfsd("\t%s\t%d\n", ".quad", v); }
void cgdefl(int v)	{ genfsl("\t%s\t%c%d\n", ".quad", v); }
void cgdefc(int c)	{ genfsc("\t%s\t'%c'\n", ".byte", c); }
void cggbss(char *s, int z)	{ genfsd("\t.comm\t%s,%d\n", s, z); }
void cglbss(char *s, int z)	{ genfsd("\t.lcomm\t%s,%d\n", s, z); }
void cgalign(void)	{ /* unused */ }

// Copy the primary register to the secondary one,
// then free the primary register. We need this to do:
// xx.a= xx.b= xx.c= value; so that the value bubbles
// down through the registers. Only do this when we
// have a secondary register.
void cgregtosec(void)	{
			  if ((R < 2) && (S == 0)) return;
			  cgswap(); cgfreereg(1);
			}
