# Cyclic Register Allocation

On pages 310-313 of Nils' book
[Practical Compiler Construction](http://www.t3x.org/reload/index.html),
he discusses cyclic register allocation. Rather than having a single
primary register (an *accumulator*) with a secondary register, the compiler
is modified to use a number of registers as are available on the underlying
hardware.

I thought this would be a relatively easy addition to make to the compiler,
but it actually turned out to be tricky. Let's have a look at the idea,
and why its implementation was non-trivial.

## Cyclic Register Allocation: The Idea

Assume that there are four available registers in the target CPU.
As a register is required, allocate a new register from the set, i.e.
R1, then R2, then R3, then R4. Once all four registers are allocated
and we need a new register, push R1 on the stack (i.e. *spill* it).
This frees R1 up to be re-allocated. As we continue, the registers are
allocated (and possibly spilled) in a cycle.

The idea of a primary accumulator and a secondary register is modified
to be that of the two most recently allocated registers. If R3 was the
most recently allocated register, then R3 is the primary register and
R2 is the secondary register.

Once a register is no longer needed, it is freed. Any previous value for
it that was spilled can now be unspilled from the stack.

## The Implementation: Register Allocation

As the changes to the SubC code are non-trivial, I've made a new folder
called `Cyclic` with th modified SubC code in there. We'll start with
a look at the functions to allocate a register and deallocate it:

```
#define NUMREGS  4
static int R = 0;               // R is the primary register. Not allocated yet
static int SR = 0;              // SR is the secondary register. Not allocated yet
static int S = 0;               // Depth of register stack

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
```

Initially, R is zero which indicates no allocated registers. For the first four
calls, R becomes 1, 2, 3 and 4 with S (the stack depth) remaining zero. On the
fifth call, R is equal to 4. We reset R to 1,increment S and spill the old R1
value onto the stack.

S represents the number of register values spilled on the stack. From now on, if
S is not zero, we must spill an existing register value on the stack when we
increment R.

The last line of code determines the number of the secondary register, SR.

So, R cycles in the range 1 to 4. So does SR, but one behind R. Finally, S
holds the number of register values spilled on the stack.

```
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
```

The code to free a register essentially undoes the actions in `cgallocreg()`.
Note that there is an argument, `dopop`, to determine if we really do need
to pop the old register value from the stack. I'll explain why this is
needed later.

The algorithm for `cgfreereg()` is slightly different to the code on page 312
of Nils' book, as Nils' code has a small bug in it.

Finally, there is a function to release all registers. This is called at the
end of each expression.

```
// Free up all the registers
void cgfreeregs() {
  R = 0; S = 0; SR = 0;
}
```

Technically, I should also pop S registers from the stack.

I've chose to implement cyclic register allocation for the x86-64 hardware platform.
Several registers are used for specific tasks, e.g. division and shifting, so
I've chosen four registers only for the cycle:

```
static char *Reg[NUMREGS+1];    // Names of the registers, filled in later
static char *Breg[NUMREGS+1];   // Names of the byte registers, filled in later

// Generate prelude/postlude
void cgprelude(void)
{
        // SubC can't parse array initialisation, so we do it here
        Reg[1]=  "%r8";  Reg[2]=  "%r9";  Reg[3]=  "%r10";  Reg[4]= "%r11";
        Breg[1]= "%r8b"; Breg[2]= "%r9b"; Breg[3]= "%r10b"; Breg[4]= "%r11b";
}
```

## The Implementation: Function Return Values

Let's start with some of the more simpler changes. In the original SubC, a function
returns a value in the `%rax` register which is always the primary register. In the
new compiler, I've kept the calling convention, but now `%rax` needs to be copied
into the correct primary register after a function return.

The AST is modified to have a new operator, OP_RETURN. This indicates that the single
child tree is the return value from a function, and the rvalue will need to be
copied to the primary register. I modified `expr()` as follows:

```
// If markreturn, adjust the
// tree to have OP_RETURN at the root.
void expr(struct lvalue *lv, int ckvoid, int markreturn) {
        node    *n;

        Ndtop = 1;
        n = exprlist(lv, ckvoid);
        n = rvalue(n, lv);
        if (markreturn)
                n = mkunop(OP_RETURN, n);
        emittree(n);
}
```

The `return_stmt()` function, which parses *return* statements, is modified to turn on
the `markreturn` argument. Elsewhere in the compiler, all other calls to `expr()` set
this argument to zero.

```
static void return_stmt(void) {
        struct lvalue lv;

        Token = scan();                 // Skip the 'return' token
        if (Token != SEMI) {            // If there's an expression
                expr(&lv, 1, 1);        // Parse it and check its type matches
                                        // this function
                if (!typematch(lv.prim, Syms[Thisfn].prim))
                        error("incompatible type in 'return'", NULL);
        }
	...
}
```

In the large switch statement in `emittree1()`, we add another case:

```
        case OP_RETURN: emittree1(a->left);
                        commit();
                        genretvalue();
                        break;
        }
```

`genretvalue()` in `gen.c` will emit the code to copy the return value:

```
void genretvalue(void) {
        gentext();
        cgretvalue();
}
```

and in `cg.c`:

```
void cgretvalue(void)   { genfs("\tmovq\t%s,%%rax\n", Reg[R]); }
```

## The Implementation: Loading Registers

In original SubC, the `cgsynth()` function worked on the primary register `%rax`. It now
works on the name of the current primary register, `Reg[R]`.

The original `cgload2()` function loaded values into the secondary register `%rcx`. It now
works on the name of the current secondary register, `Reg[SR]`.

The `cglXX()` functions which load values into the primary register now work on `Reg[SR]`.
These functions are also where `cgallocreg()` gets called to allocate a new primary
register. The side effect is to make what was the primary register into the new
secondary register.

## The Implementation: Two-Register Operations

We now have the operations on two registers, which now operate on `Reg[SR]` and `Reg[R]`.
Examples are:

```
void cgadd(void)        { genfss("\taddq\t%s,%s\n", Reg[R], Reg[SR]); cgfreereg(1); }
void cgmul(void)        { genfss("\timulq\t%s,%s\n", Reg[R], Reg[SR]); cgfreereg(1); }
void cgsub(void)        { genfss("\tsubq\t%s,%s\n", Reg[R], Reg[SR]);  cgfreereg(1); }
```

Notice that the result is left in `Reg[SR]`. Once `cgfreereg(1)` is called, this frees
one of the registers and converts `Reg[SR]` into the primary register `Reg[R]`.

The division, modulo and shift operations are tricky as several specific registers
need to be set up. So there is some awkward copying of registers to make these operations
work.

## The Implementation: Calling Functions

SubC passes function arguments by pushing them on the stack. So too does the new
compiler:

```
void cgpush(void)       { genfs("\tpushq\t%s\n", Reg[R]); cgfreereg(0); }
```

Note, however, that it passes zero to `cgfreereg()`. This releases the register
for another use, but doesn't pop the value back from the stack.

One problem, now, is that we have four registers. Some of these we are probably using
in the current function. It is likely that the called function will destroy them. So
we need to spill the registers represented by the stack depth, S, onto the stack
and retrieve them after the function call:

```
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
```

Question: why do we need a stack for R and S? Why not just use the current values of R
and S? The answer is that can call functions as part of the arguments to functions, e.g.:

```
void main()
{
  a= 2; b= 10;
  for (c=0; c < 20; c++) {
    printf("%d\n", foo(b,c) + c / a);
  }
  exit(0);
}
```

At the call to `foo()`, we will have a different number of registers in use than
at the call to `printf()`. Hence we need a stack to remember what R and S are so
that we retrieve the correct number of registers when the functions return.

## The Implementation: Changes in `stmt.c`, `expr.c` and `tree.c`

Outside of `gen.c` and `cg.c`, most of the changes to SubC are minor. I've already
mentioned the change to `expr()` and the need to decorate the AST with OP_RETURN.

Everywhere in SubC where there was a call to `clear()` to free the primary register,
there is now a call to `freeregs()` in `gen.c`:

```
// Free all registers and flush the
// instruction queues if q is true.
void freeregs(int q) {
        cgfreeregs();
        if (q) {
                Q_type = empty;
                Q_cmp = cnone;
                Q_bool = bnone;
        }
}
```

In `tree.c` there is now a function call that makes explicit
that arguments have to be pushed on the function call stack:

```
void emitargs(node *a) {
      if (NULL == a) return;
      emittree1(a->right);
      genpush();
      emitargs(a->left);
}
```

The handling of function calls also makes explicit the need
to spill and unspill registers:

```
        case OP_CALL:   genspillregs();
                        emitargs(a->left);
                        commit();
                        gencall(a->args[0]);
                        genstack((a->args[1]) * BPW);
                        genunspillregs();
                        break;
        case OP_CALR:   genspillregs();
                        emitargs(a->left);
                        commit();
                        lv.prim = FUNPTR;
                        lv.sym = a->args[0];
                        genrval(&lv);
                        gencalr();
                        genstack((a->args[1]) * BPW);
                        genunspillregs();
                        break;        case OP_CALL:   genspillregs();
                        emitargs(a->left);
                        commit();
                        gencall(a->args[0]);
                        genstack((a->args[1]) * BPW);
                        genunspillregs();
                        break;
        case OP_CALR:   genspillregs();
                        emitargs(a->left);
                        commit();
                        lv.prim = FUNPTR;
                        lv.sym = a->args[0];
                        genrval(&lv);
                        gencalr();
                        genstack((a->args[1]) * BPW);
                        genunspillregs();
                        break;
```

## The Implementation: Changes in `gen.c` and `cg.c`

The bulk of the code changes for cyclic register allocation are
in `gen.c` and `cg.c`. I've already mentioned the use of
`Reg[SR]` and `Reg[R]` in `cg.c`.

At the top of `cg.c`, I've created new `genXX()` functions to to
output the assembly code to the `Outfile` because I found the
existing ones too constraining.

One function at the end of `cg.c`, `cgregtosec(), I will explain further down.

The code to deal with the `switch` jump table assumes that `%rax` will
hold the value to index the table. We need to fill this in from the
primary register, so there is this new function in `gen.c`:

```
void genswtchval(void) {
      cgldswtchval();
}
```

### The Ternary Operator

I had problems with the `? :` ternary operator. The code in `gen.c`
was generating branches to labels based on the result of the `?`
operation, but it assumed that the result was always in the same
register `%rax`. Depending on the expression being tested, this
could now be in any of four different registers.

To solve this problem, we have to keep track of which registers
were in use at certain points in time, so that we know which
ones to use later on. Thus, in `cg.c`, we have functions to
do the tracking:

```
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
```

We don't need to spill the registers, just remember which ones we should be using.
The code in `tree.c` that uses these is the code that deals with the termary operator:

```
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
```

and in `gen.c`:

```
void genstoreregs() {
        gentext();
        commit();
        cgstoreregs();
}

void genrestoreregs(int pop) {
        gentext();
        commit();
        cgrestoreregs(pop);
}
```

## Testing the New Compiler

The ultimate goal, once I started to modify SubC, was to get the new compiler to
compile itself: the so-called *triple test*. I built up to this in stages.

To start with, I would write small example C programs and get them to compile.
Each time I got one to work, I'd save it into the [tests](Cyclic/tests) directory.
In there, there is a script [mktests](Cyclic/tests/mktests) which compiles
each C program with `gcc` and the new compiler. If their output differs, the test fails.

Eventually, I started to try the triple test and failed. The problem here is that the
failure gives no clue as to where the problem is. So I wrote a couple of scripts to
do this with the original SubC code:

```
   for each C file in original SubC
     compile it with both original SubC and my compiler to assembly
     for each function in the assembly output
       replace the known-working version with my compiler's version
       run the triple test with this one function changed
```

This allowed me to narrow down problems in my compiler's output down to
the function level. I could then look at my assembly output for the
function, and also compare it to the known good assembly output for the
same function.

For each problem, I tried to create a small C program that exhibited the
problem, and put it into my [tests](Cyclic/tests) directory. Once I fixed
the bug, I would run my tests to ensure that I hadn't introduced a regression.

At some point I had all the SubC files compiling and each function working,
one at a time. However, I was still not passing the triple test. What I wasn't
doing was compiling the functions in the SubC library with my compiler.

So, next, I wrote a script to compile each library file, one at a time, with
my compiler and to build the library with just this file changed. Again, I could
narrow down to a file (usually a function) and spot the problem. Ditto, more
test programs to check for regressions.

Eventually I had got down to a single library function, `_fwrite()` that was causing a
segmentation fault. This took the longest to work out the problem, mainly as the
bug was in a different function in the [lib/fwrite.c](Cyclic/lib/fwrite.c) file,
`_fsync()`. The bug was in this line:

```
  f->end = f->ptr = 0;
```

The addresses of `f->end` and `f->ptr` were allocated to different registers,
as was the rvalue 0. Because the compiler assumes that the primary and secondary
registers are adjacent, my compiler was outputting this code:

```
        movq    %r10,(%r9)	# f->ptr = 0;
        movq    %r10,(%r9)	# Supposedly, f->end = 0;
```

The solution was to swap `%r11` and `%r10` after the first assignment and free `%r11`,
thus making `%r10` the primary register. The correct code is thus:

```
        movq    %r10,(%r9)	# f->ptr = 0;
        xchgq   %r10,%r9
        movq    %r9,(%r8)	# f->end = 0;
```

Thus, I added the `cgregtosec()` function at the end of `cg.c`

# Thoughts for the Future

At present the "generic" code generator `gen.c` has the concept of a
primary accumulator and a secondary register. This is OK for two-register
operand architectures like the x86 but not so good for the RISC architectures.
Also, my implementation of cyclic register allocation assumes that these
registers are adjacent in the cycle. Together, these force the code generator
in `cg.c` to do extra work like that shown at the end of the previous section.

I think it would be better for the functions in `cg.c` to return an integer
which is the register number that was just allocated, or 0 if none allocated.
These would probably bubble back up to `tree.c` where we could do something
like:

```
        case OP_MOD:    /* fallthru */
        case OP_LSHIFT: /* fallthru */
        case OP_RSHIFT: /* fallthru */
        case OP_DIV:    /* fallthru */
        case OP_BINAND: /* fallthru */
        case OP_BINIOR: /* fallthru */
        case OP_BINXOR: /* fallthru */
        case OP_MUL:    /* fallthru */
        case OP_SUB:    /* fallthru */
        case OP_PLUS:   /* fallthru */
        case OP_ADD:    r1= emittree1(a->left);
                        r2= emittree1(a->right);
                        commit();
                        switch(a->op) {
                        case OP_LSHIFT: r3= genshl(r1, r2); break;
                        case OP_RSHIFT: r3= genshr(r1, r2); break;
                        case OP_DIV:    r3= gendiv(r1, r2); break;
                        case OP_MOD:    r3= genmod(r1, r2); break;
                        case OP_BINAND: r3= genand(r1, r2); break;
                        case OP_BINIOR: r3= genior(r1, r2); break;
                        case OP_BINXOR: r3= genxor(r1, r2); break;
                        case OP_MUL:    r3= genmul(r1, r2); break;
                        case OP_ADD:    r3= genadd(r1, r2); break;
                        case OP_PLUS:   r3= genadd(a->args[0], a->args[1]);
                                        r3= break;
                        case OP_SUB:    r3= gensub(a->args[0], a->args[1]);
                                        r3= break;
                        }
	...
	return(r3);
```

This would allow `cg.c` to have any register allocation scheme,
and also allow non-contiguous registers and three-register operand
instructions.

But it would be a lot of changes to the compiler!
