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

## The Implementation

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

*More to come.*
