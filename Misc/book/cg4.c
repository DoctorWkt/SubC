#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

// This is an implementation of cyclic register allocation
// including the use of the stack to spill registers.
// The two most important functions are push() and pop().

char	*P;			// Pointer to next token
int	N = 4;			// Four available general-purpose registers
				// numbered 1 to 4
int	R = 0;			// No register has yet been allocated
int	S = 0;			// Depth of register stack

// A one-position instruction queue
// When Qi==0, the queue is empty.
// When Qi==1, Qx holds the queued instruction.
int	Qi = 0, Qx;

// Print out a fixed format instruction
void gen(char *s) {
	printf(s, R);
	putchar('\n');
}

// Print out a parameterised instruction
void gen2(char *s, int a) {
	printf(s, a, R);
	putchar('\n');
}

// Generate an instruction using the
// currently allocated register and
// a previously allocated register

// Normally use R and R-1. But if there
// is no previous register to select (R<=1), 
// use the value N.
//
void genr(char *s) {
	int	d;

	d = R < 2? N: R-1;
	printf(s, R, d);
	putchar('\n');
}

// Allocate a register by setting R to
// the next available register. Normally,
// just increment R and use that register.
// We can only do this if we haven't
// allocated it before (S==0, R<N).

// When R>=N, we've run out of registers.
// push R1 onto the stack and record the
// push by increasing the stack depth in S.

// If we have pushed registers on the stack
// (i.e. S>0, we've looped around the circle
// of registers at least once), push and
// allocate the next register. Record the
// push by increasing the stack depth in S.
void push(void) {
	if (R >= N) {
		R = 1;
		gen("push R%d");
		S++;
	}
	else if (S) {
		R++;
		gen("push R%d");
		S++;
	}
	else {
		R++;
	}
}

// Free up previously allocated register R.
// Normally, just decrement R.
// We can only do this if we haven't
// allocated it before (S==0, R<N).

// When there is no previous register to
// select (R<=1), pop the current register
// from the stack, decrement the stack count
// and set R to N.

// If there are registers on the stack (S!=0),
// pop the current register from the stack,
// decrement the stack count and decrement R.
void pop(void) {
	if (R <= 1) {
		gen("pop  R%d");
		R = N;
		S--;
	}
	else if (S) {
		gen("pop  R%d");
		S--;
		R--;
	}
	else {
		R--;
	}
}

// Do code synthesis to generate
// instruction(s) for the operation in i.
// We only synthesize binary instructions.
void synth(int i) {
	int	g;

	// g is true if the second operand is a global
	g = isupper(Qx);

	// If there's nothing in the queue, then we
        // already have both operands in two registers.

        // If a queued load instruction, use direct
        // addressing to access either the global or
        // local variable. For division and subtraction,
        // swap A and X to get the operands in the correct order.
	switch (i) {
	case '+': if (!Qi)
			genr("addr R%d,R%d");
		  else if (g)
			gen2("addg _%c,R%d", Qx);
		  else
			gen2("addl _%c,R%d", Qx);
		  break;
	case '*': if (!Qi)
			genr("mulr R%d,R%d");
		  else if (g)
			gen2("mulg _%c,R%d", Qx);
		  else
			gen2("mull _%c,R%d", Qx);
		  break;
	case '-': if (!Qi) {
			genr("swap R%d,R%d");
			genr("subr R%d,R%d");
		  }
		  else if (g)
			gen2("subg _%c,R%d", Qx);
		  else
			gen2("subl _%c,R%d", Qx);
		  break;
	case '/': if (!Qi) {
			genr("swap R%d,R%d");
			genr("divr R%d,R%d");
		  }
		  else if (g)
			gen2("divg _%c,R%d", Qx);
		  else
			gen2("divl _%c,R%d", Qx);
		  break;
	}

	// If there was a queued instruction,
	// we've used that operand, so free that register.
	// Set the queue empty
	if (!Qi) pop();
	Qi = 0;
}

// Load a global or local variable into
// a register based on the instruction
// in the queue.
void load(void) {
	// Choose a new register
	push();

	// Issue either a local or global load
	switch (Qi) {
	case 'l': gen2("ll   _%c,R%d", Qx);
		  break;
	case 'g': gen2("lg   _%c,R%d", Qx);
		  break;
	}

	// Set the queue empty again
	Qi = 0;
}

// Queue an instruction. If there's
// already an instruction in the queue,
// generate that instruction.
void queue(int i, int x) {
	if (Qi) load();
	Qi = i;
	Qx = x;
}

// Emit assembly code for a CPU with a main accumulator.
// i is the operation to perform.
// x is any variable name for load ops 'l' and 'g'.
// For binary ops, x is zero.
void emit(int i, int x) {
	switch (i) {

	// For load instructions, just queue the instruction
	case 'l':
	case 'g': queue(i, x);
		  break;

	// For negating instructions, load the accumulator
        // and then generate a negate instruction
	case '_': load();
		  gen("neg  R%d");
		  break;

	// Otherwise, generate synthesized code for the instruction
	default:  synth(i);
		  break;
	}
}

// Skip whitespace
void skip(void) {
	while (isspace(*P)) P++;
}

void sum(void);

// factor :=
//        - factor
//      | GLOBALVAR     i.e one uppercase letter
//      | LOCALVAR      i.e one local letter
//
void factor(void) {
	skip();
	if ('-' == *P) {
		P++;
		factor();
		emit('_', 0);
	}
	else if ('(' == *P) {
		P++;
		sum();
		P++;
	}
	else if (isupper(*P))
		emit('g', *P++);
	else
		emit('l', *P++);
}

// term :=
//        factor
//      | factor * factor
//      | factor / factor
//
void term(void) {
	skip();
	factor();
	for (;;) {
		skip();
		switch (*P) {
		case '*': P++;
			  factor();
			  emit('*', 0);
			  break;
		case '/': P++;
			  factor();
			  emit('/', 0);
			  break;
		default:  return;
		}
	}
}

// sum :=
//        sum
//      | sum + sum
//      | sum - sum
//
void sum(void) {
	skip();
	term();
	for (;;) {
		skip();
		switch (*P) {
		case '+': P++;
			  term();
			  emit('+', 0);
			  break;
		case '-': P++;
			  term();
			  emit('-', 0);
			  break;
		default:  return;
		}
	}
}

// Point P at the start of the
// expression and then parse it
void expr(char *s) {
	P = s;
	sum();
}

// Parse the argument on the command line
// or an empty string if no argument
int main(int argc, char **argv) {
	expr(argc>1? argv[1]: "");
	return EXIT_SUCCESS;
}
