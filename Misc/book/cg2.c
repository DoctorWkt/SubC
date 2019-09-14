#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

char	*P;				// Pointer to next token
int	A = 0;				// True if accumulator is occupied

// A one-position instruction queue
// When Qi==0, the queue is empty.
// When Qi==1, Qx holds the queued instruction.
int	Qi = 0, Qx;

// Print out a fixed format instruction
void gen(char *s) {
	puts(s);
}

// Print out a parameterised instruction
void gen2(char *s, int a) {
	printf(s, a);
	putchar('\n');
}

// Do code synthesis to generate
// instruction(s) for the operation in i.
// We only synthesize binary instructions.
void synth(int i) {
	int	g;

	// g is true if the second operand is a global
	g = isupper(Qx);

	// If there's no queued instruction, A would
	// have been pushed. Get the first operand
	// into the X register
	if (!Qi) gen("pop  X");

	// If there's no queued instruction, work on
	// both registers. If a queued load instruction,
	// use direct addressing to access either the
	// global or local variable.
	// For division and subtraction, swap A and X
	// to get the operands in the correct order.
	switch (i) {
	case '+': if (!Qi)
			gen("addr X,A");	// A= A + X
		  else if (g)
			gen2("addg _%c,A", Qx);
		  else
			gen2("addl _%c,A", Qx);
		  break;
	case '*': if (!Qi)
			gen("mulr X,A");	// A= A * X
		  else if (g)
			gen2("mulg _%c,A", Qx);
		  else
			gen2("mull _%c,A", Qx);
		  break;
	case '-': if (!Qi) {
			gen("swap A,X");
			gen("subr X,A");	// A= A - X
		  }
		  else if (g)
			gen2("subg _%c,A", Qx);
		  else
			gen2("subl _%c,A", Qx);
		  break;
	case '/': if (!Qi) {
			gen("swap A,X");
			gen("divr X,A");	// A= A / X
		  }
		  else if (g)
			gen2("divg _%c,A", Qx);
		  else
			gen2("divl _%c,A", Qx);
		  break;
	}

	// There is no longer a queued load instruction
	Qi = 0;
}

// Load a value into the accumulator using
// the instruction in the queue. If the
// accumulator is occupied, spill it
// on the stack. Generate code to load a
// local or a global variable. Finally mark
// the accumulator occupied and the queue empty.
void load(void) {
	if (A) gen("push A");
	switch (Qi) {
	case 'l': gen2("ll   _%c,A", Qx);
		  break;
	case 'g': gen2("lg   _%c,A", Qx);
		  break;
	}
	Qi = 0;
	A = 1;
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
		  gen("neg  A");
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

// Point P at the start of the expression
// and then parse it
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
