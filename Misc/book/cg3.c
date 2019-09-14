#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

char	*P;			// Pointer to next token
int	N = 4;			// Four available general-purpose registers
				// numbered 1 to 4.
int	R = 0;			// No register has yet been allocated

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
// the previously allocated register
void genr(char *s) {
	printf(s, R, R-1);
	putchar('\n');
}

// Select the next available register.
// The word "push" is a misnomer.
void push(void) {
	if (++R > N) {
		fprintf(stderr, "out of registers\n");
		exit(1);
	}
}

// Free up the currently allocated register,
// and go back to the previous register. Again,
// the word "pop" is a misnomer.
void pop(void) {
	R--;
}

// Do code synthesis to generate
// instruction(s) for the operation in i.
// We only synthesize binary instructions.
void synth(int i) {
	int	g;

	// g is true if the second operand is a global
	g = isupper(Qx);

	// If there's nothing in the queue, then we
	// already have both operands in R and R-1.

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

// Queue a load instruction.
// Issue any existing load instruction
// in the queue.
void queue(int i, int x) {
	if (Qi) load();
	Qi = i;
	Qx = x;
}

// Emit assembly code for the CPU.
// i is the operation to perform.
// x is any variable name for load ops 'l' and 'g'.
// For binary ops, x is zero.
void emit(int i, int x) {
	switch (i) {
	case 'l':
	case 'g': queue(i, x);
		  break;
	case '_': load();
		  gen("neg  R%d");
		  break;
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
//      | ( sum )
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
