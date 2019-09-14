#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

char	*P;				// Pointer to next token
int	A = 0;				// True if accumulator is occupied

// Emit assembly code for a CPU with a main accumulator.
// i is the operation to perform.
// x is any variable name for load ops 'l' and 'g'.
// For binary ops, x is zero.
void emit(int i, int x) {

	// Load a global or local. If A is occupied, spill
	// it on the stack. Mark it occupied. We will load
	// the value using the switch statement below
	if ('l' == i || 'g' == i) {
		if (A) emit('p', 0);
		A = 1;
	}

	// Process each possible instruction
	switch (i) {
	case '_': printf("neg  A\n");		// Negate A
		  break;
	case '+': printf("pop  X\n");		// A= A + X
		  printf("addr X,A\n");
		  break;
	case '*': printf("pop  X\n");		// A= A * X
		  printf("mulr X,A\n");
		  break;
	case '-': printf("pop  X\n");		// A= A - X
		  printf("swap A,X\n");
		  printf("subr X,A\n");
		  break;
	case '/': printf("pop  X\n");		// A= A / X
		  printf("swap A,X\n");
		  printf("divr X,A\n");
		  break;
	case 'g': printf("lg   _%c,A\n", x);	// Load A with global x
		  break;
	case 'l': printf("ll   _%c,A\n", x);	// Load A with local x
		  break;
	case 'p': printf("push A\n");		// Push A
		  break;
	}
}

// Skip whitespace
void skip(void) {
	while (isspace(*P)) P++;
}

// factor :=
//	  - factor
//	| GLOBALVAR	i.e one uppercase letter
//	| LOCALVAR	i.e one local letter
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
//	  factor
//	| factor * factor
//	| factor / factor
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
//	  sum
//	| sum + sum
//	| sum - sum
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
