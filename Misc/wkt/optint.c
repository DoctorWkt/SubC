// This is a extension of the SubC code
// from book/cg4.c and book/opt1.c
// to be an interpreter. The interpreter
// has two statement types:
//
// >   expression ;     - print an expression
// V = expression ;     - set the variable to an expression

// For each statement:
//   Parse and build an AST. Optimise it.
//   Generate code with cyclic register allocation.
//   Perform each instruction with the interpreter

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// ************************
// *** Pre-declarations ***
// ************************

int drawflag = 0;		// 1 = dump, 2 = draw

void gen(char *s);
void gen2(char *s, int a);
void genr(char *s);
int fold(int n);
int rewrite(int n);
int cse(int n);

#define NODES	1024		// Up to 1024 AST nodes
				// Each AST node has these member fields
int Type[NODES],		// The operation being performed on
  Left[NODES],			// the left, right, neither or both sub-trees
  Right[NODES], Value[NODES];	// Leaf nodes have a value

#define N 4			// Four available general-purpose registers
				// numbered 1 to 4
int R = 0;			// No register has yet been allocated
int S = 0;			// Depth of register stack


int Reg[N + 1];			// Registers for the interpreter
int Var[128];			// Variables for the interpreter
int Stack[1024];		// Hardware stack
int Stkptr = 0;			// Pointer to hardware stack


// Die with an error message
void Die(char *s, int t) {
  fprintf(stderr, s, t);
  exit(1);
}


// **********************************
// *** Cyclic Register Allocation ***
// **********************************

// This is an implementation of cyclic register allocation
// including the use of the stack to spill registers.
// The two most important functions are push() and pop().

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
    Stack[Stkptr++] = Reg[R];
    // printf("Pushed %d\n", Reg[R]);
    S++;
  } else if (S) {
    R++;
    gen("push R%d");
    Stack[Stkptr++] = Reg[R];
    // printf("Pushed %d\n", Reg[R]);
    S++;
  } else {
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
    Reg[R] = Stack[--Stkptr];
    // printf("Popped %d\n", Reg[R]);
  } else if (S) {
    gen("pop  R%d");
    S--;
    R--;
    Reg[R] = Stack[--Stkptr];
    // printf("Popped %d\n", Reg[R]);
  } else {
    R--;
  }
}

// When we get a ',' operation, we now have
// a second expression. Free up all the
// registers
void freeallregs() {
  R = 0; S = 0;
}

// ***********************
// *** Code Generation ***
// ***********************

// A one-position instruction queue
// When Qi==0, the queue is empty.
// When Qi==1, Qx holds the queued instruction.
int Qi = 0, Qx;

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
  int d = R < 2 ? N : R - 1;
  printf(s, R, d);
  putchar('\n');
}

// Do code synthesis to generate
// instruction(s) for the operation in i.
// We only synthesize binary instructions.
void synth(int i) {
  int isv;
  int d = R < 2 ? N : R - 1;

  // isv is true if the second operand is a variable
  isv = isalpha(Qx);

  // If there's nothing in the queue, then we
  // already have both operands in two registers.

  // If a queued load instruction, use direct
  // addressing to access either the global or
  // local variable. For division and subtraction,
  // swap A and X to get the operands in the correct order.
  switch (i) {
    case '+':
      if (!Qi) {
	genr("  addr R%d,R%d");
	Reg[d] = Reg[d] + Reg[R];
	// printf("R now %d\n", Reg[d]);
      } else if (isv) {
	gen2("  add   %c,R%d", Qx);
	Reg[R] = Reg[R] + Var[Qx];
	// printf("R now %d\n", Reg[R]);
      } else {
	gen2("  add  #%d,R%d", Qx);
	Reg[R] = Reg[R] + Qx;
	// printf("R now %d\n", Reg[R]);
      }
      break;
    case '*':
      if (!Qi) {
	genr("  mul  R%d,R%d");
	Reg[d] = Reg[d] * Reg[R];
	// printf("R now %d\n", Reg[d]);
      } else if (isv) {
	gen2("  mul   %c,R%d", Qx);
	Reg[R] = Reg[R] * Var[Qx];
	// printf("R now %d\n", Reg[R]);
      } else {
	gen2("  mul  #%d,R%d", Qx);
	Reg[R] = Reg[R] * Qx;
	// printf("R now %d\n", Reg[R]);
      }
      break;
    case '-':
      if (!Qi) {
	genr("  swap R%d,R%d");
	genr("  subr R%d,R%d");
	int temp = Reg[R];
	Reg[R] = Reg[d];
	Reg[d] = temp;
	Reg[d] = Reg[d] - Reg[R];
	// printf("R now %d\n", Reg[d]);
      } else if (isv) {
	gen2("  sub   %c,R%d", Qx);
	Reg[R] = Reg[R] - Var[Qx];
	// printf("R now %d\n", Reg[R]);
      } else {
	gen2("  sub  #%d,R%d", Qx);
	Reg[R] = Reg[R] - Qx;
	// printf("R now %d\n", Reg[R]);
      }
      break;
    case '/':
      if (!Qi) {
	genr("  swap R%d,R%d");
	genr("  divr R%d,R%d");
	int temp = Reg[R];
	Reg[R] = Reg[d];
	Reg[d] = temp;
	Reg[d] = Reg[d] / Reg[R];
	// printf("R now %d\n", Reg[d]);
      } else if (isv) {
	gen2("  div   %c,R%d", Qx);
	Reg[R] = Reg[R] / Var[Qx];
	// printf("R now %d\n", Reg[R]);
      } else {
	gen2("  div  #%d,R%d", Qx);
	Reg[R] = Reg[R] / Qx;
	// printf("R now %d\n", Reg[R]);
      }
      break;
  }

  // If there was a queued instruction,
  // we've used that operand, so free that register.
  // Set the queue empty
  if (!Qi)
    pop();
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
    case 'c':
      gen2("  load #%d,R%d", Qx);
      Reg[R] = Qx;
      // printf("R now %d\n", Reg[R]);
      break;
    case 'v':
      gen2("  load  %c,R%d", Qx);
      Reg[R] = Var[Qx];
      // printf("R now %d\n", Reg[R]);
      break;
  }

  // Set the queue empty again
  Qi = 0;
}

// Queue an instruction. If there's
// already an instruction in the queue,
// generate that instruction.
void queue(int i, int x) {
  if (Qi)
    load();
  Qi = i;
  Qx = x;
}

// Emit assembly code for a CPU with a main accumulator.
// n is the root of the AST tree.
// i is the instruction.
// x is a literal constant for 'c' operations.
// x is a variable name for 'v' operations.
// For binary ops, x is zero.
void emit(int n) {
  int i, x;

  // No node, return
  if (!n)
    return;
  i = Type[n];
  x = Value[n];

  switch (i) {

      // For load instructions, just queue the instruction
    case 'c':
    case 'v':
      queue(i, x);
      break;

      // Print operation
    case '>':
      emit(Right[n]);		// Do the right-hand expression
      if (Qi)
	load();			// Flush final instruction
      gen("  prnt    R%d");
      printf("Result is %d\n", Reg[R]);
      break;

      // Assignment operation
    case '=':
      emit(Right[n]);		// Do the right-hand expression
      if (Qi)
	load();			// Flush final instruction
      gen2("  stor  %c,R%d", Value[Left[n]]);
      Var[Value[Left[n]]] = Reg[R];
      // printf("Stored %d in Variable %c\n", Reg[R], Value[ Left[n] ]);
      break;

      // Two sequential expressions
    case ',':
      emit(Left[n]);		// First emit the left-hand child code
      if (Qi)
	load();			// Flush final instruction
      freeallregs();		// Free the registers
      emit(Right[n]);		// Now do the right-hand one
      break;

      // Otherwise, generate synthesized code for the instruction
    default:
      emit(Left[n]);
      emit(Right[n]);
      synth(i);
      break;
  }
}


// **********************
// *** AST Generation ***
// **********************

// Type can be one of: + - * /, or
//      'c' for a literal constant stored in Value, or
//      'v' for a single-letter variable name store in Value

// We start at node 1 because 0 is used in Left and Right to indicate
// no child sub-node
int Next = 1;

// Allocate a node from the list with
// the given values and return its slot number
int node(int t, int v, int l, int r) {
  if (Next >= NODES) {
    fprintf(stderr, "out of nodes\n");
    exit(EXIT_FAILURE);
  }
  Type[Next] = t;
  Value[Next] = v;
  Left[Next] = l;
  Right[Next] = r;
  return Next++;
}

// Recursively dump the AST rooted at n.
// Indent by k spaces.
void dump(int n, int k) {
  int i;

  // No node, return
  if (!n)
    return;

  // Print k spaces
  for (i = 0; i < k; i++)
    printf("  ");

  // Print out the operator, 'c' or 'v'
  putchar(Type[n]);

  // Print out a leaf's literal value or variable name
  if ('c' == Type[n])
    printf("(%d)", Value[n]);
  else if ('v' == Type[n])
    printf("(%c)", Value[n]);

  // End the line, then dump the left and right children
  putchar('\n');
  dump(Left[n], k + 1);
  dump(Right[n], k + 1);
}

// These functions draw the AST using
// pic and groff. Invoke with -d -d.
void draw2(int n, int w, int d) {
  if (!n)
    return;
  if ('c' == Type[n])
    printf("N%d: box width 0.3i height 0.3i \"%d\"\n", n, Value[n]);
  else if ('v' == Type[n])
    printf("N%d: box width 0.3i height 0.3i \"%c\"\n", n, Value[n]);
  else
    printf("N%d: circle radius 0.15i \"%c\"\n", n, Type[n]);
  if (Left[n]) {
    printf("move to N%d\n", n);
    if (Right[n]) {
      printf("move down 0.5i left %d.%di\n", w / 1000, w % 1000);
      draw2(Left[n], w / d, d + 1);
      printf("arrow from left of N%d to top of N%d\n", n, Left[n]);
    } else {
      printf("move down 0.5i\n");
      draw2(Left[n], w / d, d + 1);
      printf("arrow from bottom of N%d to top of N%d\n", n, Left[n]);
    }
  }
  if (Right[n]) {
    printf("move to N%d\n", n);
    if (Left[n]) {
      printf("move down 0.5i right %d.%di\n", w / 1000, w % 1000);
      draw2(Right[n], w / d, d + 1);
      printf("arrow from right of N%d to top of N%d\n", n, Right[n]);
    } else {
      printf("move down 0.5i\n");
      draw2(Right[n], w / d, d + 1);
      printf("arrow from bottom of N%d to top of N%d\n", n, Right[n]);
    }
  }
}

// These functions draw the AST using
// pic and groff. Invoke with -d -d.
void draw(int n) {
  printf(".ft C\n.ps 12\n.PS\n");
  draw2(n, 1800, 24);
  printf(".PE\n");
}

// ****************************
// *** Scanning and Parsing ***
// ****************************

char *P;			// The most recent token character scanned in

// Skip whitespace on the input
void skip(void) {
  while (isspace(*P))
    P++;
}

int sum(void);

// factor := 
//        VARIABLE
//      | CONSTANT
//      | '(' sum ')'
//      | '-' factor

int factor(void) {
  int n = 0, v;

  // Skip whitespace
  skip();

  // Parentheses: skip, parse with sum, skip ')'
  if ('(' == *P) {
    P++;
    n = sum();
    P++;
  }

  // Minus: skip, make a unary '-' node with the
  // factor() expression as the left child
  else if ('-' == *P) {
    P++;
    n = node('-', 0, factor(), 0);
  }

  // Digit: build up the decimal value, make
  // constant leaf node with this value
  else if (isdigit(*P)) {
    for (v = 0; isdigit(*P); P++)
      v = v * 10 + *P - '0';
    n = node('c', v, 0, 0);
  }

  // Letter: make a variable leaf node with this letter
  else if (isalpha(*P)) {
    n = node('v', *P++, 0, 0);
  }

  // Return the slot number of the tree
  return n;
}

// term :=
//        factor
//      | factor '*' factor
//      | factor '/' factor

int term(void) {
  int n, n2;

  // Skip whitespace, parse the next expression
  skip();
  n = factor();
  for (;;) {
    // Skip following whitespace
    skip();
    switch (*P) {

      // Next token was '*'. Skip over it. Parse
      // the next expression. Build a binary node
      // with '*' as the operator and both children
      case '*':
	P++;
	n2 = factor();
	n = node('*', 0, n, n2);
	break;

      // Next token was '/'. Skip over it. Parse
      // the next expression. Build a binary node
      // with '/' as the operator and both children
      case '/':
	P++;
	n2 = factor();
	n = node('/', 0, n, n2);
	break;

      // No following '*' or '/', return the factor tree
      default:
	return n;
    }
  }
}

// sum :=
//        term
//      | term '+' term
//      | term '-' term
int sum(void) {
  int n, n2;

  // Skip whitespace, parse the next expression
  skip();
  n = term();
  for (;;) {
    // Skip following whitespace
    skip();
    switch (*P) {

      // Next token was '+'. Skip over it. Parse
      // the next expression. Build a binary node
      // with '+' as the operator and both children
      case '+':
	P++;
	n2 = term();
	n = node('+', 0, n, n2);
	break;

      // Next token was '-'. Skip over it. Parse
      // the next expression. Build a binary node
      // with '-' as the operator and both children
      case '-':
	P++;
	n2 = term();
	n = node('-', 0, n, n2);
	break;
      default:
	return n;
    }
  }
}


// Parse an expression.
int expr() {
  return sum();
}

// statment :=
//        > sum ;
//      | VARIABLE = sum ;

// Parse a statement. 
void statement() {
  int n;
  int v = 0;
  int t;

  // Skip whitespace
  skip();

  // Print statement
  if (*P == '>') {
    // Print statement
    P++;
    n = expr();
    // Make a print node with a right child
    n = node('>', 0, 0, n);

    // Assignment statement
  } else if (*P >= 'A' && *P <= 'z') {
    // Get the variable name
    v = *P++;

    // Get the '=' token
    skip();
    if (*P != '=')
      Die("Unrecognised token %c\n", *P);

    // Get AST tree of the rvalue
    P++;
    n = expr();
    // Make a variable node
    t = node('v', v, 0, 0);
    // Make an assignment tree
    n = node('=', 0, t, n);
  } else {
    Die("Unrecognised token %c\n", *P);
  }

  // Find the terminating ';'
  skip();
  if (*P != ';')
    Die("Expecting ';', got token %c\n", *P);
  P++;

  // Perform expression folding with fold().
  // Perform strength reduction with rewrite().
  // Perform common sub-expression elimination with cse().
  // Dump or draw the final AST tree.
  // Finally emit the assembly code for the tree
  n = fold(n);
  n = rewrite(n);
  n = cse(n);
  if (drawflag == 1)
    dump(n, 0);
  if (drawflag == 2)
    draw(n);
  emit(n);
  if (Qi)
    load();			// Flush final instruction
  freeallregs();
}

// ********************
// *** Optimisation ***
// ********************

// Perform constant expression folding
// on the tree rooted at n, returning
// the root of a new tree.
int fold(int n) {
  int cl, cr, vl, vr;

  // Empty tree, nothing to do
  if (!n)
    return 0;

  // Fold both child sub-trees
  Left[n] = fold(Left[n]);
  Right[n] = fold(Right[n]);

  // Determine if the left child and/or
  // the right child is a literal constant
  cl = Left[n] && 'c' == Type[Left[n]];
  cr = Right[n] && 'c' == Type[Right[n]];

  // No right child, left child is a literal
  // constant and the operation is unary minus
  if (cl && 0 == Right[n]) {
    if ('-' == Type[n])
      // Replace with a negative literal value
      return node('c', -Value[Left[n]], 0, 0);
  }

  // Get the values from left and right child. Set to zero
  // if no child
  vl = Left[n] ? Value[Left[n]] : 0;
  vr = Right[n] ? Value[Right[n]] : 0;

  // Both children are literal constants
  if (cl && cr) {

    // So we can do the node's operation here
    // and save assembly instructions. Only
    // do a division if the divisor is not zero.
    switch (Type[n]) {
      case '+':
	return node('c', vl + vr, 0, 0);
      case '-':
	return node('c', vl - vr, 0, 0);
      case '*':
	return node('c', vl * vr, 0, 0);
      case '/':
	if (vr)
	  return node('c', vl / vr, 0, 0);
    }
  }

  // 0 + right is just right.
  if (cl && 0 == vl && '+' == Type[n]) {
    return Right[n];
  }

  // 1 * right is just right.
  if (cl && 1 == vl && '*' == Type[n]) {
    return Right[n];
  }

  // left + or - 0 is just left
  if (cr && 0 == vr && ('+' == Type[n] || '-' == Type[n])) {
    return Left[n];
  }

  // left * or / 1 is just left
  if (cr && 1 == vr && ('*' == Type[n] || '/' == Type[n])) {
    return Left[n];
  }

  // Either child is 0 and multiply, result is just 0
  if ((cr && 0 == vr || cl && 0 == vl) && '*' == Type[n]) {
    return node('c', 0, 0, 0);
  }
  return n;
}

// Return true if n is a leaf node
int leaf(int n) {
  return n && ('c' == Type[n] || 'v' == Type[n]);
}

int rewrite(int n) {
  int t;

  if (!n)
    return 0;
  Left[n] = rewrite(Left[n]);
  Right[n] = rewrite(Right[n]);
  if ('+' == Type[n] || '*' == Type[n]) {
    if (leaf(Left[n]) && !leaf(Right[n])) {
      t = Left[n];
      Left[n] = Right[n];
      Right[n] = t;
    }
  }
  if ('+' == Type[n] && Right[n] && '-' == Type[Right[n]] && !Right[Right[n]]
    ) {
    return node('-', 0, Left[n], Left[Right[n]]);
  }
  if ('-' == Type[n] &&
      Left[n] && Right[n] && 'c' == Type[Left[n]] && 0 == Value[Left[n]]
    ) {
    return node('-', 0, Right[n], 0);
  }
  if ('*' == Type[n] &&
      leaf(Left[n]) &&
      Right[n] && Type[Right[n]] == 'c' && Value[Right[n]] == 2) {
    return node('+', 0, Left[n], Left[n]);
  }
  return n;
}

// Return true if trees n1 and n2 are equal.
// This means that the Type and Value of this
// node are the same, the left trees are the
// same and the right trees are the same.
int equal(int n1, int n2) {
  // Return true if both n1 and n2 are empty
  if (!n1)
    return 0 == n2;
  if (!n2)
    return 0 == n1;

  // False if the Types or Values differ
  if (Type[n1] != Type[n2])
    return 0;
  if (Value[n1] != Value[n2])
    return 0;

  // Recursively check the left & right sub-trees
  return equal(Left[n1], Left[n2]) && equal(Right[n1], Right[n2]);
}

// Find tree x in larger tree n. Return true if it's
// found, false otherwise.
int find(int x, int n) {

  // No match if n is empty
  if (!n)
    return 0;

  // Return if x and n are equal, but not
  // the same tree
  if (x != n && equal(x, n))
    return 1;

  // Didn't find an exact match, so try finding x
  // in the left or right sub-tree
  return find(x, Left[n]) || find(x, Right[n]);
}

// Return the number of nodes in the tree rooted at n
int size(int n) {
  if (!n)
    return 0;
  return 1 + size(Left[n]) + size(Right[n]);
}

// Sub will be the biggest common sub-tree found in t
// K will be the size of the biggest common sub-tree found so far
int Sub, K;

// Traverse t looking for sub-tree n. If not found, try the
// children in n.
void trav2(int t, int n) {
  int k;

  // Nothing to search for, return
  if (!n)
    return;

  // If the trees are not identical, there is more than
  // 2 nodes in n, we can find tree n in tree t, and
  // n's size is bigger than the biggest common sub-tree
  // found so far: record n's size and existence
  if (t != n && (k = size(n)) > 2 && find(n, t) && k > K) {
    K = k;
    Sub = n;
  }

  // Didn't find n in t, so try both of n's children
  trav2(t, Left[n]);
  trav2(t, Right[n]);
}

// Given a tree n, find and record the largest common
// sub-tree in N in the Sub variable
int maxduptree(int n) {
  // Start with no known common sub-tree, size 0
  Sub = 0;
  K = 0;

  // Traverse n looking for n or n's children
  trav2(n, n);
  return Sub;
}

// Replace sub-tree x everywhere in n where it occurs
int replace(int x, int n) {

  // Tree n is empty, return
  if (!n)
    return 0;

  // If both trees are equal, replace the n tree
  // with a variable '@' that represents the
  // pre-calculated value of x. Note that equal()
  // won't be true when x and n are the same tree,
  // so there will be one part of the n tree where
  // x still exists.
  if (equal(x, n))
    return node('v', '@', 0, 0);

  // x and n are not equal, so recursively
  // search and replace x in n's two children
  return node(Type[n], Value[n], replace(x, Left[n]), replace(x, Right[n]));
}

// Replace the biggest common sub-tree in n
int cse(int n) {
  int csub, t;

  // Find the slot number of the biggest common sub-tree
  csub = maxduptree(n);

  // If we found it
  if (csub) {
    // Recursively replace csub in the n tree
    n = replace(csub, n);

    // Create a variable node which gets
    // assigned the common sub-tree
    t = node('v', '@', 0, 0);
    csub = node('=', 0, t, csub);

    // Finally, glue this assignment tree to the
    // n tree where the common sub-tree has been
    // replaced
    n = node(',', 0, csub, n);
  }
  return n;
}

// ********************
// *** Main Program ***
// ********************

// Turn on AST dumping if -d is on the command line once.
// Turn on AST drawing if -d is on the command line twice.
// Open the file on the command line and parse each line.
int main(int argc, char **argv) {
  FILE *zin;
  char buf[1024];

  if (argc > 1 && !strcmp(argv[1], "-d")) {
    drawflag++; argc--; argv++;
  }

  if (argc != 2) {
    fprintf(stderr, "Usage: optint [-d] [-d] inputfile\n");
    exit(1);
  }

  if ((zin = fopen(argv[1], "r")) == NULL) {
    fprintf(stderr, "Unable to open %s\n", argv[1]);
    exit(1);
  }

  while (!feof(zin)) {
    fgets(buf, 1024, zin);
    P = buf;
    printf("Input is %s", buf);
    while (*P != '\n')
      statement();
  }
  fclose(zin);
  return EXIT_SUCCESS;
}
