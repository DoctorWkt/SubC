#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Do successive register allocations, showing which register
// gets allocated and which register pair get operated on.
//
// Then do successive register deallocations, and ditto.

// **********************************
// *** Cyclic Register Allocation ***
// **********************************

// This is an implementation of cyclic register allocation
// including the use of the stack to spill registers.
// The two most important functions are push() and pop().

int     N = 4;                  // Four available general-purpose registers
                                // numbered 1 to 4
int     R = 0;                  // No register has yet been allocated
int     S = 0;                  // Depth of register stack

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
                printf("push R%d, ",R);
                S++;
        }
        else if (S) {
                R++;
                printf("push R%d, ",R);
                S++;
        }
        else {
                R++;
        }
	printf("R%d is allocated", R);
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
                printf("pop R%d, ",R);
                R = N;
                S--;
        }
        else if (S) {
                printf("pop R%d, ",R);
                S--;
                R--;
        }
        else {
                R--;
        }
	printf("R%d is allocated", R);
}

// Normally use R and R-1. But if there
// is no previous register to select (R<=1), 
// use the value N.
//
void genr() {
        int     d;

        d = R < 2? N: R-1;
        printf(", operation on R%d and R%d\n", R, d);
}

void main()
{
  int i;
  printf("Allocating registers\n");
  printf("--------------------\n");
  push(); puts("");
  for (i=0; i < 10; i++) {
    push(); genr();
  }
  puts("");
  printf("Freeing registers\n");
  printf("-----------------\n");
  for (i=0; i < 9; i++) {
    pop(); genr();
  }
}
