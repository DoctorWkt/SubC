#include <stdio.h>
#include <stdlib.h>

#define MAXNMAC		32

int next();

char	*Macp[MAXNMAC];
int	Macc[MAXNMAC];
int Putback= 0;
int Mp;

void playmac(char *s) {
        Macc[Mp] = next();
        Macp[Mp++] = s;
}

// Get the next character from the input file.
int next(void) {
        if (Mp) {                               // If we are expanding a macro
                if ('\0' == *Macp[Mp-1]) {      // and we've reached the end,
                        Macp[Mp-1] = NULL;      // set the expanded ptr to NULL
                        return Macc[--Mp];      // pop the stack and return the
                }                               // first real char after the macro
                else {
                        return *Macp[Mp-1]++;   // Return the next char from the
                }                               // expanded macro
        }
        return('X');
}

int ch;

void main() {
  Mp = 0;
  playmac("hello ");
  while (1) {
    ch= next();
    printf("%c", ch);
    if (ch== 'X') break;
  }
  printf("\n");
  exit(0);
}
