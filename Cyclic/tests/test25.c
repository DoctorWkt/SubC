#include <stdio.h>
#include <stdlib.h>

static int intcmp(int *x1, int *x2) {
        while (*x1 && *x1 == *x2) {
                x1++; x2++;
 	}
        return *x1 - *x2;
}

int a[5];
int b[5];
int x;

void main() {
  int i;

  a[0]= 2; a[1]= 4; a[2]= 6; a[3]= 5; a[4]=0;
  b[0]= 2; b[1]= 4; b[2]= 6; b[3]= 2; b[4]=0;

  for (i=0; i< 10; i++) {
    b[3]= i;
    x= intcmp(a, b);
    printf("%d\n", x);
  }
  exit(0);
}
