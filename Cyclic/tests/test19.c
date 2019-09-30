#include <stdio.h>
#include <stdlib.h>

int a[4];
int x;

void main() {
  a[2]= 16;
  x= a[2]++;		// Problem
  printf("%d\n", x);
  exit(0);
}
