#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int a, b, c;

int dbl(int x) { return x + x; }

void main()
{
  b=10; c=4;
  a= dbl(b + c - strlen("foobar"));
  printf("%d\n", a);
  exit(0);
}
