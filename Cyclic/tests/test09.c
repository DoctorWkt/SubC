#include <stdio.h>
#include <stdlib.h>

int a, b, c;

int foo(int x, int y)
{
    return(x + y);
}

void main()
{
  a=4; b=5; c=6;
  a= b + a * c  + foo(b,c);
  printf("%d\n", a);
  a= b + foo(b,c) * c + a * b;
  printf("%d\n", a);
  exit(0);
}
