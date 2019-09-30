#include <stdio.h>
#include <stdlib.h>


int foo(int x, int y)
{
    return(x + y);
}

void main()
{
  int a, b, c;
  a=4; b=5; c=6;
  a= foo(b,c);
  printf("%d\n", a);
  a= foo(c,b);
  printf("%d\n", a);
  exit(0);
}
