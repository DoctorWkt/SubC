#include <stdio.h>
#include <stdlib.h>

int a, b, c;

int foo(int x, int y)
{
    int z;
    if (x > y)
    	z= x + y;
    else
    	z= y - x;
    return(z);
}

void main()
{
  b= 5; c= 11;
  a= c - b;    printf("%d\n", a);
  a= foo(b,c); printf("%d\n", a);
  a= foo(c,b); printf("%d\n", a);
  exit(0);
}
