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
  a= 2; b= 10;
  for (c=0; c < 20; c++) {
    printf("%d\n", foo(b,c) + c / a);
  }
  exit(0);
}
