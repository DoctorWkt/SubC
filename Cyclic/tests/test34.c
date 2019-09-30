#include <stdio.h>
#include <stdlib.h>

struct x {
  int a;
  int b;
  int c;
};

struct x xx;
struct x *xptr;

void main()
{
  xx.a= 2; xx.b= 3; xx.c= 4;
  xx.a= xx.b = xx.c = 0;
  printf("%d %d %d\n", xx.a, xx.b, xx.c);
  xptr= &xx;
  xptr->a= 2; xptr->b= 3; xptr->c= 4;
  xptr->a= xptr->b = xptr->c = 0;
  printf("%d %d %d\n", xptr->a, xptr->b, xptr->c);
  exit(0);
}
