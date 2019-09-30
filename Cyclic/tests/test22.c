#include <stdio.h>
#include <stdlib.h>

int x, y;

void main()
{
  x= 0xffff;
  y= x & 0xff; printf("y is %x\n", y);
  y= 0xff & x; printf("y is %x\n", y);
  y= 0xffff;
  y &= 0xff; printf("y is %x\n", y);
  exit(0);
}
