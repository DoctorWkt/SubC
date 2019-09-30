#include <stdio.h>
#include <stdlib.h>

int a, b, c;

void main()
{
  a= 6;
  for (c=0; c < 20; c++) {
    printf("%d\n", c % a);
  }
  exit(0);
}
