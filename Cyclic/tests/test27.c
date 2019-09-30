#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int a[5], b, c;

void main()
{
  a[2]=0; b=2; c=4;
  
  a[2]= (b>c) ? b : c;
  printf("%d\n", a[2]);
  exit(0);
}
