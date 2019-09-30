#include <stdio.h>
#include <stdlib.h>


void main()
{
  int a;
  int *aptr;
  char b;
  char *bptr;

  aptr= &a;
  *aptr=2;
  printf("%d\n", a);
  printf("%d\n", *aptr);
  bptr= &b;
  *bptr=7;
  printf("%d\n", b);
  printf("%d\n", *bptr);
  exit(0);
}
