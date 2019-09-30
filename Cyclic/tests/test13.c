#include <stdio.h>
#include <stdlib.h>

int a;
int *aptr;
char b;
char *bptr;

void main()
{
  aptr= &a;
  *aptr=2;
  printf("%d\n", a);
  printf("%d\n", *aptr);
  bptr= &b;
  *bptr=2;
  printf("%d\n", b);
  printf("%d\n", *bptr);
  exit(0);
}
