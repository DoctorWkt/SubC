#include <stdio.h>
#include <stdlib.h>

char b[5];
char *bptr;

void main()
{
  char d[5];
  char *dptr;

  bptr= &b[3];
  *bptr=4;
  printf("%d\n", b[3]);

  dptr= &d[3];
  *dptr=7;
  printf("%d\n", d[3]);
  printf("%d\n", *dptr);
  exit(0);
}
