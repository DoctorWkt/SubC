#include <stdio.h>
#include <stdlib.h>

int a[4];
int *aptr;
char b[5];
char *bptr;

void main()
{
  int c[4];
  int *cptr;
  char d[5];
  char *dptr;

  aptr= &a[2];
  *aptr=2;
  printf("%d\n", a[2]);
  printf("%d\n", *aptr);
  bptr= &b[3];
  *bptr=7;
  printf("%d\n", b[3]);
  printf("%d\n", *bptr);
  cptr= &c[2];
  *cptr=2;
  printf("%d\n", c[2]);
  printf("%d\n", *cptr);
  dptr= &d[3];
  *dptr=7;
  printf("%d\n", d[3]);
  printf("%d\n", *dptr);
  exit(0);
}
