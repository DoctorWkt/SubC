#include <stdio.h>
#include <stdlib.h>

void main()
{

  char *a[12];

  a[1]= "Hello";

  switch(a[1][1]) {
    case 'e': printf("Correct\n"); break;
    default: printf("Bas\n"); break;
  }
  exit(0);
}
