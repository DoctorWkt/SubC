#include <stdio.h>
#include <stdlib.h>

int i;

void main()
{
  for (i=0; i<10; i++)
    switch (i) {
      case 0: printf("0\n"); break;
      case 1: printf("1\n"); break;
      case 2: printf("2\n");
      case 3: printf("3\n"); break;
      case 4: printf("4\n"); break;
      default: printf("def\n");
    }
  exit(0);
}
