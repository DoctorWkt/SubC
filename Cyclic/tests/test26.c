#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Return 1 if strings are same, 0 otherwise
int fastcmp(char *a, char *b) {
  if ((*a == *b) && !strcmp(a,b))
    return(1);
  return(0);
}

void main()
{
  printf("%d\n", fastcmp("fish", "fish"));
  printf("%d\n", fastcmp("fish", "cow"));
  printf("%d\n", fastcmp("cow", "cow"));
  printf("%d\n", fastcmp("banana", "cow"));
  printf("%d\n", fastcmp("banana", "banana"));
  exit(0);
}
