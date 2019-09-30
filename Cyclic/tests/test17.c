#include <stdio.h>
#include <stdlib.h>

void main()
{
  int i;
  // Fizz buzz
  for (i=1; i <=100; i++) {
    printf("%2d ", i);
    if (!(i%3)) printf("fizz ");
    if (!(i%5)) printf("buzz");
    printf("\n");
  }
  exit(0);
}
