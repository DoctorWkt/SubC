#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int addr;

void main()
{
  addr = 16;
  addr = (addr + 7) / 8 * 8;
  printf("addr %d\n", addr);
  exit(0);
}
