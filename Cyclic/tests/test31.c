#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

#ifdef __SUBC__
#define INT int
#else
#define INT long
#endif

INT *p;

void main() {
  p= (INT *)0x1000;
  p += 7;
#ifdef __SUBC__
  printf("2p %x\n", (INT)p);
#else
  printf("2p %lx\n", (INT)p);
#endif
  exit(0);
}
