#include <stdio.h>
#include <stdlib.h>

struct zfile {
        int     fd;
        char    iom;
        char    last;
        char    mode;
        int     ptr;
        int     end;
        int     size;
        int     ch;
        char    *buf;
};

struct zfile z;
struct zfile *zptr;
int i;

void main() {
  char ch;

  zptr= &z;
  zptr->buf="Hello world\n";
  zptr->ptr=4;

  ch= zptr->buf[ zptr->ptr ]; printf("%d\n", ch);
  i=12; ch= zptr->buf[ zptr->ptr++ ]; i=13; printf("%d\n", ch);
  ch= z.buf[ z.ptr ]; printf("%d\n", ch);
  ch= z.buf[ z.ptr++ ]; printf("%d\n", ch);

  // These are OK
  printf("%d\n", zptr->ptr);
  printf("%d\n", zptr->ptr++);
  printf("%d\n", zptr->ptr);
  printf("%d\n", zptr->ptr++);
  printf("%d\n", ++zptr->ptr);
  printf("%d\n", ++zptr->ptr);
  
  exit(0);
}
