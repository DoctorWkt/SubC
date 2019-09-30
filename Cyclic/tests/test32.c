#include <stdio.h>
#include <stdlib.h>

int a;

struct x {
  int val;
};

struct x s;
struct x *sptr;

void main() {
  s.val=5;
  sptr= &s;
  a= sptr->val++;
  printf("%d\n", a);
  a=  ++sptr->val;
  printf("%d\n", a);
  exit(0);
}
