#include <stdio.h>
#include <stdlib.h>

static int      Opt_sum_lim;

void opt_init(void) {
        int     i, k;

        Opt_sum_lim = 1;
        for (i=1; i<31; i++)
                Opt_sum_lim <<= 1;
}

void main() {
  opt_init();
  printf("%x\n", Opt_sum_lim);
  exit(0);
}
