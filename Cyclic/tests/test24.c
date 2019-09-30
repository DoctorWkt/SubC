#include <stdio.h>
#include <stdlib.h>

#define NSYMBOLS	1024

struct symtable {
        char    *name;          // Name of a symbol
        int     prim;           // Primitive type of a symbol
        char    type;           // Meta type of a symbol
        char    stcls;          // Storage class of a symbol
        int     size;           // Number of elements in the symbol
        int     val;            // Initial value of the symbol
        char    *mtext;         // The definition of a macro
};

struct symtable Syms[NSYMBOLS];
struct symtable *sptr;
int i;

void main() {

  i=2;
  sptr= &Syms[i];
  sptr->prim= 56;
  printf("%d\n", Syms[i].prim);
  exit(0);
}
