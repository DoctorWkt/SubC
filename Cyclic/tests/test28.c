#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

void foo(int y, int uniondecl, int usize, int addr) {
	Syms[y].size = uniondecl? usize: addr;
}

void main()
{
  foo(2,1,23,45);
  printf("%d\n", Syms[2].size);
  foo(2,0,23,45);
  printf("%d\n", Syms[2].size);
  exit(0);
}
