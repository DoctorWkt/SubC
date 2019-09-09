# Symbol Table Management

Before we get to the next parsing topic, declaration parsing,
we should first look at how the details of the declarations
will get stored.

SubC has a symbol table that records this information. Now, SubC was written
before it could handle structures, and so each entry for a single
symbol is actually spread out across several arrays (in [src/data.h](src/data.h)):

```
#define NSYMBOLS        1024

extern char   *Names[NSYMBOLS];        // Name of a symbol
extern int     Prims[NSYMBOLS];        // Primitive type of a symbol
extern char    Types[NSYMBOLS];        // Meta type of a symbol
extern char    Stcls[NSYMBOLS];        // Storage class of a symbol
extern int     Sizes[NSYMBOLS];        // Number of elements in the symbol
extern int     Vals[NSYMBOLS];         // Initial value of the symbol
extern char   *Mtext[NSYMBOLS];        // The definition of a macro
```

An index, `i=2` for example, can be used across all arrays to get the details of symbol 2.
Now that SubC supports structures, this could be rewritten as an array of structures.

Each array holds one attribute of each symbol:

| Array | Holds |
|:-----:|:------|
| Names | The name of a symbol |
| Prims | The symbol's primitive type |
| Types | The *meta* type of a symbol |
| Stcls | The symbol's storage class |
| Sizes | The number of elements in the symbol |
| Vals  | The initial value of the symbol |
| Mtext | The definition of a symbol which is a pre-processor macro |

## Meta Types and Primitive Types

The `Types[]` list holds what sort of symbol is at this position in the arrays
(in [src/defs.h](src/defs.h)), one of:

```
enum {
        TVARIABLE = 1,          // Variable
        TARRAY,                 // Array
        TFUNCTION,              // Function
        TCONSTANT,              // Literal constant
        TMACRO,                 // Pre-processor macro
        TSTRUCT                 // Struct or union
};
```

The `Prims[]` list holds the type of this symbol (in [src/defs.h](src/defs.h)), one of:

```
enum {
        PCHAR = 1,              // unsigned char
        PINT,                   // signed int
        CHARPTR,                // char pointer
        INTPTR,                 // int pointer
        CHARPP,                 // char pointer pointer
        INTPP,                  // int pointer pointer
        PVOID,                  // void
        VOIDPTR,                // void pointer
        VOIDPP,                 // void pointer pointer
        FUNPTR,                 // function pointer
        PSTRUCT = 0x2000,       // struct
        PUNION  = 0x4000,       // union
        STCPTR  = 0x6000,       // struct pointer
        STCPP   = 0x8000,       // struct pointer pointer
        UNIPTR  = 0xA000,       // union pointer
        UNIPP   = 0xC000,       // union pointer pointer
        STCMASK = 0xE000        // mask for struct/union bits
};
```

Note two things. Firstly, SubC doesn't allow more than two levels of pointer indirection, whereas
the C language allows arbitrary levels of pointer indirection. Secondly, the values for structs
and unions are a set of bit values completely separate from the non-struct/union bits. I think
this is done to allow the storage of of a field's type within a struct/union as well as its membership
within the struct/union.

## Storage Classes

C allows a symbol to exist in one of several
[storage classes](http://cs-fundamentals.com/c-programming/storage-classes-in-c-and-storage-class-specifiers.php). This affects the symbol's scope and location (e.g. in initialised or unitialised global
storage or on the stack).

SubC recognises these storage classes (in [src/defs.h](src/defs.h)):

```
enum {
        CPUBLIC = 1,            // publicly visible symbol
        CEXTERN,                // extern symbol
        CSTATIC,                // static symbols in global context
        CLSTATC,                // static symbols in local context
        CAUTO,                  // non-static local identifiers
        CSPROTO,                // function prototype
        CMEMBER,                // field of a struct/union
        CSTCDEF                 // unused
};
```

A storage class can be promoted to another storage class. For example, a function prototype will
start as CSPROTO but then may turn into a static function (CSTATIC) or a public function
(CPUBLIC) when it is properly declared. Similarly, a variable may be initially declared `extern`
(CEXTERN) but later on declared fully and become CPUBLIC.

## Sizes, Initial Values and Macro Definitions

For these, it's probably easier to get SubC to show you what it stores in the symbol table.
SubC can dump its symbol tables with a command-line option, so I'll show you some examples
of declarations and how they are stored in the symbol table.

SubC actually has two symbol tables:

  + global symbols are visible across all functions
  + local variables are visible within a single function

### Unitialised Global Variables

We'll start with some global variable declarations.
These will be followed by the output of `subc -t -d gsym ...`:

```
int x;
char ch;
int *xptr;
char **cpp;
void *FILE;
void (*funcptr)();

PRIM    TYPE  STCLS   SIZE  VALUE  NAME [MVAL]/(SIG)
------  ----  -----  -----  -----  -----------------
INT     VAR   PUBLC      0      0  x
CHAR    VAR   PUBLC      0      0  ch
INT*    VAR   PUBLC      0      0  xptr
CHAR**  VAR   PUBLC      0      0  cpp
VOID*   VAR   PUBLC      0      0  FILE
FUN*    VAR   PUBLC      0      0  funcptr
```

### Initialised Global Variables

Now some glocal variables with initial values:

```
int x= 2;
char ch= 'A';
int *xptr= &x;		// SubC doesn't permit this
char *cptr= &ch;	// SubC doesn't permit this
char **cpp= &cptr;	// SubC doesn't permit this

PRIM    TYPE  STCLS   SIZE  VALUE  NAME [MVAL]/(SIG)
------  ----  -----  -----  -----  -----------------
INT     VAR   PUBLC      0      2  x
CHAR    VAR   PUBLC      0     65  ch
INT*    VAR   PUBLC      0      0  xptr
CHAR*   VAR   PUBLC      0      0  cptr
CHAR**  VAR   PUBLC      0      0  cpp
```

SubC seems to only allow global integers to be initialised. Let's now
move on to some arrays, structures and unions.

### Global Arrays, Structs and Unions

```
int list[100];
char *namelist[50];

struct customer {
	char *name;
	int age;
	char gender;
};

union jack {
	int size;
	char colour;
	struct customer *human;
};

PRIM    TYPE  STCLS   SIZE  VALUE  NAME [MVAL]/(SIG)
------  ----  -----  -----  -----  -----------------
INT     ARRY  PUBLC    100      0  list
CHAR*   ARRY  PUBLC     50      0  namelist
STRUCT  STCT  MEMBR     24      0  customer
CHAR*   VAR   MEMBR      0      0  name
INT     VAR   MEMBR      0      8  age
CHAR    VAR   MEMBR      0     16  gender
UNION   STCT  MEMBR      8      0  jack
INT     VAR   MEMBR      0      0  size
CHAR    VAR   MEMBR      0      0  colour
STCT*   VAR   MEMBR      0      0  human
```

For arrays, the size is the number of elements not the number of bytes
occupied. But for structs and unions, the size *is* the number of
bytes occupied: I compiled the above on an x64 platform. For fields
which are members of structs/unions, the value is the offset of
the field (in bytes) within the struct/union.

### Functions

Let's try some functions out:

```
char       *malloc(int size);
extern void exit(int how);
extern int  strcmp(char *a, char *b);
extern int  strncmp(char *s1, char *s2, int n);
int         main() { exit(0); }
static int  increment() { int x=1; return(x++); }

PRIM    TYPE  STCLS   SIZE  VALUE  NAME [MVAL]/(SIG)
------  ----  -----  -----  -----  -----------------
CHAR*   FUN   EXTRN      1      0  malloc (INT)
VOID    FUN   EXTRN      1      0  exit (INT)
INT     FUN   EXTRN      2      0  strcmp (CHAR*)
INT     FUN   EXTRN      3      0  strncmp (CHAR*)
INT     FUN   PUBLC      0      0  main ()
INT     FUN   STATC      0      0  increment ()
```

For functions, the size is the number of arguments. The `Mtext[]` stores
details of the parameters to the function.

### Local Variables

Finally, some local variables:

void main()
{
  static int x;
  int y;
  char list[20];
  char *name;
}
```
$ scc -t -d lsym z.c

===== LOCALS: main =====
PRIM    TYPE  STCLS   SIZE  VALUE  NAME [MVAL]/(SIG)
------  ----  -----  -----  -----  -----------------
CHAR*   VAR   AUTO       1    -40  name
CHAR    ARRY  AUTO      20    -32  list
INT     VAR   AUTO       1     -8  y
INT     VAR   LSTAT      1      1  x
```

The `x` variable is `static`, so it won't be put on the stack when
`main()` gets called. I'm not sure why it has size 1 and value 1.

The other variables are placed on the stack once `main()` is called,
and their value reflects their position in relation to the stack pointer.
I'm guessing that "size 1"s reflect that there is just one value. The
`list[]` is size 20, of course.

## Allocating in the Symbol Table

SubC needs two sybmol tables, one for global symbols and another for
local variables. Both tables live in the `Names[]` to `Mtext[]` arrays:
the globals are at one end and the locals are at the other:

```
				// in src/data.h
extern int     Globs;           // Pointer to next empty global slot
extern int     Locs;            // Pointer to next empty local slot

init() {			// in src/misc.c
	...
        Globs = 0;
        Locs = NSYMBOLS;
	...
}
```

These two functions in [src/sym.c](src/sym.c) allocate slots in both sections:

```
// Get the position of a new global symbol slot, or die
// if we've hit the local section of the symbol table.
int newglob(void) {
        int     p;

        if ((p = Globs++) >= Locs)
                fatal("too many global symbols");
        return p;
}

// Get the position of a new local symbol slot, or die
// if we've hit the global section of the symbol table.
int newloc(void) {
        int     p;

        if ((p = --Locs) <= Globs)
                fatal("too many local symbols");
        return p;
}
```

### Allocating a Global Symbol

Essentially, `addglob()` obtains a global symbol table slot using
`newglob()` and then fills in the symbol table arrays with values,
except that there is a lot of error checking to do:

```
// Add a global symbol to the symbol table.
//   name: Name of the symbol
//   prim: Primitive type of the symbol
//   type: Meta type of the symbol
//   scls: Storage class of the symbol
//   size: Number of elements in the symbol
//   val:  Initial value of the symbol
//   mtext: Text of a macro, or a function signature (list of parameters)
//   init: If true, symbol has an initial value
// Return the slot number in the symbol table
int addglob(char *name, int prim, int type, int scls, int size, int val,
                char *mtext, int init)
{
        int     y;

        // If this is already in the symbol table
        if ((y = findglob(name)) != 0) {

                // Redeclare the symbol with a new storage class.
                // If a function, keep the old function signature.
                scls = redeclare(name, Stcls[y], scls);
                if (TFUNCTION == Types[y])
                        mtext = Mtext[y];
        }

        // Doesn't exist, so get a new global symbol slot
        // and copy the symbol name into the symbol table
        if (0 == y) {
                y = newglob();
                Names[y] = globname(name);
        }

        // Check that, if this is a redefinition of a function or macro,
        // that the redefinition has the same type as before
        else if (TFUNCTION == Types[y] || TMACRO == Types[y]) {
                if (Prims[y] != prim || Types[y] != type)
                        error("redefinition does not match prior type: %s",
                                name);
        }

        // If this is a public or static declaration, generate
        // the storage in the assembly output now
        if (CPUBLIC == scls || CSTATIC == scls)
                defglob(name, prim, type, size, val, scls, init);

        // Copy the values into the symbol table
        // and return the slot
        Prims[y] = prim;
        Types[y] = type;
        Stcls[y] = scls;
        Sizes[y] = size;
        Vals[y] = val;
        Mtext[y] = mtext;
        return y;
}
```

Note the call to `defglob()` if the symbol has a public or static class.
We know all the details of the symbol, so we can generate the assembly
code to define it in the `.data` section of the output.

### Allocating a Local Symbol

This is done in `addloc()` which has the same parameters
as `addglob()`, and does a similar set of error checking.

```
// Add a local symbol to the symbol table.
//   name: Name of the symbol
//   prim: Primitive type of the symbol
//   type: Meta type of the symbol
//   scls: Storage class of the symbol
//   size: Number of elements in the symbol
//   val:  Initial value of the symbol
//   init: If true, symbol has an initial value
// Return the slot number in the symbol table
int addloc(char *name, int prim, int type, int scls, int size, int val,
                int init)
{
        int     y;

        // Ensure the local hasn't already been defined
        if (findloc(name))
                error("redefinition of: %s", name);

        // Get a new local slot in the symbol table
        y = newloc();

        // If this is a static declaration, generate the
        // storage in the assembly output now

        if (CLSTATC == scls) defloc(prim, type, size, val, init);

        // Copy the values into the symbol table
        // and return the slot
        Names[y] = locname(name);
        Prims[y] = prim;
        Types[y] = type;
        Stcls[y] = scls;
        Sizes[y] = size;
        Vals[y] = val;
        return y;
}
```

But there's a wrinkle. The local symbol is either "auto" and stored on
the stack, or it's "static".

If a symbol is static and has no specified value, it needs to be stored in
the [bss section](https://en.wikipedia.org/wiki/.bss) of the assembly
output and initialised to zero.
If a symbol is static and does have a value, we need to output the
assembly to define this value. This is all done with `defloc()`:

```
// Generate the assembly output for a local static symbol.
// Note: local auto symbols are allocated on the stack.
//   prim: Primitive type of the symbol
//   type: Meta type of the symbol
//   size: Number of elements in the symbol
//   val:  Initial value of the symbol
//   init: If true, symbol has an initial value
static void defloc(int prim, int type, int size, int val, int init) {

        // Switch to the data section
        gendata();

        // Generate a label with the value if not an array or struct/union
        // Use the val value to make the label unique. XXX: more details.
        if (type != TARRAY && !(prim &STCMASK)) genlab(val);

        // If a struct/union, generate bss storage with the object's size.
        // Deal with the situation when it's an array of struct/unions.
        if (prim & STCMASK) {
                if (TARRAY == type)
                        genbss(labname(val), objsize(prim, TARRAY, size), 1);
                else
                        genbss(labname(val), objsize(prim, TVARIABLE, size),1);
        }

        else if (PCHAR == prim) {
                // If a char array, generate bss storage with the size.
                // Otherwise generate a byte and then force alignment afterwards
                if (TARRAY == type)
                        genbss(labname(val), size, 1);
                else {
                        gendefb(init);
                        genalign(1);
                }
        }
        else if (PINT == prim) {
                // If a char array, generate bss storage with the size.
                // Otherwise generate a word which needs no further alignment
                if (TARRAY == type)
                        genbss(labname(val), size*INTSIZE, 1);
                else
                        gendefw(init);
        }
        else {
                // If a pointer array, generate bss storage with the size.
                // Otherwise generate a pointer which needs no further alignment
                if (TARRAY == type)
                        genbss(labname(val), size*PTRSIZE, 1);
                else
                        gendefp(init);
        }
}
```

The calls to `genbss()` indicate bss storage. The calls to `gendefb()`,
`gendefw()` and `gendefp()` indicate the creation of byte, word and
pointer values.

There are calls out to `objsize()` to get the size of variables and
arrays. Here's the code that does this:

```
// Given a primitive type, return the size of this type.
// If type is TARRAY, multiply the returned value by size.
// Return -1 if the type has no size: function, constant, macro.
int objsize(int prim, int type, int size) {
        int     k = 0, sp;

        // Keep only the struct/union bits in sp
        sp = prim & STCMASK;
        if (PINT == prim)
                k = INTSIZE;
        else if (PCHAR == prim)
                k = CHARSIZE;
        else if (INTPTR == prim || CHARPTR == prim || VOIDPTR == prim)
                k = PTRSIZE;
        else if (INTPP == prim || CHARPP == prim || VOIDPP == prim)
                k = PTRSIZE;
        else if (STCPTR == sp || STCPP == sp)
                k = PTRSIZE;
        else if (UNIPTR == sp || UNIPP == sp)
                k = PTRSIZE;
        else if (PSTRUCT == sp || PUNION == sp)
                k = Sizes[prim & ~STCMASK];
        else if (FUNPTR == prim)
                k = PTRSIZE;
        if (TFUNCTION == type || TCONSTANT == type || TMACRO == type)
                return -1;
        if (TARRAY == type)
                k *= size;
        return k;
}
```

## Redeclaring a Symbol

The `redeclare()` function is used to change
the storage class of an existing symbol. I've already mentioned a couple
of use case for this:

 + a function prototype will start as CSPROTO but then may turn into
   a static function (CSTATIC) or a public function (CPUBLIC) when it
   is properly declared.
 + a variable may be initially declared `extern` (CEXTERN) but later on
   declared fully and become CPUBLIC.

The code is complicated due to the wide variety of possibilities as
well as the need to do error checking on the requested storage class change.

```
// Redeclare a symbol to have a new class.
// Return the new class of the symbol.
// Here is what's permitted (apart from when oldcls == newcls):
//  
//    CEXTERN -> CPUBLIC
//    CPUBLIC -> CEXTERN, stays as CPUBLIC
//    CSPROTO -> CSTATIC
//    CSTATIC -> CSPROTO, stays as CSTATIC
//
int redeclare(char *name, int oldcls, int newcls) {
        switch (oldcls) {

        // Cannot redeclare an extern symbol as static
        case CEXTERN:
                if (newcls != CPUBLIC && newcls != CEXTERN)
                        error("extern symbol redeclared static: %s", name);
                return newcls;

        case CPUBLIC:
                if (CEXTERN == newcls)
                        return CPUBLIC;
                if (newcls != CPUBLIC) {
                        error("extern symbol redeclared static: %s", name);
                        return CPUBLIC;
                }
                break;
        case CSPROTO:
                if (newcls != CSTATIC && newcls != CSPROTO)
                        error("static symbol redeclared extern: %s", name);
                return newcls;
        case CSTATIC:
                if (CSPROTO == newcls)
                        return CSTATIC;
                if (newcls != CSTATIC) {
                        error("static symbol redeclared extern: %s", name);
                        return CSTATIC;
                }
                break;
        }
        error("redefined symbol: %s", name);
        return newcls;
}
```

## XXX Finding Symbols: to do

## Conclusion

The parser in SubC interacts with the symbol table each time a new
variable, function, structure, union or pre-processor macro is declared.
Each symbol has a number of attributes, and is also seen as either
globally visible or locally visible.

[src/sym.c](src/sym.c) deals with

 + the allocation of symbol tables into the symbol table
 + the generation of assembly code for global and static variables
 + the redeclaration of symbols
 + finding existing symbols in the symbol table
