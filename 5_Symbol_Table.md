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
char *name= "Fred";	// SubC doesn't permit this
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
