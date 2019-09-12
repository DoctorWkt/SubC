# Expression Parsing

In this part of the tour we will see how SubC parses expressions, and builds up an
abstract syntax tree for each expression.

We've already seen parsing in action with statements and declarations, so I'm not
going to dwell on the parsing side in this part. We've also seen the functions
used to build and join the nodes in the abstract syntax trees, so I'll refer to
these as we go.

What is most important in this part is how SubC tracks and spots *semantic errors*,
where an expression is permitted by the grammar but doesn't make any practical
sense.

Examples are when we try to mix incompatible types:

```
   char x= 6000 * 12;	// Int value too big to fit into char type

   struct foo {
      int a, b;
   } fred = 12;		// Can't assign int to a struct

   if (fred) {		// Can't use fred as a boolean expression
     ...
   }
```

At the same time, we do need to permit things like automatic *casting* where types
are widened, e.g. from a `char` size to an `int size`. To do this, SubC needs to track
the type and other characteristics of each expression.

## Concepts and Data Structures

In Computer Science parlance, an *expression* is something that is or calculates a value.
Assuming that the identifiers in the following lines refer to variables, each line below
is an expression:

```
   2
   2 + 7
   a - b * 2 + c
   (a < 7) && (b != 5)
```

An expression has a *type*: what type of data is calculates, e.g. a character,
an integer, a pointer to a list of characters etc. We can also call these things
*[rvalues](https://en.wikipedia.org/wiki/R-value)*, as the values may not be
in a fixed location in memory. For example, after doing `a + 2`, the result is
likely to be in a CPU register.

On the other hand, an *[lvalue](https://en.wikipedia.org/wiki/L-value)* is a
reference to a location in memory. The terms have these names because we write
assignment statements with a left-hand and a right-hand side:

```
    lvalue := rvalue
```

We need to make this distinction because:

  + we can assign a value to an lvalue, e.g. `a= a + 1`
  + we can't assign a value to an rvalue, e.g. `2= a + 1` doesn't make sense

The C language is tricky with lvalues and rvalues for at least a couple of
reasons:

  1. We can use an lvalue (especially an assignment) as an rvalue, e.g. `b= a= 2`.
  2. It's not immediately obvious when parsing if we have found an lvalue or an
     rvalue. For example, after we have parsed `x`, this could be an rvalue
    (i.e. the value of `x`) or it could be an lvalue once we hit the following `=`
    token.

For this reason, SubC assumes that any expression is both an lvalue and an rvalue
until such times as it can be confirmed to be an lvalue. The type and "lvalue"-ness
of each expression is kept in an `lvalue` structure.

I've taken the liberty of changing the `lvalue` structure in SubC from an array of
three `int`s into a proper structure (in [src/defs.h](src/defs.h)) because the use
of the three `int`s in an array was doing my head in. Here's the new structure:

```
/* lvalue structure */
                        // Note that struct lvalue can also represent rvalues
struct lvalue {
        int sym;        // Symbol table slot number for the l/rvalue, or 0 if no symbol
        int prim;       // Primary type for the expression
        int addr;       // If true, the l/rvalue is associated with an address
};
```

An expression has a primary type `prim` and may refer to a symbol in the symbol
table with slot number `sym`. If there is an address associated with this value, `addr` is set to 1.

It might be worth putting in a table to clarify the use of `sym` and `addr`.

| sym | addr | Meaning | Example |
|:---:|:----:|:-------:|---------|
|  0  |  0   | Calculated value | 2 + 3 |
| !0  |  0   | Symbol  |   a++  |
| !0  |  1   | Symbol  |   a  |
|  0  |  1   | Interesting! | see below |

Two lines in this table are interesting. In the second line, we have a symbol but
no value. How can this be? An example is the expression `a++` which definitely refers
to the `a` variable, but we need the result (`a+1`) before `a` has been updated. So
it will have to be in a register and not in `a` itself.

The last line in the table is also interesting. How can a lvalue or rvalue have an
address that isn't associated with a symbol? The answer is when we have dereferenced
the symbol. Here's an example:

```
  #include <stdio.h>

  int main()
  {
  int fred[5];
    fred[2]=4;
    printf("%d\n", fred[2]);
    return(0);
  }
```

When we get to the assignment `fred[2]=4`, `fred` is the name of the array but
`fred[2]` is one element in this array. It has an address but that isn't the
symbol `fred`. So this is a definitely an assignment to an lvalue (which has an
address), but that address isn't associated with a symbol.

### Passing Lvalue Structures

At any point in the parsing, an AST tree which we have built has a single lvalue
structure. The type may change as we parse, but the root of the tree has a final
lvalue/rvalue.

SubC deals with this by creating a `struct lvalue lv` local variable in only a
few places. A pointer to this get passed down the recursive function call hierarchy.
At some point, a terminal (e.g. symbol or literal integer value) is found, the
initial primitive type is set, and the function returns. As the AST is built up,
the pointer to the original `struct lvalue lv` is used to update the lvalue.
Eventually this bubbles back up the call chain.

The functions in [src/expr.c](src/expr.c) that create a top-level `struct lvalue lv` are:

 + `fnargs()`: Each argument to a function is a separate expression, and
    we need to compare them against the function's prototype.
 + `postfix()` needs to deal with indexes into arrays, which are expressions
    that return a type. We need to check that this type is integer.
 + `comp_size()` calculates the size of an expression's type.
 + `binexpr()` is given the AST tree for the left-hand expression. We need to
    parse the right-hand expression, so we need a `struct lvalue lv2` for its type.
    `cond2()` and `cond3()` also have to parse a right-hand expression.
 + `asgmnt()` has an lvalue on the left-hand of the assignment operator (given to it)
    and must parse a right-hand expression.
 + `constexpr()` has to parse what should end up being a constant expression.
 + `rexpr()` is pretty much the top-level of parsing a single expression, so
    it has a `struct lvalue lv` to collect the expression's type.

Functions in other SubC source files also supply `struct lvalue lv` variables are:
 + `return_stmt()` in [src/stmt.c](src/stmt.c) checks that the returned
    expression's type matches the function's prototype.
 + Several functions in [src/stmt.c](src/stmt.c) repackage some of the arguments
   from an AST node into `struct lvalue` so they can be passed to the code
   generator functions. This really could be changed to passing two int arguments
   to the code generator functions instead of a `struct lvalue` pointer.

We will come back to the tracking of lvalues and rvalues below. But next, a look
at the grammar of expressions in SubC.

## Expression BNF Rules and Operator Precedence

The C language has an awful lot of operators and many levels
of [operator precedence](https://en.cppreference.com/w/c/language/operator_precedence).
Any parser must enforce these precedence levels. We certainly want to calculate:

```
        3 + 2 * 6  => 15
        3 + 2 * 6  => not 30
```

Even though the parser will recognise the '+' token before the '\*' token, the '\*'
token has to be performed first.

In tools that take a grammar and generate code to recognise it (e.g.
[yacc](https://en.wikipedia.org/wiki/Yacc)), there are ways to write lists
of token in order of precedence. The generated code will know to give some
tokens priority over other tokens.

Another way to enforce token priority is to write the BNF rules of the grammar
so that we are forced to descend into a non-terminal rule when there are tokens
with a higher priority than the ones we are dealing with.

As an example, here are some of the BNF rules from SubC:

```
   sum :=
          term
        | sum + term
        | sum - term

   term :=
          cast
        | term * cast
        | term / cast
        | term % cast
```

A *sum* can be a *term*, or it can be a *sum* plus a *term*.
But note that a *term* can be a *cast* (assume a number for now), or a *term* times
another *cast*.

Therefore, when we try to parse `3 + 2 * 6`, we can parse up to `3 +`, but then we
are forced down into the `term` rule where we will parse `2 * 6`. This forces the
`2 * 6` to be evaluated before the `3 +`. Thus, the '\*' operator will get higher
precedence than the '+' operator.

## The Full SubC Expression BNF Rules

To help you see how SubC does operator precedence, and to help you "see" the
grammar of SubC expressions, I'm going to give the full BNF grammar for expressions
in one hit. Take some time and make sure you can
[grok](https://en.wikipedia.org/wiki/Grok) how the rules work together. I'm going to
put the highest precedence rules at the top, so you'll have to look down to the
rules below to see how any specific rule gets invoked.

I'll put some comments in where I think something needs more explanation

```
   string :=
          STRLIT
        | STRLIT string         // Allows "abc" "def"

   primary :=                   // Primary expression, most of
          IDENT                 // which convert to terminals
        | INTLIT                // i.e tokens
        | string
        | ARGC
        | ( expr )              // Note parentheses have highest priority

   postfix :=                   // Postfix operators
          primary
        | postfix [ expr ]
        | postfix ( )
        | postfix ( fnargs )
        | postfix ++
        | postfix --
        | postfix . identifier
        | postfix -> identifier

   type :=
          INT
        | CHAR
        | VOID
        | STRUCT IDENT
        | UNION IDENT

   prefix :=                    // Prefix and unary operators
          postfix
        | ++ prefix
        | -- prefix
        | & cast
        | * cast
        | + cast
        | - cast
        | ~ cast
        | ! cast
        | SIZEOF ( type )
        | SIZEOF ( type * )
        | SIZEOF ( type * * )
        | SIZEOF ( IDENT )

   cast :=                      // Cast operators
          prefix
        | ( type ) prefix
        | ( type * ) prefix
        | ( type * * ) prefix
        | ( INT ( * ) ( ) ) prefix

   term :=                      // Multiply, divide, mod operators
          cast
        | term * cast
        | term / cast
        | term % cast
  
   sum :=                       // Plus, minus operators
          term
        | sum + term
        | sum - term
  
   shift :=                     // Shift operators
          sum
        | shift << sum
        | shift >> sum
  
   relation :=                  // Comparison operators
          shift
        | relation < shift
        | relation > shift
        | relation <= shift
        | relation >= shift
  
   equation :=                  // Equality operators
          relation
        | equation == relation
        | equation != relation
  
   binand :=                    // Bitwise operators: & | ^
          equation
        | binand & equation

   binxor :=
          binand
        | binxor ^ binand
  
   binor :=
          binxor
        | binor '|' binxor
  
   binexpr :=
          binor

   logand :=                    // Logical operators: && ||
          binexpr
        | logand && binexpr
  
   logor :=
          logand
        | logor '||' logand

   condexpr :=                  // Ternary operator ? :
          logor
        | logor ? expr : condexpr

   asgmnt :=                    // Assignments
          condexpr
        | condexpr = asgmnt
        | condexpr *= asgmnt
        | condexpr /= asgmnt
        | condexpr %= asgmnt
        | condexpr += asgmnt
        | condexpr -= asgmnt
        | condexpr <<= asgmnt
        | condexpr >>= asgmnt
        | condexpr &= asgmnt
        | condexpr ^= asgmnt
        | condexpr |= asgmnt

   expr :=                      // An expression or
          asgmnt                // a list of comma-separated expressions
        | asgmnt , expr

   fnargs :=                    // Function arguments
          asgmnt
        | asgmnt , fnargs
```

The *fnargs* rule is invoked in a different parsing context that the *expr* rule,
even though they match exactly the same grammar. We will perform different code
generation for each one.

## Looking at the SubC Expression Parsing Code

As mentioned before, I'm not going to dwell on the parsing or the symbol table side of
things. By now, you should be able to read the code and work out what is happening here:

```
                y = findsym(Text);
                copyname(name, Text);
                Token = scan();
```

What is more important is:

  + how the lvalue and rvalues are tracked for semantic analysis
  + how the expression ASTs are constructed

So I won't be pasting in whole sections of code, but instead looking at the
the code which does the above two points.

## `rvalue()`: Get the Value From a Symbol

We start with `rvalue()`. This is used to extract a value from something, and typically
ends up being converted into a load instruction assembly.

If the input node `n` is already an rvalue (as it has no address), then return that node.
Otherwise add an OP_RVAL node with `n` as the child to generate the load instruction
later on.

```
// Convert an lvalue AST node into an rvalue node. If this really is an
// lvalue, generate an OP_RVAL AST node to get the symbol's value which
// is of type prim. Otherwise, just return this node as an rvalue.
static node *rvalue(node *n, struct lvalue *lv) {
        if (lv->addr) {
                lv->addr = 0;
                return mkunop2(OP_RVAL, lv->prim, lv->sym, n);
        }
        else {
                return n;
        }
}
```

## `primary()`: Parse a Primary Expression

```
/*
 * primary :=
 *        IDENT
 *      | INTLIT
 *      | string
 *      | ARGC
 *      | ( expr )
 *
 * string :=
 *        STRLIT
 *      | STRLIT string
 */

// Given an lvalue node, parse a primary factor and return an
// AST node representing it.
static node *primary(struct lvalue *lv);
```

This is where we parse most of the expression non-terminals in the grammar and
build an AST node for them. The relevant code is:

```
static node *primary(struct lvalue *lv) {
        node    *n = NULL;
        int     y, lab, k;

        // Start with it empty
        lv->prim = lv->sym = lv->addr = 0;

        switch (Token) {
        case IDENT:
                // Find the identifier in the symbol table,
                // then copy it so we can scan the next token
		y = findsym(Text);

                // No symbol found. If the next token is a '(',
                // assume this is a function declaration. Add
                // it as an extern int function to the symbol table.
                // Otherwise, it's an undeclared variable. It's still
                // added to the symbol table as an auto int, to reduce
                // the number of further errors when the variable is used.
                if (!y) { ... }

                // Save the symbol and primary type of the symbol
                lv->sym = y;
                lv->prim = Prims[y];

                // If the next token isn't a '(', it's a function
                // pointer, so make a function pointer node
                // for the given symbol
                if (TFUNCTION == Types[y]) {
                        if (LPAREN != Token) {
                                lv->prim = FUNPTR;
                                n = mkleaf(OP_ADDR, y);
                        }
                        return n;
                }

                // Constant: make an OP_LIT node with the value
                if (TCONSTANT == Types[y]) {
                        return mkleaf(OP_LIT, Vals[y]);
                }

                // Array: make an OP_ADDR node with the
                // type being a pointer to the array's primitive type
                if (TARRAY == Types[y]) {
                        n = mkleaf(OP_ADDR, y);
                        lv->prim = pointerto(lv->prim);
                        return n;
                }

                // If it's a struct or union, make an
                // OP_ADDR node to the struct/union member
                if (comptype(Prims[y])) {
                        n = mkleaf(OP_ADDR, y);
                        lv->sym = 0;    // But not to the original
                        return n;	// struct/union itself
                }

                // An ordinary scalar variable, so make an IDENT node
                // for the symbol, and mark it as a true lvalue
                n = mkleaf(OP_IDENT, y);
                lv->addr = 1;
                return n;
        case INTLIT:
                // Literal integer, make an OP_LIT node with the value
                // and an int primary type. Move up to the next token.
                n = mkleaf(OP_LIT, Value);
                lv->prim = PINT;
                return n;
        case STRLIT:
                k = 0;
                while (STRLIT == Token) { ... } // Allow successive "x" "y"
                n = mkleaf(OP_LDLAB, lab);      // Node refers to the label
                lv->prim = CHARPTR;             // and is a char pointer
                                                // Not marked as addr=1
                                                // as the string is mutable
                return n;
        case LPAREN:
                n = exprlist(lv, 0);            // Parse the expression list
                return n;                       // and return the node
        default:
                // It's a syntax error, skip tokens up to the next semicolon
                error("syntax error at: %s", Text);
                Token = synch(SEMI);
                return NULL;
        }
}
```

I've left the use of `synch(SEMI)` in at the end to show you how we can minimise
the avalanche of likely error reporting once we hit a syntax error. Here, we will
just skip everything up to the next semicolon and resume parsing there.

Note that most of the cases produce a leaf node in the AST with `mkleaf()` which
returns a pointer to the new node. `primary()` itself receives a pointer to an
*lvalue* struct, so it can return the type of the expression in the AST tree that
it returns.
