# Expression Parsing

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
until such times as it can be confirmed to be an lvalue.

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
| !0  |  0   | Symbol  |   a  |
| !0  |  1   | Symbol  |   a  |
|  0  |  1   | Interesting! | see below |

The last line in the table is intruiging. How can a lvalue or rvalue have an
address that isn't associated with a symbol? The answer is when we have dereferenced
the symbol. Here's an example:

```
  #include <stdio.h>

  int main()
  {
  int fred[5];
    fred[2]=4;
    printf("%d\n", fred[2]);
    printf("%d\n", ++fred[2]);
    return(0);
  }
```

When we get to the assignment `fred[2]=4`, `fred` is the name of the array but
`fred[2]` is one element in this array. It has an address but that isn't the
symbol `fred`. So this is a definitely an assignment to an lvalue (which has an
address), but that address isn't associated with a symbol.

## The Abstract Syntax Tree

As part of parsing expressions, SubC builds an
[abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST)
for each expression. If you haven't seen these before, I highly recommend you read
this short explanation of ASTs:

 + [Leveling Up One’s Parsing Game With ASTs](https://medium.com/basecs/leveling-up-ones-parsing-game-with-asts-d7a6fc2400ff)
   by Vaidehi Joshi

It's well written and really help to explain the purpose and structure of ASTs.
Don't worry, I'll be here when you get back.

For this part of the SubC tour, I'm not going to concentrate on the structure of
the ASTs, but we should at least look at what each node in the AST contains
(from [src/defs.h](src/defs.h)):

```
/* AST node */
struct node_stc {
        int             op;
        struct node_stc *left, *right;
        int             args[1];
};
```

Each AST node has zero, one or two children called `left` and `right`, as well
as an operation (`op`) that links these children together. Each AST node can also have
zero, one or two `int` arguments. These can be used, for example, to hold a
literal integer value for an INTLIT token.

Leaf nodes are nodes with no children. Here the `op` doesn't represent an operation
but a characteristic of the leaf node. For example, a literal integer 27 would be
represented as the node:

```
	OP_LIT
	NULL, NULL
	27, 0
```
