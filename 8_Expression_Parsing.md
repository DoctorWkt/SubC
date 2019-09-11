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

**XXX: Also intruiging is the second line. Explain how an lvalue can have a symbol
but no address.**

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
with a higher prioriy than the ones we are dealing with.

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
