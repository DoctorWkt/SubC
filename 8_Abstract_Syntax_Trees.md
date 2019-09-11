# Abstract Syntax Trees

As part of parsing expressions, SubC builds an
[abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST)
for each expression. If you haven't seen these before, I highly recommend you read
this short explanation of ASTs:

 + [Leveling Up One’s Parsing Game With ASTs](https://medium.com/basecs/leveling-up-ones-parsing-game-with-asts-d7a6fc2400ff)
   by Vaidehi Joshi

It's well written and really help to explain the purpose and structure of ASTs.
Don't worry, I'll be here when you get back.

The structure of each node in the ASTs is
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

## Node Operations

A node in an AST can be a binary operation on the two child nodes,
a unary operation on the left child node, or just a characteristic
of the node itself (for leaf nodes).

[src/defs.h](src/defs.h) has the list of node operators:

```
/* AST operators */
enum {
        OP_GLUE, OP_ADD, OP_ADDR, OP_ASSIGN, OP_BINAND, OP_BINIOR,
        OP_BINXOR, OP_BOOL, OP_BRFALSE, OP_BRTRUE, OP_CALL, OP_CALR,
        OP_COMMA, OP_DEC, OP_DIV, OP_EQUAL, OP_GREATER, OP_GTEQ,
        OP_IDENT, OP_IFELSE, OP_LAB, OP_LDLAB, OP_LESS, OP_LIT,
        OP_LOGNOT, OP_LSHIFT, OP_LTEQ, OP_MOD, OP_MUL, OP_NEG,
        OP_NOT, OP_NOTEQ, OP_PLUS, OP_PREDEC, OP_PREINC, OP_POSTDEC,
        OP_POSTINC, OP_RSHIFT, OP_RVAL, OP_SCALE, OP_SCALEBY, OP_SUB
};
```

Here is a table that describes each one

| Operator | Use | # Args | Meaning |
|----------|-----|--------|---------|
OP_ADD		| Binary	| 0, 1		| Add two sub-trees ? |
OP_ADDR		| Unary, Leaf	| 1		| Get the address of a symbol |
OP_ASSIGN	| Binary	| 2		| Assign the right-hand sub-tree value to the left-hand sub-tree |
OP_BINAND	| Binary	| 0, 1, 2	| Bitwise AND two sub-trees |
OP_BINIOR	| Binary	| 0, 1, 2	| Bitwise OR two sub-trees |
OP_BINXOR	| Binary	| 0, 1, 2	| Bitwise XOR two sub-trees |
OP_BOOL		| Unary		| 0		| Convert to boolean value (1 or 0) |
OP_BRFALSE	| Binary	| 1		| Branch to a label if sub-tree condition is false |
OP_BRTRUE	| Binary	| 1		| Branch to a label if sub-tree condition is true |
OP_CALL		| Unary		| 2		| Call the child sub-tree |
OP_CALR		| Unary		| 2		| Call the function pointer in the child sub-tree |
OP_COMMA	| Binary	| 0		| Child sub-trees are separated by a comma |
OP_DEC		| Unused	| 0, 1, 2	| Not used |
OP_DIV		| Binary	| 0, 1, 2	| Divide two sub-trees |
OP_EQUAL	| Binary	| 0, 1, 2	| Determine if two sub-trees have equal value |
OP_GLUE		| Binary	| 0		| Glue two adjacent sub-tree values, e.g. function parameters ?? XXX |
OP_GREATER	| Binary	| 0, 1, 2	| Test if left sub-tree value > right sub-tree value |
OP_GTEQ		| Binary	| 0, 1, 2	| Test if left sub-tree value >= right sub-tree value |
OP_IDENT	| Leaf		| 1		| Hold an identifier |
OP_IFELSE	| Unary		| 1		| Deal with C ? : expressions, more details here XXX |
OP_LAB		| Unary		| 1		| Hold a label |
OP_LDLAB	| Leaf		| 1		| Load a pointer to a label |
OP_LESS		| Binary	| 0, 1, 2	| Test if left sub-tree value < right sub-tree value |
OP_LIT		| Leaf		| 1		| Hold a literal value |
OP_LOGNOT	| Unary		| 0		| Invert the truth of a sub-tree |
OP_LSHIFT	| Binary	| 0		| Left-shift left sub-tree value by right sub-tree |
OP_LTEQ		| Binary	| 0		| Test if left sub-tree value <= right sub-tree value |
OP_MOD		| Binary	| 0, 1, 2	| Perform modulo on two sub-trees |
OP_MUL		| Binary	| 0, 1, 2	| Multiply two sub-trees |
OP_NEG		| Unary		| 0		| Negate a sub-tree |
OP_NOT		| Unary		| 0		| Bitwise NOT a sub-tree |
OP_NOTEQ	| Binary	| 0, 1, 2	| Determine if two sub-trees don't have equal value |
OP_PLUS		| Binary	| 0, 1, 2	| Add two sub-trees |
OP_POSTDEC	| Unary		| 2		| Post-decrement the sub-tree |
OP_POSTINC	| Unary		| 2		| Post-increment the sub-tree |
OP_PREDEC	| Unary		| 2		| Pre-decrement the sub-tree |
OP_PREINC	| Unary		| 2		| Pre-increment the sub-tree |
OP_RSHIFT	| Binary	| 0, 1, 2	| Right-shift left sub-tree value by right sub-tree |
OP_RVAL		| Unary		| 2		| Get rvalue of symbol & primitive type |
OP_SCALE	| Unary		| 0		| Scale by the sub-tree's value (e.g. array indexing) |
OP_SCALEBY	| Unary		| 1		| Scale by the sub-tree's value (e.g. array indexing) |
OP_SUB		| Binary	| 0, 1, 2	| Subtract two sub-trees |

XXX Comments here on the arguments, what they can represent

## Example ASTs

SubC can output the ASTs of the expressions it parses with the `-T` command-line flag. Here are some
example expressions and the generated ASTs as printed by SubC.

```
  int a;
  a=2;		// Assign a with value

x=y 2 1023	// OP_ASSIGN
  `-id a	// OP_IDENT
  `-lit 2	// OP_LIT
```

<hr>

```
  int a;
  a= a + 2;

x=y 2 1023
  `-id a
  `-x+y 2 2	// OP_PLUS
    `- *x 2 a	// Get value from a (OP_ADDR)
    | `-id a
    `-lit 2
```

<hr>

```
  int a;
  int *b;
  a= *b;

x=y 2 1023	// Assign sub-tree value to a
  `-id a
  `- *x 2 	// Dereference the ptr value (OP_ADDR)
    `- *x 4 b	// Get ptr value from b (OP_ADDR)
      `-id b
```

<hr>

```
  int a, b;
  if (a == b)
    printf("Hello world\n");

x==y
  `- *x 2 a
  | `-id a
  `- *x 2 b
    `-id b

 printf() 1
  `-glue
    `-ldlab L4  int a, b;
  if (a == b)
    printf("Hello world\n");

x==y			// Compare sub-trees (OP_EQUAL)
  `- *x 2 a		// Value at a
  | `-id a
  `- *x 2 b		// Value at b
    `-id b

 printf() 1		// Call function (OP_CALL)
  `-glue		// OP_GLUE
    `-ldlab L4		// with value at L4 (OP_LDLAB)
```

<hr>

```
void main()
{
  int a, b;
  a= (b > 2) ? b++ : --b;

x=y 2 1023		// Assign to a
  `-id a
  `-?: 3		// result of ternary operation (OP_IFELSE)
    `-glue
      `-jump/false 2	// Jump if expression false (OP_BRFALSE)
      | `-x>y		// a > comparison between (OP_GREATER)
      | | `- *x 2 b	// b's value and literal 2
      | | | `-id b
      | | `-lit 2
      | `-x++ 2 1022	// b++'s value (OP_POSTINC)
      |   `-id b
      `---x 2 1022	// --b's value (OP_PREDEC)
        `-id b
```

<hr>

```
  int a,b;
  int list[4];
  a= list[2];
  a= list[b];

x=y 2 1023		// Assign to a
  `-id a
  `- *x 2 		// the value at
    `-x+y (int,int)	// (OP_ADD, not OP_PLUS)
      `-addr list	// address of list (OP_ADDR)
      `-scale		// (OP_SCALE)
        `-lit 2		// plus 2

x=y 2 1023		// Assign to a
  `-id a
  `- *x 2 		// the value at
    `-x+y (int,int)	// (OP_ADD)
      `-addr list	// address of list (OP_ADDR)
      `-scale		// (OP_SCALEBY)
        `- *x 2 b	// plus value at b
          `-id b
```
