# Abstract Syntax Trees

Before we can cover expression parsing, we first need to take a detour into
abstract syntax trees. As part of parsing expressions, SubC builds an
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
OP_ADD          | Binary        | 0, 1          | Add the offset from the right-hand tree to the left-hand tree |
OP_ADDR         | Unary, Leaf   | 1             | Get the address of a symbol |
OP_ASSIGN       | Binary        | 2             | Assign the right-hand sub-tree value to the left-hand sub-tree |
OP_BINAND       | Binary        | 0, 1, 2       | Bitwise AND two sub-trees |
OP_BINIOR       | Binary        | 0, 1, 2       | Bitwise OR two sub-trees |
OP_BINXOR       | Binary        | 0, 1, 2       | Bitwise XOR two sub-trees |
OP_BOOL         | Unary         | 0             | Convert to boolean value (1 or 0) |
OP_BRFALSE      | Binary        | 1             | Branch to a label if sub-tree condition is false |
OP_BRTRUE       | Binary        | 1             | Branch to a label if sub-tree condition is true |
OP_CALL         | Unary         | 2             | Call the child sub-tree |
OP_CALR         | Unary         | 2             | Call the function pointer in the child sub-tree |
OP_COMMA        | Binary        | 0             | Child sub-trees are separated by a comma |
OP_DEC          | Unused        | 0, 1, 2       | Not used |
OP_DIV          | Binary        | 0, 1, 2       | Divide two sub-trees |
OP_EQUAL        | Binary        | 0, 1, 2       | Determine if two sub-trees have equal value |
OP_GLUE         | Binary        | 0             | Glue two adjacent sub-tree values, e.g. function parameters ?? XXX |
OP_GREATER      | Binary        | 0, 1, 2       | Test if left sub-tree value > right sub-tree value |
OP_GTEQ         | Binary        | 0, 1, 2       | Test if left sub-tree value >= right sub-tree value |
OP_IDENT        | Leaf          | 1             | Hold an identifier |
OP_IFELSE       | Unary         | 1             | Deal with C ? : expressions, more details here XXX |
OP_LAB          | Unary         | 1             | Hold a label |
OP_LDLAB        | Leaf          | 1             | Load a pointer to a label |
OP_LESS         | Binary        | 0, 1, 2       | Test if left sub-tree value < right sub-tree value |
OP_LIT          | Leaf          | 1             | Hold a literal value |
OP_LOGNOT       | Unary         | 0             | Invert the truth of a sub-tree |
OP_LSHIFT       | Binary        | 0             | Left-shift left sub-tree value by right sub-tree |
OP_LTEQ         | Binary        | 0             | Test if left sub-tree value <= right sub-tree value |
OP_MOD          | Binary        | 0, 1, 2       | Perform modulo on two sub-trees |
OP_MUL          | Binary        | 0, 1, 2       | Multiply two sub-trees |
OP_NEG          | Unary         | 0             | Negate a sub-tree |
OP_NOT          | Unary         | 0             | Bitwise NOT a sub-tree |
OP_NOTEQ        | Binary        | 0, 1, 2       | Determine if two sub-trees don't have equal value |
OP_PLUS         | Binary        | 0, 1, 2       | Add two sub-trees |
OP_POSTDEC      | Unary         | 2             | Post-decrement the sub-tree |
OP_POSTINC      | Unary         | 2             | Post-increment the sub-tree |
OP_PREDEC       | Unary         | 2             | Pre-decrement the sub-tree |
OP_PREINC       | Unary         | 2             | Pre-increment the sub-tree |
OP_RSHIFT       | Binary        | 0, 1, 2       | Right-shift left sub-tree value by right sub-tree |
OP_RVAL         | Unary         | 2             | Get rvalue of symbol & primitive type |
OP_SCALE        | Unary         | 0             | Scale by the sub-tree's value (e.g. array indexing) |
OP_SCALEBY      | Unary         | 1             | Scale by the sub-tree's value (e.g. array indexing) |
OP_SUB          | Binary        | 0, 1, 2       | Subtract two sub-trees |

XXX Comments here on the arguments, what they can represent

## Example ASTs

SubC can output the ASTs of the expressions it parses with the `-T` command-line flag. Here are some
example expressions and the generated ASTs as printed by SubC.

```
  int a;
  a=2;          // Assign a with value

x=y 2 1023      // OP_ASSIGN
  `-id a        // OP_IDENT
  `-lit 2       // OP_LIT
```

<hr>

```
  int a;
  a= a + 2;

x=y 2 1023
  `-id a
  `-x+y 2 2     // OP_PLUS
    `- *x 2 a   // Get value from a (OP_ADDR)
    | `-id a
    `-lit 2
```

<hr>

```
  int a;
  int *b;
  a= *b;

x=y 2 1023      // Assign sub-tree value to a
  `-id a
  `- *x 2       // Dereference the ptr value (OP_ADDR)
    `- *x 4 b   // Get ptr value from b (OP_ADDR)
      `-id b
```

<hr>

```
  int a, b;
  if (a == b)
    printf("Hello world\n");

x==y                    // Compare sub-trees (OP_EQUAL)
  `- *x 2 a             // Value at a
  | `-id a
  `- *x 2 b             // Value at b
    `-id b

 printf() 1             // Call function (OP_CALL)
  `-glue                // OP_GLUE
    `-ldlab L4          // with value at L4 (OP_LDLAB)
```

<hr>

```
  int a, b;
  a= (b > 2) ? b++ : --b;

x=y 2 1023              // Assign to a
  `-id a
  `-?: 3                // result of ternary operation (OP_IFELSE)
    `-glue
      `-jump/false 2    // Jump if expression false (OP_BRFALSE)
      | `-x>y           // a > comparison between (OP_GREATER)
      | | `- *x 2 b     // b's value and literal 2
      | | | `-id b
      | | `-lit 2
      | `-x++ 2 1022    // b++'s value (OP_POSTINC)
      |   `-id b
      `---x 2 1022      // --b's value (OP_PREDEC)
        `-id b
```

<hr>

```
  int a,b;
  int list[4];
  a= list[2];
  a= list[b];

x=y 2 1023              // Assign to a
  `-id a
  `- *x 2               // the value at
    `-x+y (int,int)     // (OP_ADD, not OP_PLUS)
      `-addr list       // address of list (OP_ADDR)
      `-scale           // (OP_SCALE)
        `-lit 2         // plus 2

x=y 2 1023              // Assign to a
  `-id a
  `- *x 2               // the value at
    `-x+y (int,int)     // (OP_ADD)
      `-addr list       // address of list (OP_ADDR)
      `-scale           // (OP_SCALEBY)
        `- *x 2 b       // plus value at b
          `-id b
```

## Building the Tree, Traversing the Tree

Each AST tree represents an expression from the input C files. The functions
in [src/expr.c](src/expr.c) build a tree as the input is parsed. Once the
expression is complete, the AST is passed to [src/gen.c](src/gen.c) to
generate the assembly output code for the expression. In the middle are the
tree-related functions in [src/tree.c](src/tree.c) which build and link the
nodes, and help traverse the tree.

## An Example of a Tree Being Built

Let's look at the sequence of function calls that results in an AST being built
for the last line in this short C function:

```
void main()
{
  int a;
  int list[4];
  a= list[2] + 9;	// This expression
}
```

Remember, assignment statements are also expressions in the C language.

 + We start in `stmt()` (in [src/stmt.c](src/stmt.c)) which is looking for a
   statement, but doesn't find a keyword. It falls to the bottom and calls `expr()`
   (in [src/expr.c](src/expr.c)).
 + `expr()` calls `exprlist()` which calls `asgmnt()`.
 + `asgmnt()` is looking for an expression which may be followed by an assignment
    token. It calls `cond3()` to parse the expression.
 + `cond3()` calls `cond2()` which calls `binexpr()` which calls `cast()`.
 + We are getting somewhere, trust me. What is happening here is that we are
   traversing functions in precedence order of the related operators.
 + `cast` calls `prefix()` which calls `postfix()` which calls `primary()`.
 + The job of `primary()` is to resolve identifiers, string and integer literals.
   We look at the token (*a*) and see that it is an identifier.
   We look for it with `y= findsym()`. Once we find it, we can create a leaf node
   with `n = mkleaf(OP_IDENT, y)`. The node is marked as having an address
   and `primary()` returns the node `n`. We now have:

```
   id a		(OP_IDENT)
```

 + We bubble back up to `asgmnt()` which receives the node pointer `n` and
   parses the `=` sign. It now recursively calls itself (i.e. `asgmt()`).
 + This bubbles back down to `primary()` which spots the `list` identifer and
   returns this OP_IDENT leaf node:

```
   id list	(OP_IDENT)
```

 + This node bubbles back to `postfix()` which parses the '[' token. Now we
   know that this is an array reference.
 + `indirection()` is called to check this. It calls `rvalue()` to get a
    pointer to `list`. This changes the previous node into:

```
   addr list	(OP_ADDR)
```

 + Once this new node is received by `postfix()`, it calls `expr()` then
   `rvalue()` to get the value of the expression with the square brackets.
 + This expression is checked to have integer type, and then 
   `n2 = mkunop(OP_SCALE, n2)` is called to tell us to scale this expression
   by the size of the elements in the `list`. We now have this new tree:

```
   scale	(OP_SCALE)
     |
     2		(OP_LIT)
```

 + `postfix()` now combines the `list` node with this tree to make the tree:

```
      OP_ADD
     /      \
 addr list   scale   
               |
               2
```

 + This tree bubbles back up to `binexpr()` which parses the `+` token.
   `rvalue()` is called to get the expression value from the right-hand side,
    which returns this tree:

```
    9		(OP_LIT)
```

 + `binexpr()` calls `mkop()` with the two sub-trees and the OP_PLUS operation
    to make the new tree:

```
           OP_PLUS
          /      \
      OP_ADD      9
     /      \
 addr list   scale   
               |
               2
```

 + This tree bubbles back up to `asgmnt()` which still has the tree for the
   side before the `=` token. This now calls `mkbinop2(OP_ASSIGN, ...)` with
   the two sub-trees to make this final AST:

```
    OP_ASSIGN
     /      \
  id a     OP_PLUS
          /      \
      OP_ADD      9
     /      \
 addr list   scale   
               |
               2
```

And when we run `scc -T -t` on this input file, it draws the same tree
slightly differently:


```
x=y 2 1023
  `-id a
  `-x+y 2 2
    `- *x 2 
    | `-x+y (int,int)
    |   `-addr list
    |   `-scale
    |     `-lit 2
    `-lit 9
```

Now, where would be be without recursion?!

I'm going to cover the parsing of expressions and AST building
in the next part of the tour. The part after that will cover
the traversing of the ASTs and the generation of generic assembly output.

## The Functions in [src/tree.c](src/tree.c)

Now that we've seen what the AST nodes look like, and how an AST is built, let's
look at the functions in [src/tree.c](src/tree.c) that are used to build and
maintain each AST.

There is a low-level function with this prototype:

```
// Create a new AST node and add it to the Nodes[] storage area.
// args points at the arguments in the node. na is the number of
// argument pointers.
// Return a pointer to the node.
static node *mknode(int op, int na, int *args, node *left, node *right);
```

I'm not going to show the code because it's not pretty. Rather than using
dynamic memory allocation (`malloc()` and friend), SubC allocates this from
a pool of memory called `Nodes[]`:

```
#define NODEPOOLSZ	4096	/* ints */
extern int     Nodes[NODEPOOLSZ];
extern int     Ndtop;
extern int     Ndmax;
```

The code creates an AST node, copies the left/right pointers and the arguments,
and sets the node's operation to `op`.

## More Specific Node Creation Functions

There are some functions which make specific node types:

```
// Make a leaf node with one argument.
node *mkleaf(int op, int n) {
        int     a[1];

        a[0] = n;
        return mknode(op, 1, a, NULL, NULL);
}

// Make a unary operation node with no arguments & one child.
node *mkunop(int op, node *left) {
        return mknode(op, 0, NULL, left, NULL);
}

// Make a unary operation node with one argument & one child.
node *mkunop1(int op, int n, node *left) {
        int     a[1];

        a[0] = n;
        return mknode(op, 1, a, left, NULL);
}

// Make a unary operation node with two arguments & one child.
node *mkunop2(int op, int n1, int n2, node *left) {
        int     a[2];

        a[0] = n1;
        a[1] = n2;
        return mknode(op, 2, a, left, NULL);
}

// Make a binary operation node with no arguments.
node *mkbinop(int op, node *left, node *right) {
        return mknode(op, 0, NULL, left, right);
}

// Make a binary operation node with one argument.
node *mkbinop1(int op, int n, node *left, node *right) {
        int     a[1];

        a[0] = n;
        return mknode(op, 1, a, left, right);
}

// Make a binary operation node with two arguments.
node *mkbinop2(int op, int n1, int n2, node *left, node *right) {
        int     a[2];

        a[0] = n1;
        a[1] = n2;
        return mknode(op, 2, a, left, right);
}
```

## Tree Dumping Functions

SubC can dump the contents of the expression trees in text form. There is a
high-level function called `dumptree()` which receives a pointer to the root
of the tree, and which traverses and dumps the entire tree. It relies on
several helper functions to print out unary, binary and leaf nodes.

The code is straight-forward, but you look at the functions to see how an
AST is traversed in general:

  + Deal with any left-hand child
  + Deal with any right-hand child
  + Deal with the operation between them

The only difference is, for printing reasons, the operation is printed first.

## Functions that Traverse and Generate Code

At the end of [src/tree.c](src/tree.c) there are several `emitXXX()` functions
which take an expression tree, traverse it and call functions in the generic
code generator to *emit* generic assembly output for the expression.

As the tree traversal and the code generation are intertwined, and you've already
learned enough about ASTs here, I'll delay the coverage of the `emitXXX()` functions
until the part of the tour on generic code generation.
