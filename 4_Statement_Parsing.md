# Statement Parsing in SubC

It's time to turn our attention to the parsing (syntax analysis) of
the input tokens from the scanner. SubC breaks this down into three
sections:

  + Statement parsing in [src/stmt.c](src/stmt.c)
  + Declaration parsing in [src/decl.c](src/decl.c)
  + Expression parsing in [src/expr.c](src/expr.c)

![](Figs/subc_parsing.png)

The parser interacts with the symbol table to do things like create and
retrieve the types of variables and function parameters. Not only does
the parser do syntax analysis, e.g. to identify this as incorrect:

```
	{ if (2+3] { print(("Hell"o}; ++ ]))
```

but it also does semantic analysis to detect errors like type mismatches, e.g.

```
	int x[];
	int y;
	y= x+23;
```

The parser has the job of calling the code generator to output the assembly
code. Some of this can be done directly; some of this is done by constructing
[abstract syntax trees](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
(ASTs) which are then optimised and flattened by the generic code generator.

## The C Grammar and BNF Notation

Each language has a *grammar*: the set of acceptable statements in that language,
and how that language is structured. In computing languages, this is often
expressed in [Backus-Naur Form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form).
In the comments for the files [src/stmt.c](src/stmt.c), [src/decl.c](src/decl.c) and
[src/expr.c](src/expr.c), SubC outlines the grammar of the subset of C that it recognises
in BNF format.

Here's the first fragment of the grammar from the top of [src/stmt.c](src/stmt.c)

```
   compound :=
          { stmt_list }
        | { }
  
   stmt_list:
          stmt
        | stmt stmt_list
```

A *compound statement* is a *list of statements* surrounded by curly brackets, or
it is a pair of curly brackets with nothing on the inside. At this point I should
touch on the concept of
[terminal and non-terminal symbols](https://en.wikipedia.org/wiki/Terminal_and_nonterminal_symbols).

A *terminal* symbol is a symbol in a grammar that cannot be broken into other symbols: they
are irreducible. In SubC, the tokens that were recognised by the scanner are the terminal
symbols, because they cannot be subdivided.

On the other hand, a *non-terminal* symbol is a symbol which can be broken into other
symbols. There is a *production rule* in the grammar which describes how a *non-terminal* can
be *produced* (broken up) into other symbols.

So, for the *compound* non-terminal, the rule is:

```
   compound :=
          { stmt_list }
        | { }
```

where the curly brackets are the terminal symbols and *stmt_list* is another non-terminal.
How do we know that *stmt_list* is a non-terminal symbol? Because it has its own
production rule:

```
   stmt_list:
          stmt
        | stmt stmt_list
```

A statement list is either a single statement, or a statement followed by another
statement list. Notice that this rule is *recursive*: one of the alternatives in the
rule has its own non-terminal.

Now, we can replace *stmt_list* by a single *stmt* (alternative #1). Therefore, the
second alternative allows *stmt_list* to be two *stmt*s in a row: the first *stmt*
is given, and we know that the *stmt_list* can be replaced with a single *stmt*.

Given this, it should be obvious that a *stmt_list* could also be three *stmt*s in a row:

```
    stmt_list => stmt stmt_list
    stmt_list => stmt stmt stmt_list
    stmt_list => stmt stmt stmt
```

Inductively, the original BNF rule for *stmt_list* allows it to be one or more *stmt*s.
This is the power of the BNF notation: it's compact but it permits a very sophisticated
grammar to be described.

## Recursive Descent Parsing

SubC implements the recognition of the C grammar with a
[recursive descent parser](https://en.wikipedia.org/wiki/Recursive_descent_parser).
Following the recursive nature of the BNF rules, each function in the parser can
call other functions, which can themselves call each other including the original
function.

Each function has the job of recognising one of the non-terminals in the
grammar. To do this, it reads one or more tokens from the input stream. Based on this
token, it uses the BNF definition to determine which alternative it should follow
and goes down that path. Sometimes that path fails, so the parser might have to
back up (i.e. push back a token) to follow a different path. If the tokens do not
match any of the paths in the BNF rules, the parser declares a *syntax error*.

That's enough of the theory of parsing, but I've only touched the surface. At this
point, I highly recommend that you go get Nils' book about SubC,
[Practical Compiler Construction](http://www.t3x.org/reload/index.html), as it
covers the theory in more detail.

## Statement Parsing: Where and How to Start?

We will start our tour of the SubC parser with statement parsing. The
top-level statement parsing function is `stmt()` which is in
[src/stmt.c](src/stmt.c). The first thing we need to determine is
how we get to the point where the code in `stmt()` is running.

The SubC compiler starts in `main()` (in [src/main.c](src/main.c))
which interprets the flags and file arguments on the command line.

Every non-flag argument is passed to the `compile()` function:

```
        for (i=1; i<argc; i++) {
                if (*argv[i] != '-') break;
                if (!strcmp(argv[i], "-")) {
                        compile(NULL, def);
                        exit(Errors? EXIT_FAILURE: EXIT_SUCCESS);
                }
```

After opening the correct files, `compile()`
(also in [src/main.c](src/main.c)) then passes control to `program()`:

```
static void compile(char *file, char *def) {
	...
        program(file, in, out, def);
```

`program()` (in [src/decl.c](src/decl.c)) generates the assembly prelude
code, scans the first token and calls the `top()` function:

```
// Parse the specified file and generate assembly output
void program(char *name, FILE *in, FILE *out, char *def) {

        init();                         // Initialise all variables
        defarg(def);                    // Define any -D from the cmd line
        Infile = in;                    // Set the current in/out FILE pointers
        Outfile = out;
        File = Basefile = name;         // Set the input file's name
        genprelude();                   // Generate the assembly prelude
        Token = scan();                 // Get the first token from the input
        while (XEOF != Token)           // Until we hit the end of the file
                top();
        genpostlude();                  // Generate the assembly postlude
                                        // Optionally, dump symbols and stats
        if (O_debug & D_GSYM) dumpsyms("GLOBALS", "", 1, Globs);
        if (O_debug & D_STAT) stats();
}
```

The `top()` function (also in [src/decl.c](src/decl.c)) parses the various
declarations that can occur at the top of a C program. Eventually it
finds a function declaration followed by a compound statement, i.e.
a pair of '{' ... '}' with zero or more statements inside.

`compound()` (in [src/stmt.c](src/stmt.c)) gets the token after '{'
and repetetively calls `stmt()` until we hit the closing '}':

```
void compound(int lbr) {
        if (lbr) Token = scan();        // Get next token if still on the '{'
        while (RBRACE != Token) {       // Until we get the '}'
                if (eofcheck()) return; // Return on EOF
                stmt();                 // Parse the statement within
        }
        Token = scan();                 // And get the token after the '}'
}
```

And so, we finally hit the `stmt()` function!

## `stmt()`: Recognising the Main C Statements

`stmt()` implements this BNF rule:

```
   stmt :=
          break_stmt
        | continue_stmt
        | do_stmt
        | for_stmt
        | if_stmt
        | return_stmt
        | switch_stmt
        | while_stmt
        | compound
        | ;
        | expr ;
```

where ';' is a terminal and all the other symbols are non-terminals. In other words,
it recognises all the main statements in the C language. Of course, this is too much
for one function to perform, so `stmt()` just looks at the current token and uses this
to determine which alternative to follow. It calls other functions to parse these alternatives:

```
static void stmt(void) {
        int     lv[LV];

        switch (Token) {
        case BREAK:     break_stmt();    break;
        case CONTINUE:  continue_stmt(); break;
        case DO:        do_stmt();       break;
        case FOR:       for_stmt();      break;
        case IF:        if_stmt();       break;
        case RETURN:    return_stmt();   break;
        case SWITCH:    switch_stmt();   break;
        case WHILE:     while_stmt();    break;
        case LBRACE:    compound(1);     break;
        case SEMI:      Token = scan();  break;

        // Can't have 'default' or 'case' outside a 'switch' statement
        case DEFAULT:   wrong_ctx(DEFAULT); break;
        case CASE:      wrong_ctx(CASE); break;

        // Not a statement, try parsing it as an expression followed
        // by a semicolon. Flush the insruction queue.
        default:        expr(lv, 0); semi(); commit(); break;
        }
        clear(1);       // Primary register is now empty
}
```

Note some syntax error reporting: we can't have a `default` or `case` keyword because
we are not in a `switch` statement at this point in time.

There are functions which are involved in the generation of the output assembly code:
`commit()` and `clear()`. I'll delay discussion of those for now, but I'll come back
to them soon.

Let's choose to look at compound statements next.

## `compound()`: Compound Statements

We've already seen the BNF rules for compound statements:

```
   compound :=
          { stmt_list }
        | { }
  
   stmt_list:
          stmt
        | stmt stmt_list
```

We must look for a '{', then zero or more statements, then a '}'. This is done with
a loop that parses statements until the '}' token is received:

```
void compound(int lbr) {
        if (lbr) Token = scan();        // Get next token if still on the '{'
        while (RBRACE != Token) {       // Until we get the '}'
                if (eofcheck()) return; // Return on EOF
                stmt();                 // Parse the statement within
        }
        Token = scan();                 // And get the token after the '}'
}
```

## `do_stmt()`: Do ... While Loops

Here's the BNF rule for Do ... While loops:

```
   do_stmt := DO stmt WHILE ( expr ) ;
```

`stmt` and `expr` are non-terminals, and we will call functions to deal with them.
The rest are terminals, i.e. tokens, and we can read them in from the scanner here.
Here is the code for `do_stmt()`:

```
static void do_stmt(void) {
        int     ls, lb, lc;

        Token = scan();                 // Skip the 'do' token
        ls = label();                   // Get a label for the top of loop
        pushbrk(lb = label());          // Get labels for breaks and continues
        pushcont(lc = label());         // & push them on the respective stack
        genlab(ls);                     // Generate the top label
        stmt();                         // Parse the statement
        match(WHILE, "'while'");        // Get the 'while' token
        lparen();                       // Get the '(' token
        genlab(lc);                     // Generate the continue label
        rexpr();                        // Parse the expression
        genbrtrue(ls);                  // Generate "branch if true to top"
        clear(1);                       // Primary register is now empty
        genlab(lb);                     // Generate the break label
        rparen();                       // Get the ')' and ';' tokens
        semi();
        Bsp--;                          // And pop the break and continue
        Csp--;                          // labels from their stacks
}
```

You should be able to spot the code that reads and checks the tokens: `scan()`,
`match()`, `lparen()`, `rparen()`, `semi()`. You should also be able to spot the
calls to the functions that deal with the non-terminals: `stmt()` and `rexpr()`.
So now let's look at the rest of the code.

We have to translate C code that looks like this:

```
   do {
      // Zero or more statements inside the loop
   } while (this expression is true);
```

into assembly code. We need the ability to return to the top of the set of statements
if the loop expression is true, or keep going onwards if not.

Even more interesting, C has the `break` and `continue` keywords. If we see a `break`,
we must immediately leave the loop, skipping all the statements and the loop test.
If we see a `continue`, we must immediately run the loop's expression; if it's true,
we can execute another loop iteration. If not, we leave the loop.

This is translated into assembly with three labels, the testing of the loop expression
and a jump based on the test's result:

```
    start_label:
	// All the assembly code for the
	// statements inside the loop
	
    continue_label:
	test the loop expression
	jump to the start_label if the test is true
    break_label:
```

Any `break` inside the loop will be translated into an immediate jump to the `break_label`,
and any `continue` inside the loop will be translated into an immediate jump to the
`continue_label`. The loop's expression is evaluated at the bottom and jumps to the
`start_label` if true.

With that explained, we can return to the analysis of the `do_stmt()` code, but with one
more sidetrack. C allows loops inside loops, so there can be nested labels, e.g.

```
    lvl1_continue_label:
	...
    	lvl2_continue_label:
	...
    	lvl2_break_label:
	...
    lvl1_break_label:
```

We thus need a stack to hold the `break` and `continue` labels for our loops, so that
any specific `break` command can be translated to the correct label for its loop.

Returning to the `do_stmt()` code, we can see that three assembly labels are generated:
`ls`, `lb` and `lc` for the `start_label`, `break_label` and `continue_label`. Two of
them are pushed onto stacks.

The `start_label` is output, followed by the assembly for all the statements inside
the loop. The `while` and '(' tokens are parsed. Now the `continue_label` is output.
The assembly code for the expression is output by `expr()`. The "branch if true"
assembly to the `start_label` is output by `genbrtrue()`. Now the `break_label` is
output along with the parsing of the ')' and ';' tokens. Finally, the two labels
are popped from their stacks as they are no longer needed.

Finally, a quick word about the call to `clear(1)`. SubC's generic gode generator
models a hypothetical CPU with a main register called an *accumulator*. There is only
one accumulator; if it is needed while it is occupied, the accumulator must be
[spilled](https://www.webster-dictionary.org/definition/register%20spilling) to
free it for another use.

The `rexpr()` call calculated the value of the loop's expression and left this value in
the accumulator. The `genbrtrue()` call generated the code that used this value, but
it left the accumulator "occupied". So, the call to `clear()` marks that the accumulator
is free again for other assembly code to use it.

## `while_stmt()`: While Loops

The BNF rule for While loops is:

```
   while_stmt := WHILE ( expr ) stmt
```

It's very similar to the Do ... While loop except the loop condition is tested
at the top of the loop, not at the end. So the `while_stmt()` function's code
is very similar to the `do_stmt()` code:

```
static void while_stmt(void) {
        int     lb, lc;

        Token = scan();                 // Skip the 'while' token
        pushbrk(lb = label());          // Get labels for breaks and continues
        pushcont(lc = label());         // & push them on the respective stack
        genlab(lc);                     // Generate the continue label
        lparen();                       // Get the '(' token
        rexpr();                        // Parse the expression
        genbrfalse(lb);                 // Generate branch if false to end of loop
        clear(1);                       // Primary register is now empty
        rparen();                       // Get the ')' token
        stmt();                         // Parse the inner statement
        genjump(lc);                    // Generate jump back to top of loop
        genlab(lb);                     // Generate the end of loop label
        Bsp--;                          // And pop the break and continue
        Csp--;                          // labels from their stacks
}
```

## Implementing `break` and `continue`

Given that we already have a stack of labels for the relevant jump points
for both `break` and `continue`, these are easy to implement.

```
/*
 * break_stmt := BREAK ;
 */

static void break_stmt(void) {
        // Read the break
        // Error check if nothing on the break stack
        // Generate a jump to the location on top of the break stack
        // and skip past the semicolon
        Token = scan();
        if (!Bsp) error("'break' not in loop/switch context", NULL);
        genjump(Breakstk[Bsp-1]);
        semi();
}

/*
 * continue_stmt := CONTINUE ;
 */

static void continue_stmt(void) {
        // Read the break
        // Error check if nothing on the continue stack
        // Generate a jump to the location on top of the continue stack
        // and skip past the semicolon
        Token = scan();
        if (!Csp) error("'continue' not in loop context", NULL);
        genjump(Contstk[Csp-1]);
        semi();
}
```

## `for_stmt()`: Implementing For Loops

```
   for_stmt :=
        FOR ( opt_expr ; opt_expr ; opt_expr ) stmt
  
   opt_expr :=
        | expr
```

For loops in C are interesting for two reasons:

 + The three clauses inside the loop parentheses are each optional.
 + The generated code isn't actually sequential.

Let me explain the second point by rewriting a For loop as if it was a
While loop:

```
   for_stmt :=
        FOR ( opt_expr1 ; opt_expr2 ; opt_expr3 ) stmt

   opt_expr1;
   while ( opt_expr2 is true ) {
      stmt;
      opt_expr3;
   }
```

As you can see, the code for `opt_expr3` comes after `stmt` even though we will
parse `opt_expr3` comes before `stmt`. We're going to have to deal with this
somehow.

One strategy is to generate and "stash" the `opt_expr3` code. Then generate all
the other code. Then "unstash" the `opt_expr3` code and generate it. We would end
up with this assembly structure:

```
	opt_expr1 code
    continue_label:
	test the loop expression
	jump to the break_label if the test is true
	stmt code
	opt_expr3 code
	jump to the continue_label
    break_label:
```

SubC takes a different strategy which doesn't require the "stashing" of any
assembly ouput. But this forces the `opt_expr3` code to be generated before
the `stmt` code. So how do we get them to execute in the correct sequence
if they are generated out of order? The answer is to use two more labels and
some jumps:

```
	opt_expr1 code
    continue_label:
	test the loop expression
	jump to the break_label if the test is true
	jump to the body_label
expr3_label:
	opt_expr3 code
	jump to the continue_label
body_label:
	stmt code
	jump to the expr3_label
    break_label:
```

Yes it's ugly and inefficient, but it works!. So you should now be able to see how the
C code in `for_stmt()` implements the above structure:

```
static void for_stmt(void) {
        int     ls, lbody, lb, lc;

        Token = scan();                 // Skip the 'for' token
        ls = label();                   // Get a label for the top of loop
        lbody = label();                // and for the start of the inner statement
        pushbrk(lb = label());          // Get labels for breaks and continues
        pushcont(lc = label());         // & push them on the respective stack
        lparen();                       // Get the '(' token
        if (Token != SEMI) {            // Parse any first expression
                rexpr();
                clear(1);               // Primary register is now empty
        }
        semi();                         // Get the ';' token
        genlab(ls);                     // Generate the top label
        if (Token != SEMI) {            // Parse any second expression
                rexpr();
                genbrfalse(lb);         // Generate branch if false to break label
                clear(1);               // Primary register is now empty
        }
        genjump(lbody);
        semi();                         // Get the ';' token
        genlab(lc);                     // Generate the continue label
        if (Token != RPAREN)
                rexpr();
        clear(1);                       // Primary register is now empty
        genjump(ls);                    // Generate jump to start of middle expression
        rparen();                       // Get the ')' token
        genlab(lbody);                  // Generate label for start of inner statement
        stmt();                         // Parse the inner statement
        genjump(lc);                    // Generate jump back to third expression code
        genlab(lb);                     // Generate the break label
        Bsp--;                          // And pop the break and continue
        Csp--;                          // labels from their stacks
}
```

## `if_stmt()`: Implementing If Statements

The BNF rule for If statements is:

```
   if_stmt :=
          IF ( expr ) stmt
        | IF ( expr ) stmt ELSE stmt
```

which means that the `else` clause is optional. This means that we have to translate
this into two different assembly outputs. The first alternative is:

```
	test the expression
	jump to the l1_label if the test is false
	stmt code					// Code when expr is true
l1_label:
```

The second alternative is:

```
	test the expression
	jump to the l1_label if the test is false
	stmt code					// Code when expr is true
	always jump to the l2_label
l1_label:
	stmt code					// Code when expr is false
l2_label:
```

This means that we have to generate the `l1_label` at the end for one alternative,
but generate the `l2_label` at the end for the second alternative. See if you can
spot how this is done in the following C code:

```
static void if_stmt(void) {
        int     l1, l2;

        Token = scan();                 // Skip the 'if' token
        lparen();                       // Get the '(' token
        rexpr();                        // Parse the expression
        l1 = label();                   // Get the label for non-true code
        genbrfalse(l1);                 // Generate branch if false to this label
        clear(1);                       // Primary register is now empty
        rparen();                       // Get the ')' token
        stmt();                         // Parse the statement
        if (ELSE == Token) {            // If we have an 'else' token
                l2 = label();           // Get a second label. Swap l1/l2 so the
                genjump(l2);            // new label is the non-false code. Generate
                genlab(l1);             // the jump to the end (l2) code. Generate
                l1 = l2;                // the false code (l1 label). 
                Token = scan();         // Skip the 'else' token and
                stmt();                 // Parse the inner statement
        }
        genlab(l1);                     // Generate the label for non-true code
}
```

## `switch_stmt()` and `switch_block()`: Implementing Switch Statements

SubC gives the BNF rules for Switch statements as follows:

```
   switch_stmt :=
          SWITCH ( expr ) { switch_block }
  
   switch_block :=
          switch_block_stmt
        | switch_block_stmt switch_block
  
   switch_block_stmt :=
          CASE constexpr :
        | DEFAULT :
        | stmt
```

The `switch_block` has to contain at least one `switch_block_stmt`. However, notice that
a `switch_block_stmt` could be just a single `stmt`. This means that the BNF grammar
permits the following code:

```
    switch (x) {
	printf("Hello\n");
    }
```

Obviously we don't want to permit this, so we'll have to implement *semantic analysis* to
prevent a programmer from doing this.

SubC implements a Switch statement in assembly this way:

```
	test the expression
	jump to the ls_label
la:	case a code	// Including a jump to lb if a break
lb:	case b code
ldef:   default code
	jump to lb

ls:	load a pointer to the switch jump table
	call code to jump to la, lb or ldef
	switch jump table
lb:
```
As with the For loop, the switch block code is generated sequentially, with
appropriate labels and jumps to delay the selection of the correct
option until after all the switch blocks.

I'm not going to cover the operation of the code that interprets the switch
jump table. This is hand-coded assembly code in [src/lib/crt0.s](src/lib/crt0.s)
if you want to see it.

We start with `switch_stmt()`:

```
static void switch_stmt(void) {
        Token = scan();                 // Skip the 'switch' token
        lparen();                       // Get the '(' token
        rexpr();                        // Parse the expression
        commit();                       // Flush the insruction queue
        clear(0);                       // Primary register is now empty, but
                                        // don't touch the instruction queue.
                                        // XXX Why not?
        rparen();                       // Get the ')' token
        if (Token != LBRACE)            // Syntax error if '{' not the next token
                error("'{' expected after 'switch'", NULL);
        switch_block();                 // Now parse the switch block itself
}
```

which does the parsing of the tokens and calls `rexpr()` to generate the
code to evaluate the Switch expression. I'm still not sure why `clear()`
is called with argument 1. With the top of the Switch statement parsed,
we call `switch_block()`:

```
static void switch_block(void) {
        int     lb, ls, ldflt = 0;
        int     cval[MAXCASE];          // List of case values and case labels
        int     clab[MAXCASE];
        int     nc = 0;                 // Start with no cases found yet

        Token = scan();                 // Skip the '{' token
        pushbrk(lb = label());          // Get a label for the end of the block
                                        // and add it to the break queue
        ls = label();                   // Generate a label for the jump table
        genjump(ls);                    // and jump to it
        while (RBRACE != Token) {       // Until we find the closing '}'
                if (eofcheck()) return;
                                        // Error if too many cases
                if ((CASE == Token || DEFAULT == Token) && nc >= MAXCASE) {
                        error("too many 'case's in 'switch'", NULL);
                        nc = 0;
                }
                if (CASE == Token) {            // Find and skip a 'case' token
                        Token = scan();
                        cval[nc] = constexpr(); // Get the case expression
                                                // and generate a label for it
                        genlab(clab[nc++] = label());
                        colon();                // Get the ':' token
                }
                else if (DEFAULT == Token) {    // Find and skip a 'default' token
                        Token = scan();
                        ldflt = label();        // Get a label for the default case
                        genlab(ldflt);          // and output this label
                        colon();                // Get the ':' token
                }
                else
                        stmt();                 // Otherwise parse a statement
        }
        if (!nc) {                              // If we have some case statements
                if (ldflt) {                    // Add the default label with
                        cval[nc] = 0;           // value to the value/label lists
                        clab[nc++] = ldflt;
                }
                else
                        error("empty switch", NULL);    // Error if no cases
        }
        genjump(lb);                    // Output a jump to the end label
        genlab(ls);                     // Generate the jump table label
        genswitch(cval, clab, nc, ldflt? ldflt: lb);    // Generate the jump table
        gentext();                      // Go back to the text segment
        genlab(lb);                     // Generate the break label
        Token = scan();                 // Get the next token
        Bsp--;                          // And pop the break label from the stack
}
```

The code builds a list of `case` values in `cval[]` and the
labels that precede the code for these cases in `clab[]`.

The `while (RBRACE != Token) { ... }` loops until the matching '}' token is
received. For each `case` token, The `case` value is calculated (by the
compiler, right now) with `constexpr()` and stored in `cval[]`
along with a label in `clab[]`. The label is output immediately.
The same is done for `default` except that its label is stored in
the `ldflt` variable.

For statements, we simply call `stmt()` to generate the assembly code.

Thus, a switch block that looks like:

```
    case 'a':
    case 'b': printf("Hello\n"); break;
    default:  printf("world\n");
```

will be translated to:

```
L1:
L2:   printf("Hello\n") code
      jump to lb
Ldef: printf("world\n") code
```

Once we receive the ending '}' token and fall out of the `while (RBRACE != Token)`
loop, the code verifies that there was at least one `case` or `default` label; if
not, we have a semantic error.

The `default` label is placed on the end of the list of `case` labels.

Right now, the last assembly code generated was probably the `default` code.
We know that, coming up next, is the code to deal with the switch jump table.
So output a jump to the break label with `genjump(lb)`.

Now it's time to deal with the jump table. Output the label for this code
with `genlab(ls)`. Generate the table itself with `genswitch()`: I'll cover
this later on. Finally, generate the `break` label with `genlab(lb)`.

## Conclusion

That's the parsing of the control statements done:

 + compound statements
 + Do .. While loops
 + While loops
 + For loops
 + If/Else statements
 + Switch statements
 + `break` and `continue`

[src/stmt.c](src/stmt.c) also has a function that deals with the C `return` keyword,
but this checks if the value being returned matches the declaration of the function
being returned on. As this requires knowledge of declarations, we will hold this over
until we cover declaration parsing.
