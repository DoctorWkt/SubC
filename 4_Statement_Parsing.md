# Statement Parsing in SubC

It's time to turn our attention to the parsing (syntax analysis) of
the input tokens from the scanner. SubC breaks this down into three
sections:

  + Statement parsing in [src/stmt.c](src/stmt.c)
  + Declaration parsing in [src/decl.c](src/decl.c)
  + Expression parsing in [src/expr.c](src/expr.c)

The parser interacts with the symbol table to do things like create and
retrieve the types of variables, function parameters. Not only does
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

## Recursive Descent and BNF Notation

XXX add stuff here.

## Statement Parsing; Where and How to Start?

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
