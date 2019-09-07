# The C Pre-Processor

As its name implies, the C pre-processor takes C source code and
pre-processes it before it is passed to the compiler proper. Here are
some of the things that it can do.

A macro can be defined, and its definition will later be used instead
of the original macro name, e.g.

```
#define ASCII_BITMASK 0x7f

        ...
  // Ensure ch is a 7-bit ASCII character
  ch = ch & ASCII_BITMASK;

  // A macro can also be undefined
#undef ASCII_BITMASK
```

C code can be compiled or ignored based on the existence of a defined macro,
e.g.

```
#define DEBUG

        ...

#ifdef DEBUG
        // This code would be compiled because DEBUG is defined
#else
        // This code would not be compiled because DEBUG is defined
#endif
```

Code from another file can be included. This redirects the compiler
to reading from the named file. When that file's contents are read,
the compiler resumes reading from the original file, e.g.

```
#include <limits.h>             // A system-wide file, usually in /usr/include
#include "defs.h"
#include "data.h"               // Local files residing in the current dir
#include "decl.h"
        ...
```

When the pre-processor reads lines from other files, it can decorate
these lines with its own pre-processor directives, e.g.

```
# 50 "defs.h"
enum {
 TVARIABLE = 1,
        ...
```

indicates that the `enum` line is line 50 in the `defs.h` file.

## The C Pre-Processor in SubC

Unlike other C compilers which implement the pre-processor as a
separate program, the pre-processor is built into SubC as part
of the lexical analysis stage. The bulk of the pre-processor code
is in [src/prep.c](src/prep.c), but it works with [src/scan.c](src/scan.c)
to be an integral part of the lexical analysis.

Let's go back to `skip()` to see how the pre-processor gets hooked into
the lexical analysis. Remember that `skip()` is supposed to ignore
unwanted input such as whitespace and newlines. But it also does this:

```
                // Deal with preprocessor directives
                // if we have a '#' after a newline
                if (nl && c == '#') {
                        preproc();
                        c = next();
                        continue;
                }
```

A '#' character after a newline signals a pre-processor directive which
calls `preproc()` which is in [src/prep.c](src/prep.c). Now it gets
interesting.

The first thing that `preproc()` does is to put the '#' character
back into the input stream and then call the lexical scanner.

```
void preproc(void) {
        putback('#');                    // Put the '#' back so the scanner
        Token = scanraw();               // can use it. Disable macro expansion
```

This will find the '#' character, then call `keyword()`:

```
                case '#':
                        Text[0] = '#';
                        scanident(next(), &Text[1], TEXTLEN-1);
                        if ((t = keyword(Text)) != 0)
                                return t;
                        error("unknown preprocessor command: %s", Text);
                        return IDENT;
```

`keyword()` does its usual `strcmp()` to find the known directives
and it returns the matching token:

```
static int keyword(char *s) {
        switch (*s) {
        case '#':
                switch (s[1]) {
                case 'd':
                        if (!strcmp(s, "#define")) return P_DEFINE;
                        break;
                case 'e':
                        if (!strcmp(s, "#else")) return P_ELSE;
                        if (!strcmp(s, "#endif")) return P_ENDIF;
                        if (!strcmp(s, "#error")) return P_ERROR;
                        break;
                case 'i':
                        if (!strcmp(s, "#ifdef")) return P_IFDEF;
                        if (!strcmp(s, "#ifndef")) return P_IFNDEF;
                        if (!strcmp(s, "#include")) return P_INCLUDE;
                        break;
                case 'l':
                        if (!strcmp(s, "#line")) return P_LINE;
                        break;
                case 'p':
                        if (!strcmp(s, "#pragma")) return P_PRAGMA;
                        break;
                case 'u':
                        if (!strcmp(s, "#undef")) return P_UNDEF;
                        break;
                }
                break;
                ...
```

However, why did `preproc()` call a function called `scanraw()` and not
the usual `scanpp()`?

The answer is that we have to disable macro expansion. Imagine we have
this C code:

```
#define FRUIT apple
#define FRUIT banana
```

The `#define FRUIT apple` sets up the `FRUIT` macro which will be expanded
to be the identifier `apple`. Later on, any occurance of `FRUIT` will
be replaced with `apple`.

If we don't disable macro expansion, then when we get to the 
`#define FRUIT banana` line, then this would be expanded to:

```
#define apple banana
```

which would define a new macro `apple` instead of redefining the old `FRUIT` macro.
Therefore, there is an alternate version of `scanpp()` called `scanraw()`:

```
// Return the next token from the input
// file without expanding macros
int scanraw(void) {
        int     t, oisp;

        oisp = Isp;             // Save the old ifdef stack pointer
        Isp = 0;                // Set the ifdef stack pointer to the stack bottom
                                // XXX: why do the above?
        Expandmac = 0;          // Don't try to expand macros
        t = scan();             // Get the next input token
        Expandmac = 1;          // Re-enable macro expansion
        Isp = oisp;             // Restore the old ifdef stack pointer
        return t;               // and return the token found
}
```

As you can see, I haven't grokked this fully yet. However, you can see the `Expandmac`
variable being set to zero before we call `scan()` which itself calls `scanpp()`.
There is code in `scanpp()` to prevent a macro from being expanded when `Expandmac` is zero:

```
                default:
                        ...
                        else if (isalpha(c) || '_' == c) {
                                Value = scanident(c, Text, TEXTLEN);
                                if (Expandmac && macro(Text))
                                        break;
                                if ((t = keyword(Text)) != 0)
                                        return t;
                                return IDENT;
                        }
```

The `break` goes to the end of the `switch` statement instead of returning a keyword
or an identifier token.

## On With `preproc()`

The last time we looked at `preproc()`, it had put back the '#' character, disabled
macro expansion with `scanraw()` and received the next token. Here is what happens next:

```
        switch (Token) {                        // Otherwise deal with the known
        case P_DEFINE:  defmac(); break;        // preprocessor directives
        case P_UNDEF:   undef(); break;
        case P_INCLUDE: include(); break;
        case P_IFDEF:   ifdef(1); break;
        case P_IFNDEF:  ifdef(0); break;
        case P_ELSE:    p_else(); break;
        case P_ENDIF:   endif(); break;
        case P_ERROR:   pperror(); break;
        case P_LINE:    setline(); break;
        case P_PRAGMA:  junkln(); break;        // Ignore #pragma and unknown
        default:        junkln(); break;        // preprocessor directives
                        break;
        }
```

Essentially, based on the pre-processor token, we invoke a number of helper functions.
Note that the `#pragma` pre-processor directive and any unknown pre-processor directives
are silently ignored. Let's look at the helper functions one at a time.
