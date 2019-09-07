# Lexical Analysis in SubC

## The Tokens that SubC Recognises

We start our tour through the lexical analyser in SubC with the list
of tokens that it recognises (from [src/defs.h](src/defs.h)):

| Token | Input | Token | Input | Token | Input | Token | Input |
|:-----:|:------:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|
| SLASH | / | ASMINUS | =- | EXTERN | extern | SWITCH | switch |
| STAR | * | ASMOD | =% | FOR | for | TILDE | ~ |
| MOD | % | ASOR | =\| | IDENT | *&lt;identifier&gt;* | UNION | union |
| PLUS | + | ASPLUS | =+ | IF | if | VOID | void |
| MINUS | - | ASRSHIFT | =>> | INCR | ++ | VOLATILE | volatile |
| LSHIFT | << | ASDIV | =/ | INT | int | WHILE | while |
| RSHIFT | >> | ASMUL | =* | INTLIT | *&lt;number&gt;* | XEOF | end of file |
| GREATER | > | ASSIGN | = | LBRACE | { | XMARK | ! |
| GTEQ | >= | AUTO | auto | LBRACK | [ | P_DEFINE | #define |
| LESS | < | BREAK | break | LPAREN | ( | P_ELSE | #else |
| LTEQ | <= | CASE | case | NOT | *unused* | P_ELSENOT | *internal* |
| EQUAL | == | CHAR | char | QMARK | ? | P_ENDIF | #endif |
| NOTEQ | != | COLON | : | RBRACE | } | P_ERROR | #error |
| AMPER | & | COMMA | , | RBRACK | ] | P_IFDEF | #ifdef |
| CARET | ^ | CONTINUE | continue | REGISTER | register | P_IFNDEF | #ifndef |
| PIPE | \| | DECR | -- | RETURN | return | P_INCLUDE | #include |
| LOGAND | && | DEFAULT | default | RPAREN | ) | P_LINE | #line |
| LOGOR | \|\| | DO | do | SEMI | ; | P_PRAGMA | #pragma |
| ARROW | -> | DOT | . | SIZEOF | sizeof | P_UNDEF | #undef |
| ASAND | =& | ELLIPSIS | ... | STATIC | static | | |
| ASXOR | \^ | ELSE | else | STRLIT | *"string"* | | |
| ASLSHIFT | =<< | ENUM | enum | STRUCT | struct | | |

The first column and a third contain the operators in C such as `+=`, `>>`
etc. Then, in columns two and three you can see the keywords such as
`continue`, `if`, `while` etc. There are also the structural tokens such
as parentheses, square brackets and curly braces. Finally, in the last
column, you can see the C pre-processor directives such as `#include` and
`#ifdef`.

There are three tokens that return a value as well as their token identity:

  + IDENT returns an identifier's name. For example the `main()` function
    is an IDENT (value `main`) followed by LPAREN and RPAREN.
  + INTLIT returns a literal integer value such as 2043.
  + STRLIT returns a literal string value such as "Hello, world".

## Scanning: The Simple Tokens

Let's start with some of the code in `scanpp()` from [src/scan.c](src/scan.c).
This function is the code of the token scanning, but it does call a bunch
of helper functions for specific tokens.

`scanpp()` skips unwanted characters with `skip()` which, in turn, calls
`next()` to get the next character from the input file.

```
        // Loop, but return when a token is found
        for (;;) {
                c = skip();
```

We use this first character to determine which token is in the input, but this
might require looking at the next character:

```
                switch (c) {
                case '(':
                        return LPAREN;
                case ')':
                        return RPAREN;
                case '&':
                        if ((c = next()) == '&') {
                                Text[1] = '&';
                                return LOGAND;
                        }
                        else if ('=' == c) {
                                Text[1] = '=';
                                return ASAND;
                        }
                        else {
                                putback(c);
                                return AMPER;
                ...
                }
```

Notice the code that deals with `'&'`. This could be the token `&`, `&&`
or `&=`. We need to get the `next()` character. Once we have this character,
we can discriminate between LOGAND, ASAND or AMPER.

However, if it's AMPER, we have read one too many input characters. So,
we need to `putback(c)` which puts the character back into the input stream.

The scanner builds up the text of some tokens into the `Text[]` character
array.

## The `next()` and `putback()` Functions

`next()` gets the next character from the input stream, or the
character that was previously put back by the scanner. I'll cover the
pre-processor code soon. `putback()` stores any "put back" character
into the `Putback` variable which is zero when empty.

```
// Get the next character from the input file.
int next(void) {
        int     c;

        if (Putback) {                          // Use the character put
                c = Putback;                    // back if there is one
                Putback = 0;
                return c;
        }
        c = fgetc(Infile);                      // Read from input file
        if ('\n' == c) Line++;                  // Increment line count
        return c;
}

// Put back an unwanted character
void putback(int c) {
        Putback = c;
}

```

## More Complex Tokens: Keywords and Identifiers.

The simple one, two and three character tokens are recognised in `scanpp()`
using the same sort of code as we saw for `&`, `&&` and `&=`. For the other
tokens, `scanpp()` calls other functions.

Let's start with keywords and identifiers. These are words which are
alphanumeric and can include underscore characters. We get down to the
`default` case in the `scanpp()` switch statement:

```
                default:
                        if (isdigit(c)) {
                                Value = scanint(c);
                                return INTLIT;
                        }
                        else if (isalpha(c) || '_' == c) {
                                Value = scanident(c, Text, TEXTLEN);
                                if ((t = keyword(Text)) != 0)
                                        return t;
                                return IDENT;
```

We call `scanident()` to get the actual word into the `Text[]` array.
Then we call `keyword()` to see if this is a keyword in the C language.
If so, we return the token that `keyword()` return, otherwise it's an IDENT
token.

`scanident()` gets the first token character, a pointer to the `Text[]` array
and its length. It fills the word into `Text[]` and returns the identifier's
length.

```
// Scan an identifier from the input file and
// store it in buf[].Return the identifier's length
static int scanident(int c, char *buf, int lim) {
        int     i = 0;

        // Allow digits, alpha and underscores
        while (isalpha(c) || isdigit(c) || '_' == c) {
                // Error if we hit the identifier length limit,
                // else append to buf[] and get next character
                if (lim-1 == i) {
                        error("identifier too long", NULL);
                        i++;
                }
                else if (i < lim-1) {   // XX Why the test here?
                        buf[i++] = c;
                }
                c = next();
        }
        // We hit a non-valid character, put it back.
        // NUL-terminate the buf[] and return the length
        putback(c);
        buf[i] = 0;
        return i;
}
```

`keyword()` simply takes a pointer to the stored identifier string,
tries to match it against all known C and C pre-processor keywords.
On a match, it returns the specific token value. If not, it returns 0.
As an optimisation, it switches on the first character of the string
so that it doesn't have to waste time `strcmp()`ing against all the keywords.

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
                ...
        case 'b':
                if (!strcmp(s, "break")) return BREAK;
                break;
        case 'c':
                if (!strcmp(s, "case")) return CASE;
                if (!strcmp(s, "char")) return CHAR;
                if (!strcmp(s, "continue")) return CONTINUE;
                break;
        ...
        return 0;
}
```

## More Complex Tokens: Character Literals

SubC recognises lexical elements such as `'a'` and `'x'` by
returning an INTLIT (literal integer) token with the character's
ASCII value. The `Value` global variable holds the value of INTLIT tokens.
The `Text` variable gets a copy of the characters from the input which
were recognised as a character literal.

Here's the switch code in `scanpp()`:

```
                case '\'':
                        Text[1] = Value = scanch();
                        if ((c = next()) != '\'')
                                error(
                                 "expected '\\'' at end of char literal",
                                        NULL);
                        Text[2] = '\'';
                        return INTLIT;
```

We recognise the leading `'` character. We call out to `scanch()` to get
the ASCII value, and then ensure that the `next()` character is also a `'`.

Character literals in C are quite complicated. Here are some examples:

 + 'a' is a lowercase 'a'
 + '\\t' is a tab character. The backslash gives the 't' special meaning
 + '\012' is the ASCII character with value 012, where 012 is in octal as
   indicated by the leading zero.
 + '\x41' is the ASCII character with value 41, where 41 is in hexadecimal
   as indicated by the leading 'x'.

The code looks for a leading backslash. If none, it's an ordinary character.
Otherwise, use a following '0' or 'x' to spot octal or hexadecimal values.
Otherwise, use a `switch` statement to interpret things like '\\n', '\\t' etc.

```
// Return the next character from a character
// or string literal
static int scanch(void) {
        int     i, c, c2;

        // Get the next input character and interpret
        // metacharacters that start with a backslash
        c = next();
        if ('\\' == c) {
                switch (c = next()) {
                case 'a': return '\a';
                case 'b': return '\b';
                case 'f': return '\f';
                case 'n': return '\n';
                case 'r': return '\r';
                case 't': return '\t';
                case 'v': return '\v';
                case '\\': return '\\';
                case '"': return '"' | 256;     // XXX why 256?
                case '\'': return '\'';

                // Deal with octal constants by reading in
                // characters until we hit a non-octal digit.
                // Build up the octal value in c2 and count
                // # digits in i. Permit only 3 octal digits.
                case '0': case '1': case '2':
                case '3': case '4': case '5':
                case '6': case '7':
                        for (i = c2 = 0; isdigit(c) && c < '8'; c = next()) {
                                if (++i > 3) break;
                                c2 = c2 * 8 + (c - '0');
                        }
                        putback(c);     // Put back the first non-octal char
                        return c2;
                case 'x':
                        return hexchar();
                default:
                        scnerror("unknown escape sequence: %s", c);
                        return ' ';
                }
        }
        else {
                return c;               // Just an ordinary old character!
        }
}
```

## More Complex Tokens: String Literals

For string literals such as "Hello, world", `scanpp()` recognises the leading
double quote and then calls scanstr() which stores the string's value in
`Text[]` and returns a STRINGLIT token. `Value` is the string's length.

```
                case '"':
                        Value = scanstr(Text);
                        return STRLIT;
```

`scanstr()` uses `scanch()` to interpret and get each character in the string.
Once we hit the ending '"' character, the string is NUL-terminated.

```
// Scan in a string literal from the input file,
// and store it in buf[]. Return the length of
// the string. Surround the string with double quotes.
static int scanstr(char *buf) {
        int     i, c;

        // Put in the first double quote
        buf[0] = '"';
        // Loop while we have enough buf space
        for (i=1; i<TEXTLEN-2; i++) {
                // Get the next char and append to buf
                // Return when we hit the ending double quote
                if ((c = scanch()) == '"') {
                        buf[i++] = '"';
                        buf[i] = 0;
                        return Value = i;
                }
                buf[i] = c;
        }
        // Ran out of buf[] space
        fatal("string literal too long");
        return 0;
}
```

## More Complex Tokens: Integer Literals

When `scanpp()` hits a digit, it passes that digit to `scanint()` to
find and return an integer literal, INTLIT:

```
                default:
                        if (isdigit(c)) {
                                Value = scanint(c);
                                return INTLIT;
                        }                default:
```

I'm not going to give the code of `scanint()` because it's code that has
a job to do and it's an unpleasant job. However, the basic steps are:

  + Assume that the number is radix-10, but change to radix-8 if
    there's a leading '0', then to radix-16 if that's followed by an 'x'.
  + Set up a running "sum" to zero.
  + Use a function called `chrpos()` on the fixed string
    "123456789abcdef" to find the next character in that string. This
    returns a value between 0 and 15.
  + For each next character, multiply the "sum" by the radix and
    add on the value of that character (between 0 and 15).
  + When a non-digit (including hex digit) character is found,
    put it back with `putback()` and return the final integer value.

## The `skip()` Function

I mentioned `skip()` at the beginning. It's job is to skip anything
which can be ignored and to return the first input character which
has to be processed by `scanpp()`.

Things that `skip()` ignores are:

  + spaces and tabs
  + newlines
  + `//` comments until the end of the line
  + `/*` comments up until the matching `*/`

As with `scanint()`, the code has a job to do and that job isn't pleasant.

## Rejecting a Token

We've seen that there are times when the scanner looks ahead one character
to make a token determination, and has to put that character back when it's
not needed. For example, we have seen '&' which could be followed by another
'&' to be the LOGAND (logical AND) token. But the next character is 'a', the
beginning of an identifier. We need to put the 'a' back into the input stream.

So, too, there are times when an entire token has to be pushed back into
the stream. This occurs when we are handling expression in
[src/expr.c](src/expr.c). I'll cover this situation when I cover the
[src/expr.c](src/expr.c) file. However, here's the code to do it.

There is a function, `reject()`, to push an entire token back.

```
// We hit a token but can't use it.
// Mark it as rejected. See the cast()
// function in expr.c.
void reject(void) {
        Rejected = Token;
        Rejval = Value;
        strcpy(Rejtext, Text);
}
```

And right at the top of `scanpp()` it checks if there is a rejected token and
uses that instead of getting the next input character through `skip()`:

```
static int scanpp(void) {
        int     c, t;

        if (Rejected != -1) {                   // We have a previously
                t = Rejected;                   // rejected token. Copy
                Rejected = -1;                  // the token #, the text
                strcpy(Text, Rejtext);          // and value, and turn off
                Value = Rejval;                 // the Rejected flag.
                return t;                       // Return this rejected token
        }
```

## Pre-processor Directives

If you have looked at the [src/scan.c](src/scan.c) file, you will notice
that I have overlooked the code that is partially responsible for dealing
with C pre-processor directives. I will cover that in the next section:
[The Pre-Processor](3_Preprocessor.md).

## Conclusion

The code in [src/scan.c](src/scan.c) has the job of recognising the
lexical elements from the input files, and to return a stream of
tokens. `scanpp()` is the code which does this task. Not only does
it return a token value, it also sets the `Text[]` character array
with the text of some tokens (e.g. strings, identifiers), and it also
sets the `Value` variable with the value of some tokens (e.g. integer
literals).
