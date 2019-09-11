# A Tour Through the SubC C Compiler

I've always wanted to [grok](https://en.wikipedia.org/wiki/Grok) how a
real compiler works. I've done compiler courses which were theoretical, and
I've written my own [toy compiler](https://github.com/DoctorWkt/h-compiler).

This is my own personal tour through the
[SubC](http://www.t3x.org/subc/http://www.t3x.org/subc/) C compiler,
written by Nils M Holm. I've done this to help me grok a compiler that
is small but which is complete enough to be able to recompile itself.

I've added a lot of comments to the original compiler code,
[subc-20161212.tgz](http://www.t3x.org/subc/subc-20161212.tgz), and I've
written the descriptions of the stages below.

I haven't really covered much of the compiler theory behind the operation
of SubC, so you really should buy and read Nils' book about SubC:
[Practical Compiler Construction](http://www.t3x.org/reload/index.html).
What I have done is to give an alternate viewpoint of the operation of
SubC, and to cover the additions to the compiler written after Nils' book.
Please don't read my descriptions instead of the book; Nils has put a
lot of effort into SubC and he deserves some recompense.

The topics that I will cover are:

 + [The Basic Operation of a Compiler](1_Basic_Operation.md)
 + [Lexical Analysis]( 2_Lexical_Analysis.md)
 + [The C Pre-processor](3_Preprocessor.md)
 + [Parsing of Statements](4_Statement_Parsing.md)
 + [The Symbol Table](5_Symbol_Table.md)
 + Parsing of Declarations
 + [Abstract Syntax Trees](7_Abstract_Syntax_Trees.md)
 + [Parsing of Expressions](8_Expression_Parsing.md)
 + General Optimisation
 + General Code Generation
 + The x64 Code Generator

## Putting It Into Action

If you've cloned this GitHub repository to a Linux or BSD box, you should
be able to do:

```
$ ./configure
```

to get the source ready to compile on your system. Then `cd src` into
the source code tree and do:

```
$ make
```

to build the `scc0` compiler binary. To prove that the compiler works,
you can do this:

```
$ make scc
```

which is the *compiler triple test*: compile the compiler with another
compiler (e.g. Gnu C), then compile the compiler with itself, then
use this second compiler executable to compile itself again.

After that, you can do:

```
$ sudo make install
```

to install `scc` into `/usr/local/bin`.

Now go back and read through the descriptions and following along
in the source code.

## Copyrights

Unless otherwise noted,

 + all source code and scripts are &copy; Nils M Holm and
   have been placed in the public domain.
 + all non-source code documents (e.g. English documents,
   image files) are &copy; Warren Toomey under the
   [Creative Commons BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) license.
