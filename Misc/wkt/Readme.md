# Warren's Play Area

Here I've got some programs that I'm playing with that are
related to the SubC material:

 + `cgopt41.c`: A combination of the `cg4.c` and `opt1.c` programs from
   the `../book` directory.
 + `optint.c`: An extension to `cgopt41.c` which has print and
    assignment statements. Here is an example input file ('>' is
    the print token):

```
    a=3; b=4; c=5;
    > 2*(a+b)+c*(a+b);
    > a+b+c;
```
 
 + The files `example1` and `example2` are sample inputs to `optint.c`.
 + `showcycle.c`: I just wanted to visualise the cyclic register allocation
    code in Nils' book, so I wrote this code to do that.
