/*
 *	NMH's Simple C Compiler, 2012
 *	setbuf()
 */

#include <stdio.h>

void setbuf(FILE *f, char *buf) {
	setvbuf(f, buf, buf? _IOFBF: _IONBF, BUFSIZ);
}
