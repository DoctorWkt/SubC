/*
 *	NMH's Simple C Compiler, 2011,2012
 *	Error handling
 */

#include "defs.h"
#include "data.h"
#include "decl.h"

// Report an error in the current file on the
// current line. Print out a formatted with the
// string s. Die if we've had too many errors.
void error(char *s, char *a) {
	if (Syntoken) return;		// Don't report an error if we are currently
					// trying to find a synchronisation token.
	if (!Errors) cleanup();		// Cleanup output files if some errors
	fprintf(stderr, "error: %s: %d: ", File, Line);
	fprintf(stderr, s, a);
	fprintf(stderr, "\n");
	if (++Errors > 10) {
		Errors = 0;
		fatal("too many errors");
	}
}

// Report a fatal error and die.
void fatal(char *s) {
	error(s, NULL);
	error("fatal error, stop", NULL);
	exit(EXIT_FAILURE);
}

// Print out a character, either as itself if
// printable or in hex if not, as an error.
void scnerror(char *s, int c) {
	char	buf[32];

	if (isprint(c))
		sprintf(buf, "'%c' (\\x%x)", c, c);
	else
		sprintf(buf, "\\x%x", c);
	error(s, buf);
}

// Synchronise token scanning by reading
// in tokens until the syn token is found.
// Die if we hit EOF. Return the first token
// found after the synchronisation token.
int synch(int syn) {
	int	t;

	t = scan();
	while (t != syn) {
		if (EOF == t)
			fatal("error recovery failed");
		t = next();
	}
	Syntoken = syn;		// XXX Why?
	return t;
}
