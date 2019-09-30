/*
 *	NMH's Simple C Compiler, 2011,2014
 *	Miscellanea
 */

#include "defs.h"
#include "data.h"
#include "decl.h"

// Initialise the global variables
// XXX Add comments to most variables
void init(void) {
	Line = 1;
	Putback = '\n';
	Rejected = -1;
	Errors = 0;
	Mp = 0;
	Expandmac = 1;
	Syntoken = 0;
	Isp = 0;
	Inclev = 0;
	Globs = 0;
	Locs = NSYMBOLS;
	Nbot = 0;
	Ntop = POOLSIZE;
	Ndmax = 0;
	Bsp = 0;
	Csp = 0;
	Q_type = empty;
	Q_cmp = cnone;
	Q_bool = bnone;
	addglob("", 0, 0, 0, 0, 0, NULL, 0);
	addglob("__SUBC__", 0, TMACRO, 0, 0, 0, globname(""), 0);
	if (!strcmp(OS, "DOS"))
		addglob("__dos", 0, TMACRO, 0, 0, 0, globname(""), 0);
	else
		addglob("__unix", 0, TMACRO, 0, 0, 0, globname(""), 0);
	Infile = stdin;
	File = "(stdin)";
	Basefile = NULL;
	Outfile = stdout;
	opt_init();
}

// Return the position of character c
// in string s, or -1 if c not found
int chrpos(char *s, int c) {
	char	*p;

	p = strchr(s, c);
	return p? p-s: -1;
}

// Copy s into the name global variable
void copyname(char *name, char *s) {
	strncpy(name, s, NAMELEN);
	name[NAMELEN] = 0;
}

// Ensure that the current token is t,
// and fetch the next token. Otherwise
// throw an error 
void match(int t, char *what) {
	if (Token == t) {
		Token = scan();
	}
	else {
		error("%s expected", what);
	}
}

// Several helper functions that
// match specific tokens
void lparen(void) {
	match(LPAREN, "'('");
}

void rparen(void) {
	match(RPAREN, "')'");
}

void lbrace(void) {
	match(LBRACE, "'{'");
}

void rbrace(void) {
	match(RBRACE, "'}'");
}

void rbrack(void) {
	match(RBRACK, "']'");
}

void semi(void) {
	match(SEMI, "';'");
}

void colon(void) {
	match(COLON, "':'");
}

void ident(void) {
	match(IDENT, "identifier");
}

// Check for end of file and report a
// missing '}' if this is the case
int eofcheck(void) {
	if (XEOF == Token) {
		error("missing '}'", NULL);
		return 1;
	}
	return 0;
}

// Return true if the primitive
// type is INT or CHAR
int inttype(int p) {
	return PINT == p || PCHAR == p;
}

// Return true if the primitive type is
// composite, i.e. a struct or union
int comptype(int p) {
	p &= STCMASK;
	return p == PSTRUCT || p == PUNION;
}

// Report an error if the primitive type is void
void notvoid(int p) {
	if (PVOID == p)
		error("void value in expression", NULL);
}
