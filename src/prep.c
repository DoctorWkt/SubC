/*
 *	NMH's Simple C Compiler, 2011,2012,2014
 *	Preprocessor
 */

#include "defs.h"
#include "data.h"
#include "decl.h"

// Push an expanded macro onto the macro stack.
// Error if there's no room on the stack.
// Also store the next "real" character from
// the input file in Macp[] at the top of the stack.
// Increment Mp to point at the next free stack slot.
void playmac(char *s) {
	if (Mp >= MAXNMAC) fatal("too many nested macros");
	Macc[Mp] = next();
	Macp[Mp++] = s;
}

// Get a line of text up to max characters from
// the input file. Trim off the \n and any \r
// character. Return the length of the line.
int getln(char *buf, int max) {
	int	k;

	if (fgets(buf, max, Infile) == NULL) return 0;
	k = strlen(buf);
	if (k) buf[--k] = 0;
	if (k && '\r' == buf[k-1]) buf[--k] = 0;
	return k;
}

// Define a macro with #ifdef
static void defmac(void) {
	char	name[NAMELEN+1];
	char	buf[TEXTLEN+1], *p;
	int	y;

	// Get the next token and copy its value into name[]
	// Error if this was not an identitier
	Token = scanraw();
	if (Token != IDENT)
		error("identifier expected after '#define': %s", Text);
	copyname(name, Text);

	// Get the rest of the line into the buf[]
	// Skip any leading whitespace
	if ('\n' == Putback)
		buf[0] = 0;
	else
		getln(buf, TEXTLEN-1);
	for (p = buf; isspace(*p); p++)
		;

	// Error if the macro was already defined.
	// Otherwise use addglob() to add this as a global macro
	if ((y = findmac(name)) != 0) {
		if (strcmp(Mtext[y], buf))
			error("macro redefinition: %s", name);
	}
	else {
		addglob(name, 0, TMACRO, 0, 0, 0, globname(p), 0);
	}
	Line++;
}

// Deal with an #undef directive
static void undef(void) {
	char	name[NAMELEN+1];
	int	y;

	// Get the next token and copy its
	// value into name[]
	Token = scanraw();
	copyname(name, Text);

	// Error if not an identifier. Otherwise,
	// search for the macro. If found, set it to "#undef'd"
	if (IDENT != Token)
		error("identifier expected after '#undef': %s", Text);
	if ((y = findmac(name)) != 0)
		Names[y] = "#undef'd";
}

// Do the work to #include another file as input
static void include(void) {
	char	file[TEXTLEN+1], path[TEXTLEN+1];
	int	c, k;
	FILE	*inc, *oinfile;
	char	*ofile;
	int	oc, oline;

	// Skip past the '<' and set c to '<'
	// for later
	if ((c = skip()) == '<')
		c = '>';

	// Get the rest of the line
	k = getln(file, TEXTLEN-strlen(SCCDIR)-9);
	Line++;

	// Error if the line was empty or did not end in '>'
	if (!k || file[k-1] != c)
		error("missing delimiter in '#include'", NULL);
	if (k) file[k-1] = 0;

	// If the filename starts with a " it's a local filename,
	// otherwise prepend SCCDIR/include/ to the filename.
	if (c == '"')
		strcpy(path, file);
	else {
		strcpy(path, SCCDIR);
		strcat(path, "/include/");
		strcat(path, file);
	}

	// Open up the file and error if we can't
	if ((inc = fopen(path, "r")) == NULL)
		error("cannot open include file: %s", path);
	else {
		Inclev++;		// Increment the #include level
		oc = next();		// Stash the next character from the
		oline = Line;		// current file, also stash the old
		ofile = File;		// filename, current line number and
		oinfile = Infile;	// FILE pointer.
		Line = 1;		// Start at line 1
		putback('\n');		// Start with a newline (a dummy char?)
		File = path;		// Set the filename and FILE pointer
		Infile = inc;
		Token = scan();		// Get the first token.
		while (XEOF != Token)	// Start by looking for global declarations
			top();		// until the file ends.
		Line = oline;		// Restore the old filename, line number
		File = ofile;		// and FILE pointer that were stashed
		Infile = oinfile;
		fclose(inc);		// Close down the open #include'd file
		putback(oc);		// Restore the next char from old input file
		Inclev--;		// Decrement the include level
	}
}

// Deal with an #ifdef
static void ifdef(int expect) {
	char	name[NAMELEN+1];

	// Error if too many #ifdefs
	if (Isp >= MAXIFDEF)
		fatal("too many nested '#ifdef's");

	// Get the identifier after the #ifdef
	// and copy it to name[]
	Token = scanraw();
	copyname(name, Text);

	// Error if there was no identifier after the #ifdef
	if (IDENT != Token)
		error("identifier expected in '#ifdef'", NULL);

	// Put P_IFDEF on the ifdef stack if we are not in a frozen
	// context and the name[] macros evaluates to true. Otherwise
	// put P_IFNDEF on the ifdef stack to mark the context frozen.
	if (frozen(1))
		Ifdefstk[Isp++] = P_IFNDEF;
	else if ((findmac(name) != 0) == expect)
		Ifdefstk[Isp++] = P_IFDEF;
	else
		Ifdefstk[Isp++] = P_IFNDEF;
}

// Deal with an #else directive
static void p_else(void) {

	// Error if no matching #ifdef
	if (!Isp)
		error("'#else' without matching '#ifdef'", NULL);

	// Do nothing if we're in a frozen context
	else if (frozen(2))
		;

	// We have to already have a P_IFDEF or a P_IFNDEF
	// on the ifdef stack, otherwise there's no matching #ifdef.
	// Reverse the meaning of the value on the ifdef stack.
	// Use the special P_ELSENOT value, so we see a following #else
	// as an error and don't "double negative" it.
	else if (P_IFDEF == Ifdefstk[Isp-1])
		Ifdefstk[Isp-1] = P_ELSENOT;
	else if (P_IFNDEF == Ifdefstk[Isp-1])
		Ifdefstk[Isp-1] = P_ELSE;
	else
		error("'#else' without matching '#ifdef'", NULL);
		
}

// Deal with an #endif. Give error if no matching #define.
// Otherwise pop a value from the ifdef stack.
static void endif(void) {
	if (!Isp)
		error("'#endif' without matching '#ifdef'", NULL);
	else
		Isp--;
}

// Parse the #error directive. Read the rest of the
// line, and print it out with an error message,
// then die.
static void pperror(void) {
	char	buf[TEXTLEN+1];

	if ('\n' == Putback)
		buf[0] = 0;
	else
		getln(buf, TEXTLEN-1);
	error("#error: %s", buf);
	exit(1);
}

// Parse the #line directive by reading
// the rest of the line to get the
// current line number
static void setline(void) {
	char	buf[TEXTLEN+1];

	if ('\n' == Putback)
		buf[0] = 0;
	else
		getln(buf, TEXTLEN-1);
	Line = atoi(buf) - 1;
}

// Skip everything up to the end of the current line
static void junkln(void) {
	while (!feof(Infile) && fgetc(Infile) != '\n')
		;
	Line++;
}

// Return true if the "depth" element on the
// ifdef stack is a frozen context. A frozen
// context is one which can be ignored, e.g.
// a preprocessor directive in an #ifdef
// section when the #ifdef was false.
int frozen(int depth) {
	return Isp >= depth &&
		(P_IFNDEF == Ifdefstk[Isp-depth] ||
		P_ELSENOT == Ifdefstk[Isp-depth]);
}

// Deal with a preprocessor directive
void preproc(void) {
	putback('#');				// Put the '#' back so the scanner
	Token = scanraw();			// can use it. Disable macro expansion
						// Ignore #ifdef, #ifndef, #else and
	if (	frozen(1) &&			// #endif directives in a frozen context
		P_IFDEF != Token && P_IFNDEF != Token &&
		P_ELSE != Token && P_ENDIF != Token
	) {
		junkln();			// by ignoring the line and returning
		return;
	}
	switch (Token) {			// Otherwise deal with the known
	case P_DEFINE:	defmac(); break;	// preprocessor directives
	case P_UNDEF:	undef(); break;
	case P_INCLUDE:	include(); break;
	case P_IFDEF:	ifdef(1); break;
	case P_IFNDEF:	ifdef(0); break;
	case P_ELSE:	p_else(); break;
	case P_ENDIF:	endif(); break;
	case P_ERROR:	pperror(); break;
	case P_LINE:	setline(); break;
	case P_PRAGMA:	junkln(); break;	// Ignore #pragma and unknown
	default:	junkln(); break;	// preprocessor directives
			break;
	}
}
