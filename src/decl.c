/*
 *	NMH's Simple C Compiler, 2011--2014
 *	Declaration parser
 */

#include "defs.h"
#include "data.h"
#include "decl.h"

static int declarator(int arg, int scls, char *name, int *pprim, int *psize,
			int *pval, int *pinit);

/*
 * enumdecl := { enumlist } ;
 *
 * enumlist :=
 *	  enumerator
 *	| enumerator , enumlist
 *
 * enumerator :=
 *	  IDENT
 *	| IDENT = constexpr
 */

// Parse an enum declaration. If glob is true, this
// is a global declaration.
static void enumdecl(int glob) {
	// Start the first enumerator as value zero
	int	v = 0;
	char	name[NAMELEN+1];

	// Get the next token. Skip it if it's an identifier
	Token = scan();
	if (IDENT == Token)
		Token = scan();

	// Ensure we have a following '{'.
	// Then loop until we get the end '}'
	lbrace();
	while (RBRACE != Token) {
		// Copy the enumerator name and ensure
		// it's a valid identifier
		copyname(name, Text);
		ident();
		
		// If this is followed by an '=',
		// scan the following token(s), verify
		// this is a constant and get its value
		if (ASSIGN == Token) {
			Token = scan();
			v = constexpr();
		}

		// Add the enumerator name and value
		// to the relevant symbol table
		if (glob)
			addglob(name, PINT, TCONSTANT, 0, 0, v++, NULL, 0);
		else
			addloc(name, PINT, TCONSTANT, 0, 0, v++, 0);

		// Look back if next token is a ',', else stop now.
		// Also deal with end of files
		if (Token != COMMA)
			break;
		Token = scan();
		if (eofcheck()) return;
	}
	// The definition must end with a '}' ';'
	rbrace();
	semi();
}

/*
 * initlist :=
 *	  { const_list }
 *	| STRLIT
 *
 * const_list :=
 *	  constexpr
 *	| constexpr , const_list
 */

// Parse either a string literal or
// a list of constant expressions
static int initlist(char *name, int prim) {
	int	n = 0, v;
	char	buf[30];		// Used to sprintf out the value

	// Move to the data section
	gendata();

	// Generate a label for the name
	genname(name);

	// If it's a string literal, an error if char isn't the
	// primitive type.
	if (STRLIT == Token) {
		if (PCHAR != prim)
			error("initializer type mismatch: %s", name);

		// Generate the byte values after the name's label
		// ending with a NUL character. Align on a word boundary.
		gendefs(Text, Value);
		gendefb(0);
		genalign(Value-1);

		// Scan in the next token and return the literal's length
		Token = scan();
		return Value-1;		// XXX: Why -1?
	}

	// Not a string literal, so scan the '{' token
	// Until we get a '}' token ...
	lbrace();
	while (Token != RBRACE) {
		// Parse the constant expression
		v = constexpr();
		// Error check the range if it should be of 'char' type
		if (PCHAR == prim) {
			if (v < 0 || v > 255) {
				sprintf(buf, "%d", v);
				error("initializer out of range: %s", buf);
			}
			// Generate a byte of data, or a word if an 'int'
			gendefb(v);
		}
		else {
			gendefw(v);
		}

		// Increment the number of literals found
		n++;

		// Skip past any commas and loop back
		// (doing an EOF check), or leave loop
		// if no commas
		if (COMMA == Token)
			Token = scan();
		else
			break;
		if (eofcheck()) return 0;
	}

	// Align the final storage if we were storing 'char's.
	// Scan in the next token. Error if nothing in the '{ .. }'
	// Return the number of literals read in.
	if (PCHAR == prim) genalign(n);
	Token = scan();
	if (!n) error("too few initializers", NULL);
	return n;
}

// Given a token t, convert it into a primitive type.
// If it's a struct/union, s holds the name of this.
// Return the type value.
int primtype(int t, char *s) {
	int	p, y;
	char	sname[NAMELEN+1];

	// Convert from token value to type value
	p = t == CHAR? PCHAR:
		t == INT? PINT:
		t == STRUCT? PSTRUCT:
		t == UNION? PUNION:
		PVOID;

	// If a struct or union and no name given,
	// scan the next token to get it. Check this
	// is an IDENT token.
	if (PUNION == p || PSTRUCT == p) {
		if (!s) {
			Token = scan();
			copyname(sname, Text);
			s = sname;
			if (IDENT != Token) {
				error("struct/union name expected: %s", Text);
				return p;
			}
		}
		// Now check that this struct/union exists
		if ((y = findstruct(s)) == 0 || Syms[y].prim != p)
			error("no such struct/union: %s", s);
		// Add in the struct/union bitfield to the primitive type
		p |= y;
	}
	return p;
}

/*
 * pmtrdecl :=
 *	  ( )
 *	| ( pmtrlist )
 *	| ( pmtrlist , ... )
 *
 * pmtrlist :=
 *	  primtype declarator
 *	| primtype declarator , pmtrlist
 */

// Parse the declaration of a function's parameters
static int pmtrdecls(void) {
	char	name[NAMELEN+1];
	int	prim, type, size, na, addr;
	int	dummy;

	// We've already parsed the '('.
	// Do nothing if we get a ')' immediately.
	if (RPAREN == Token)
		return 0;

	// No arguments at present
	na = 0;
	addr = 2*BPW;	// XXX Why?

	// Loop until we find the ')'
	for (;;) {
		// XXX: binary invert the number of arguments
		// if we see a '...' token. Why?
		if (na > 0 && ELLIPSIS == Token) {
			Token = scan();
			na = -(na + 1);
			break;
		}
		// Parameter is an identifier, mark it as integer type
		else if (IDENT == Token) {
			prim = PINT;
		}
		// Otherwise it has to be a type specifier
		else {
			if (	Token != CHAR && Token != INT &&
				Token != VOID &&
				Token != STRUCT && Token != UNION
			) {
				// But it wasn't, so issue an error
				// and try to synchronise at the next ';'
				error("type specifier expected at: %s", Text);
				Token = synch(RPAREN);
				return na;
			}
			// It was a type specifier. Get its primitive type
			// and move up to the next token. Return when we
			// see a ')' and we had some arguments
			name[0] = 0;
			prim = primtype(Token, NULL);
			Token = scan();
			if (RPAREN == Token && prim == PVOID && !na)
				return 0;
		}

		// XXX: Not sure what's going on below yet
		size = 1;
		type = declarator(1, CAUTO, name, &prim, &size, &dummy,
				&dummy);
		addloc(name, prim, type, CAUTO, size, addr, 0);
		addr += BPW;

		// We have one more argument, scan past the following ','
		na++;
		if (COMMA == Token)
			Token = scan();
		else
			break;
	}
	return na;
}

// Given a primitive type value, return
// the type that is a pointer to it.
// Ensure we don't have too many levels
// of indirection. At worst, return a void pointer.
int pointerto(int prim) {
	int	y;

	if (CHARPP == prim || INTPP == prim || VOIDPP == prim ||
	    FUNPTR == prim ||
	    (prim & STCMASK) == STCPP || (prim & STCMASK) == UNIPP
	)
		error("too many levels of indirection", NULL);
	y = prim & ~STCMASK;
	switch (prim & STCMASK) {
	case PSTRUCT:	return STCPTR | y;
	case STCPTR:	return STCPP | y;
	case PUNION:	return UNIPTR | y;
	case UNIPTR:	return UNIPP | y;
	}
	return PINT == prim? INTPTR:
		PCHAR == prim? CHARPTR:
		PVOID == prim? VOIDPTR:
		INTPTR == prim? INTPP:
		CHARPTR == prim? CHARPP: VOIDPP;
}

/*
 * declarator :=
 *	  IDENT
 *	| * IDENT
 *	| * * IDENT
 *	| * IDENT [ constexpr ]
 *	| IDENT [ constexpr ]
 *	| IDENT = constexpr
 *	| IDENT [ ] = initlist
 *	| IDENT pmtrdecl
 *	| IDENT [ ]
 *	| * IDENT [ ]
 *	| ( * IDENT ) ( )
 */

// Parse the identifier part of a declarator
static int declarator(int pmtr, int scls, char *name, int *pprim, int *psize,
			int *pval, int *pinit)
{
	int	type = TVARIABLE;
	int	ptrptr = 0;

	if (STAR == Token) {
		Token = scan();
		*pprim = pointerto(*pprim);
		if (STAR == Token) {
			Token = scan();
			*pprim = pointerto(*pprim);
			ptrptr = 1;
		}
	}
	else if (LPAREN == Token) {
		if (*pprim != PINT)
			error("function pointers are limited to type 'int'",
				NULL);
		Token = scan();
		*pprim = FUNPTR;
		match(STAR, "(*name)()");
	}
	if (IDENT != Token) {
		error("missing identifier at: %s", Text);
		name[0] = 0;
	}
	else {
		copyname(name, Text);
		Token = scan();
	}
	if (FUNPTR == *pprim) {
		rparen();
		lparen();
		rparen();
	}
	if (!pmtr && ASSIGN == Token) {
		Token = scan();
		*pval = constexpr();
		if (PCHAR == *pprim)
			*pval &= 0xff;
		if (*pval && !inttype(*pprim))
			error("non-zero pointer initialization", NULL);
		*pinit = 1;
	}
	else if (!pmtr && LPAREN == Token) {
		Token = scan();
		*psize = pmtrdecls();
		rparen();
		return TFUNCTION;
	}
	else if (LBRACK == Token) {
		if (ptrptr)
			error("too many levels of indirection: %s", name);
		Token = scan();
		if (RBRACK == Token) {
			Token = scan();
			if (pmtr) {
				*pprim = pointerto(*pprim);
			}
			else {
				type = TARRAY;
				*psize = 1;
				if (ASSIGN == Token) {
					Token = scan();
					if (!inttype(*pprim))
						error("initialization of"
							" pointer array not"
							" supported",
							NULL);
					*psize = initlist(name, *pprim);
					if (CAUTO == scls)
						error("initialization of"
							" local arrays"
							" not supported: %s",
							name);
					*pinit = 1;
				}
				else if (CEXTERN != scls) {
					error("automatically-sized array"
						" lacking initialization: %s",
						name);
				}
			}
		}
		else {
			*psize = constexpr();
			if (*psize < 0) {
				error("invalid array size", NULL);
				*psize = 0;
			}
			type = TARRAY;
			rbrack();
		}
	}
	if (PVOID == *pprim)
		error("'void' is not a valid type: %s", name);
	return type;
}

/*
 * localdecls :=
 *        ldecl
 *      | ldecl localdecls
 *
 * ldecl :=
 *	  primtype ldecl_list ;
 *	| lclass primtype ldecl_list ;
 *	| lclass ldecl_list ;
 *	| enum_decl
 *	| struct_decl
 *
 * lclass :=
 *	| AUTO
 *	| EXTERN
 *	| REGISTER
 *	| STATIC
 *	| VOLATILE
 *
 * ldecl_list :=
 *	  declarator
 *	| declarator , ldecl_list
 */

static int localdecls(void) {
	char	name[NAMELEN+1];
	int	prim, type, size, addr = 0, val, ini;
	int	stat, extn;
	int	pbase, rsize;

	Nli = 0;
	while ( AUTO == Token || EXTERN == Token || REGISTER == Token ||
		STATIC == Token || VOLATILE == Token ||
		INT == Token || CHAR == Token || VOID == Token ||
		ENUM == Token ||
		STRUCT == Token || UNION == Token
	) {
		if (ENUM == Token) {
			enumdecl(0);
			continue;
		}
		extn = stat = 0;
		if (AUTO == Token || REGISTER == Token || STATIC == Token ||
			VOLATILE == Token || EXTERN == Token
		) {
			stat = STATIC == Token;
			extn = EXTERN == Token;
			Token = scan();
			if (	INT == Token || CHAR == Token ||
				VOID == Token ||
				STRUCT == Token || UNION == Token
			) {
				prim = primtype(Token, NULL);
				Token = scan();
			}
			else
				prim = PINT;
		}
		else {
			prim = primtype(Token, NULL);
			Token = scan();
		}
		pbase = prim;
		for (;;) {
			prim = pbase;
			if (eofcheck()) return 0;
			size = 1;
			ini = val = 0;
			type = declarator(0, CAUTO, name, &prim, &size,
					&val, &ini);
			rsize = objsize(prim, type, size);
			rsize = (rsize + INTSIZE-1) / INTSIZE * INTSIZE;
			if (stat) {
				addloc(name, prim, type, CLSTATC, size,
					label(), val);
			}
			else if (extn) {
				addloc(name, prim, type, CEXTERN, size,
					0, val);
			}
			else {
				addr -= rsize;
				addloc(name, prim, type, CAUTO, size, addr, 0);
			}
			if (ini && !stat) {
				if (Nli >= MAXLOCINIT) {
					error("too many local initializers",
						NULL);
					Nli = 0;
				}
				LIaddr[Nli] = addr;
				LIval[Nli++] = val;
			}
			if (COMMA == Token)
				Token = scan();
			else
				break;
		}
		semi();
	}
	return addr;
}

// Walk two integer lists and return the difference
// between the first different elements. The list
// ends with a zero element. This is used to
// compare the signature of an existing function
// against a new function signature.
static int intcmp(int *x1, int *x2) {
	while (*x1 && *x1 == *x2)
		x1++, x2++;
	return *x1 - *x2;
}

static void signature(int fn, int from, int to) {
	int	types[MAXFNARGS+1], i;

	if (to - from > MAXFNARGS)
		error("too many function parameters", Syms[fn].name);
	for (i=0; i<MAXFNARGS && from < to; i++)
		types[i] = Syms[--to].prim;
	types[i] = 0;
	if (NULL == Syms[fn].mtext) {
		Syms[fn].mtext = galloc((i+1) * sizeof(int), 1);
		memcpy(Syms[fn].mtext, types, (i+1) * sizeof(int));
	}
	else if (intcmp((int *) Syms[fn].mtext, types))
		error("declaration does not match prior prototype: %s",
			Syms[fn].name);
}

/*
 * decl :=
 *	  declarator { localdecls stmt_list }
 *	| decl_list ;
 *
 * decl_list :=
 *	  declarator
 *	| declarator , decl_list
 */

void decl(int clss, int prim) {
	char	name[NAMELEN+1];
	int	pbase, type, size = 0, val, init;
	int	lsize;

	pbase = prim;
	for (;;) {
		prim = pbase;
		val = 0;
		init = 0;
		type = declarator(0, clss, name, &prim, &size, &val, &init);
		if (TFUNCTION == type) {
			clss = clss == CSTATIC? CSPROTO: CEXTERN;
			Thisfn = addglob(name, prim, type, clss, size, 0,
					NULL, 0);
			signature(Thisfn, Locs, NSYMBOLS);
			if (LBRACE == Token) {
				clss = clss == CSPROTO? CSTATIC:
					clss == CEXTERN? CPUBLIC: clss;
				Thisfn = addglob(name, prim, type, clss, size,
					0, NULL, 0);
				Token = scan();
				lsize = localdecls();
				gentext();
				if (CPUBLIC == clss) genpublic(name);
				genaligntext();
				genname(name);
				genentry();
				genstack(lsize);
				genlocinit();
				Retlab = label();
				compound(0);
				genlab(Retlab);
				genstack(-lsize);
				genexit();
				if (O_debug & D_LSYM)
					dumpsyms("LOCALS: ", name, Locs,
						NSYMBOLS);
			}
			else {
				semi();
			}
			clrlocs();
			return;
		}
		if (CEXTERN == clss && init) {
			error("initialization of 'extern': %s", name);
		}
		addglob(name, prim, type, clss, size, val, NULL, init);
		if (COMMA == Token)
			Token = scan();
		else
			break;
	}
	semi();
}

/*
 * structdecl :=
 *	  STRUCT identifier { member_list } ;
 *
 * member_list :=
 *	  primtype mdecl_list ;
 *	| primtype mdecl_list ; member_list
 *
 * mdecl_list :=
 *	  declarator
 *	| declatator , mdecl_list
 */

// Parse the definition of a struct or union.
// clss is the storage class already specified.
// uniondecl is true if this is a union declaration.
void structdecl(int clss, int uniondecl) {
	int	base, prim, size, dummy, type, addr = 0;
	char	name[NAMELEN+1], sname[NAMELEN+1];
	int	y, usize = 0;

	// Get the name of the struct/union and copy it
	// into sname. Confirm this was an identifier
	// and get the next token next token
	Token = scan();
	copyname(sname, Text);
	ident();

	// The next token wasn't an '{', so this must be the
	// declaration of a variable. Check that such a struct/union
	// exists with primtype(). Then call decl() to parse the
	// variable's definition.
	if (Token != LBRACE) {
		prim = primtype(uniondecl? UNION: STRUCT, sname);
		decl(clss, prim);
		return;
	}

	// This is a struct/union definition. Add a symbol for
	// it in the global symbol table.
	y = addglob(sname, uniondecl? PUNION: PSTRUCT, TSTRUCT,
			CMEMBER, 0, 0, NULL, 0);

	// Loop reading in the member definitions for the struct/union
	Token = scan();
	while (	INT == Token || CHAR == Token || VOID == Token ||
		STRUCT == Token || UNION == Token
	) {
		// Convert the token into a type value: PCHAR, PINT etc.
		base = primtype(Token, NULL);
		size = 0;
		Token = scan();
		for (;;) {
			if (eofcheck()) return;
			// XXX Not sure why prim wasn't used 5 lines above
			prim = base;
			// Parse and verify the member's name
			type = declarator(1, clss, name, &prim, &size,
						&dummy, &dummy);
			// Add it as a member of the struct/union
			addglob(name, prim, type, CMEMBER, size, addr,
				NULL, 0);
			// Get the member's size in bytes
			size = objsize(prim, type, size);
			if (size < 0)
				error("size of struct/union member"
					" is unknown: %s",
					name);
			// If a union, keep the size of the biggest member
			if (uniondecl) {
				usize = size > usize? size: usize;
			}
			// Otherwise add on member's size to the total size.
			// Ensure this is aligned to the size of an int.
			else {
				addr += size;
				addr = (addr + INTSIZE-1) / INTSIZE * INTSIZE;
			}
			// Loop back on a ',' for another member, else stop
			if (Token != COMMA) break;
			Token = scan();
		}
		semi();		// Finally parse the ';' at the end
	}

	// The definition must end with a '}' ';'
	// Set up the size of the struct/union in bytes
	rbrace();
	semi();
	Syms[y].size = uniondecl? usize: addr;
}

/*
 * top :=
 *	  ENUM enumdecl
 *	| decl
 *	| primtype decl
 *	| storclass decl
 *	| storclass primtype decl
 *
 * storclass :=
 *	  EXTERN
 *	| STATIC
 */

// Parse the top of a C file
void top(void) {
	// Assume the default class is public
	int	prim, clss = CPUBLIC;

	// Change the class to extern or static based on the next token.
	// Ignore the keyword volatile.
	switch (Token) {
	case EXTERN:	clss = CEXTERN; Token = scan(); break;
	case STATIC:	clss = CSTATIC; Token = scan(); break;
	case VOLATILE:	Token = scan(); break;
	}

	// If we get an enum, struct or union, parse them separately
	switch (Token) {
	case ENUM:
		enumdecl(1);
		break;
	case STRUCT:
	case UNION:
		structdecl(clss, UNION == Token);
		break;

	// Set the primitive type to char, int or void
	// Skip past this token and use decl() to parse the
	// rest of the declaration
	case CHAR:
	case INT:
	case VOID:
		prim = primtype(Token, NULL);
		Token = scan();
		decl(clss, prim);
		break;
	// If we get an identifier with no type specifier, assume int
	case IDENT:
		decl(clss, PINT);
		break;
	// Error as we didn't get a type specifier
	default:
		error("type specifier expected at: %s", Text);
		Token = synch(SEMI);
		break;
	}
}

// Print out the compile statistics
// at the end of the compilation
static void stats(void) {
	printf(	"Memory usage: "
		"Symbols: %5d/%5d, "
		"Names: %5d/%5d, "
		"Nodes: %5d/%5d\n",
		Globs, NSYMBOLS,
		Nbot, POOLSIZE,
		Ndmax, NODEPOOLSZ);
}

// Define a macro given on the command line
void defarg(char *s) {
	char	*p;

	if (NULL == s) return;
	if ((p = strchr(s, '=')) != NULL)
		*p++ = 0;
	else
		p = "";
	addglob(s, 0, TMACRO, 0, 0, 0, globname(p), 0);
	if (*p) *--p = '=';
}

// Parse the specified file and generate assembly output
void program(char *name, FILE *in, FILE *out, char *def) {

	init();				// Initialise all variables
	defarg(def);			// Define any -D from the cmd line
	Infile = in;			// Set the current in/out FILE pointers
	Outfile = out;
	File = Basefile = name;		// Set the input file's name
	genprelude();			// Generate the assembly prelude
	Token = scan();			// Get the first token from the input
	while (XEOF != Token)		// Until we hit the end of the file
		top();
	genpostlude();			// Generate the assembly postlude
					// Optionally, dump symbols and stats
	if (O_debug & D_GSYM) dumpsyms("GLOBALS", "", 1, Globs);
	if (O_debug & D_STAT) stats();
}
