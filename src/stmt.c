/*
 *	NMH's Simple C Compiler, 2011--2016
 *	Statement parser
 */

#include "defs.h"
#include "data.h"
#include "decl.h"

static void stmt(void);

/*
 * compound :=
 *	  { stmt_list }
 *	| { }
 *
 * stmt_list:
 *	  stmt
 *	| stmt stmt_list
 */

void compound(int lbr) {
	if (lbr) Token = scan();	// Get next token if still on the '{'
	while (RBRACE != Token) {	// Until we get the '}'
		if (eofcheck()) return;	// Return on EOF
		stmt();			// Parse the statement within
	}
	Token = scan();			// And get the token after the '}'
}

// XXX: Comment
static void pushbrk(int id) {
	if (Bsp >= MAXBREAK)
		fatal("too many nested loops/switches");
	Breakstk[Bsp++] = id;
}

// XXX: Comment
static void pushcont(int id) {
	if (Csp >= MAXBREAK)
		fatal("too many nested loops/switches");
	Contstk[Csp++] = id;
}

/*
 * break_stmt := BREAK ;
 */

static void break_stmt(void) {
	// Read the break
	// Error check if nothing on the break stack
	// Generate a jump to the location on top of the break stack
	// and skip past the semicolon
	Token = scan();
	if (!Bsp) error("'break' not in loop/switch context", NULL);
	genjump(Breakstk[Bsp-1]);
	semi();
}

/*
 * continue_stmt := CONTINUE ;
 */

static void continue_stmt(void) {
	// Read the break
	// Error check if nothing on the continue stack
	// Generate a jump to the location on top of the continue stack
	// and skip past the semicolon
	Token = scan();
	if (!Csp) error("'continue' not in loop context", NULL);
	genjump(Contstk[Csp-1]);
	semi();
}

/*
 * do_stmt := DO stmt WHILE ( expr ) ;
 */

static void do_stmt(void) {
	int	ls, lb, lc;

	Token = scan();			// Skip the 'do' token
	ls = label();			// Get a label for the top of loop
	pushbrk(lb = label());		// Get labels for breaks and continues
	pushcont(lc = label());		// & push them on the respective stack
	genlab(ls);			// Generate the top label
	stmt();				// Parse the statement
	match(WHILE, "'while'");	// Get the 'while' token
	lparen();			// Get the '(' token
	genlab(lc);			// Generate the continue label
	rexpr();			// Parse the expression
	genbrtrue(ls);			// Generate "branch if true to top"
	clear(1);			// Primary register is now empty
	genlab(lb);			// Generate the break label
	rparen();			// Get the ')' and ';' tokens
	semi();
	Bsp--;				// And pop the break and continue
	Csp--;				// labels from their stacks
}

/*
 * for_stmt :=
 *	FOR ( opt_expr ; opt_expr ; opt_expr ) stmt
 *
 * opt_expr :=
 *	| expr
 */

static void for_stmt(void) {
	int	ls, lbody, lb, lc;

	Token = scan();			// Skip the 'for' token
	ls = label();			// Get a label for the top of loop
	lbody = label();		// and for the start of the inner statement
	pushbrk(lb = label());		// Get labels for breaks and continues
	pushcont(lc = label());		// & push them on the respective stack
	lparen();			// Get the '(' token
	if (Token != SEMI) {		// Parse any first expression
		rexpr();
		clear(1);		// Primary register is now empty
	}
	semi();				// Get the ';' token
	genlab(ls);			// Generate the top label
	if (Token != SEMI) {		// Parse any second expression
		rexpr();
		genbrfalse(lb);		// Generate branch if false to break label
		clear(1);		// Primary register is now empty
	}
	genjump(lbody);
	semi();				// Get the ';' token
	genlab(lc);			// Generate the continue label
	if (Token != RPAREN)
		rexpr();
	clear(1);			// Primary register is now empty
	genjump(ls);			// Generate jump to start of middle expression
	rparen();			// Get the ')' token
	genlab(lbody);			// Generate label for start of inner statement
	stmt();				// Parse the inner statement
	genjump(lc);			// Generate jump back to third expression code
	genlab(lb);			// Generate the break label
	Bsp--;				// And pop the break and continue
	Csp--;				// labels from their stacks
}

/*
 * if_stmt :=
 *	  IF ( expr ) stmt
 *	| IF ( expr ) stmt ELSE stmt
 */

static void if_stmt(void) {
	int	l1, l2;

	Token = scan();			// Skip the 'for' token
	lparen();			// Get the '(' token
	rexpr();			// Parse the expression
	l1 = label();			// Get the label for non-true code
	genbrfalse(l1);			// Generate branch if false to this label
	clear(1);			// Primary register is now empty
	rparen();			// Get the ')' token
	stmt();				// Parse the statement
	if (ELSE == Token) {		// If we have an 'else' token
		l2 = label();		// Get a second label. Swap l1/l2 so the
		genjump(l2);		// new label is the non-false code. Generate
		genlab(l1);		// the jump to the end (l2) code. Generate
		l1 = l2;		// the false code (l1 label). 
		Token = scan();		// Skip the 'else' token and
		stmt();			// Parse the inner statement
	}
	genlab(l1);			// Generate the label for non-true code
}

/*
 * return_stmt :=
 *	  RETURN ;
 *	| RETURN expr ;
 */

static void return_stmt(void) {
	int	lv[LV];

	Token = scan();			// Skip the 'return' token
	if (Token != SEMI) {		// If there's an expression
		expr(lv, 1);		// Parse it and check its type matches
					// this function
		if (!typematch(lv[LVPRIM], Prims[Thisfn]))
			error("incompatible type in 'return'", NULL);
	}
					// No return value, so check
	else {				// that this is a void function
		if (Prims[Thisfn] != PVOID)
			error("missing value after 'return'", NULL);
	}
	genjump(Retlab);		// Generate the jump to the function return
	semi();				// label and get the ';' token
}

/*
 * switch_stmt :=
 *	  SWITCH ( expr ) { switch_block }
 *
 * switch_block :=
 *	  switch_block_stmt
 *	| switch_block_stmt switch_block
 *
 * switch_block_stmt :=
 *	  CASE constexpr :
 *	| DEFAULT :
 *	| stmt
 */

static void switch_block(void) {
	int	lb, ls, ldflt = 0;
	int	cval[MAXCASE];		// List of case values and case labels
	int	clab[MAXCASE];
	int	nc = 0;			// Start with no cases found yet

	Token = scan();			// Skip the '{' token
	pushbrk(lb = label());		// Get a label for the end of the block
					// and add it to the break queue
	ls = label();			// Generate a label for the top of the block
	genjump(ls);
	while (RBRACE != Token) {	// Until we find the closing '}'
		if (eofcheck()) return;
					// Error if too many cases
		if ((CASE == Token || DEFAULT == Token) && nc >= MAXCASE) {
			error("too many 'case's in 'switch'", NULL);
			nc = 0;
		}
		if (CASE == Token) {		// Find and skip a 'case' token
			Token = scan();
			cval[nc] = constexpr();	// Get the case expression
						// and generate a label for it
			genlab(clab[nc++] = label());
			colon();		// Get the ':' token
		}
		else if (DEFAULT == Token) {	// Find and skip a 'default' token
			Token = scan();
			ldflt = label();	// Get a label for the default case
			genlab(ldflt);		// and output this label
			colon();		// Get the ':' token
		}
		else
			stmt();			// Otherwise parse a statement
	}
	if (!nc) {				// If we have some case statements
		if (ldflt) {			// Add the default label with
			cval[nc] = 0;		// value to the value/label lists
			clab[nc++] = ldflt;
		}
		else
			error("empty switch", NULL);	// Error if no cases
	}
	genjump(lb);			// Output a jump to the end label, I assume
					// for when no cases match? XXX Check
	genlab(ls);			// Generate the top label
	genswitch(cval, clab, nc, ldflt? ldflt: lb);	// Generate the jump table
	gentext();			// Go back to the text segment
	genlab(lb);			// Generate the bottom label
	Token = scan();			// Get the next token
	Bsp--;				// And pop the break label from the stack
}

static void switch_stmt(void) {
	Token = scan();			// Skip the 'while' token
	lparen();			// Get the '(' token
	rexpr();			// Parse the expression
	commit();			// Flush the insruction queue
	clear(0);			// Primary register is now empty, but
					// don't touch the instruction queue.
					// XXX Why not?
	rparen();			// Get the ')' token
	if (Token != LBRACE)		// Syntax error if '{' not the next token
		error("'{' expected after 'switch'", NULL);
	switch_block();			// Now parse the switch block itself
}

/*
 * while_stmt := WHILE ( expr ) stmt
 */

static void while_stmt(void) {
	int	lb, lc;

	Token = scan();			// Skip the 'while' token
	pushbrk(lb = label());		// Get labels for breaks and continues
	pushcont(lc = label());		// & push them on the respective stack
	genlab(lc);			// Generate the continue label
	lparen();			// Get the '(' token
	rexpr();			// Parse the expression
	genbrfalse(lb);			// Generate branch if false to end of loop
	clear(1);			// Primary register is now empty
	rparen();			// Get the ')' token
	stmt();				// Parse the inner statement
	genjump(lc);			// Generate jump back to top of loop
	genlab(lb);			// Generate the end of loop label
	Bsp--;				// And pop the break and continue
	Csp--;				// labels from their stacks
}

// Print out a message that a keyword is used in the
// wrong context. Skip that token and scan up to a ':'.
static void wrong_ctx(int t) {
	if (DEFAULT == t) {
		error("'default' not in 'switch' context", NULL);
		Token = scan();
		colon();
	}
	else {
		error("'case' not in 'switch' context", NULL);
		Token = scan();
		constexpr();
		colon();
	}
}

/*
 * stmt :=
 *	  break_stmt
 *	| continue_stmt
 *	| do_stmt
 *	| for_stmt
 *	| if_stmt
 *	| return_stmt
 *	| switch_stmt
 *	| while_stmt
 *	| compound
 *	| ;
 *	| expr ;
 */

// Parse all the possible C statements
// using the above functions
static void stmt(void) {
	int	lv[LV];

	switch (Token) {
	case BREAK:	break_stmt(); break;
	case CONTINUE:	continue_stmt(); break;
	case DO:	do_stmt(); break;
	case FOR:	for_stmt(); break;
	case IF:	if_stmt(); break;
	case RETURN:	return_stmt(); break;
	case SWITCH:	switch_stmt(); break;
	case WHILE:	while_stmt(); break;
	case LBRACE:	compound(1); break;
	case SEMI:	Token = scan(); break;

	// Can't have 'default' or 'case' outside a 'switch' statement
	case DEFAULT:	wrong_ctx(DEFAULT); break;
	case CASE:	wrong_ctx(CASE); break;

	// Not a statement, try parsing it as an expression followed
	// by a semicolon. Flush the insruction queue.
	default:	expr(lv, 0); semi(); commit(); break;
	}
	clear(1);	// Primary register is now empty
}
