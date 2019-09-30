/*
 *	NMH's Simple C Compiler, 2011--2016
 *	Expression parser
 */

#include "defs.h"
#include "data.h"
#include "decl.h"
#include "prec.h"

static node *asgmnt(struct lvalue *lv);
static node *cast(struct lvalue *lv);
static node *exprlist(struct lvalue *lv, int ckvoid);

// Convert an lvalue AST node into an rvalue node. If this really is an
// lvalue, generate an OP_RVAL AST node to get the symbol's value which
// is of type prim. Otherwise, just return this node as an rvalue.
static node *rvalue(node *n, struct lvalue *lv) {
	if (lv->addr) {
		lv->addr = 0;
		return mkunop2(OP_RVAL, lv->prim, lv->sym, n);
	}
	else {
		return n;
	}
}

/*
 * primary :=
 *	  IDENT
 *	| INTLIT
 *	| string
 *	| ARGC
 *	| ( expr )
 *
 * string :=
 *	  STRLIT
 *	| STRLIT string
 */

// Given an lvalue node, parse a primary factor and return an
// AST node representing it.
static node *primary(struct lvalue *lv) {
	node	*n = NULL;
	int	y, lab, k;
	char	name[NAMELEN+1];

	// Start with it empty
	lv->prim = lv->sym = lv->addr = 0;

	switch (Token) {
	case IDENT:
		// Find the identifier in the symbol table,
		// then copy it so we can scan the next token
		y = findsym(Text);
		copyname(name, Text);
		Token = scan();

		// No symbol found. If the next token is a '(',
		// assume this is a function declaration. Add
		// it as an extern int function to the symbol table.
		// Otherwise, it's an undeclared variable. It's still
		// added to the symbol table as an auto int, to reduce
		// the number of further errors when the variable is used.
		if (!y) {
			if (LPAREN == Token) {
				y = addglob(name, PINT, TFUNCTION, CEXTERN,
					-1, 0, NULL, 0);
			}
			else {
				error("undeclared variable: %s", name);
				y = addloc(name, PINT, TVARIABLE, CAUTO,
					0, 0, 0);
			}
		}

		// Save the symbol and primary type of the symbol
		lv->sym = y;
		lv->prim = Syms[y].prim;

		// If the next token isn't a '(', it's a function
		// pointer, so make a function pointer node
		// for the given symbol
		if (TFUNCTION == Syms[y].type) {
			if (LPAREN != Token) {
				lv->prim = FUNPTR;
				n = mkleaf(OP_ADDR, y);
			}
			return n;
		}

		// Constant: make an OP_LIT node with the value
		if (TCONSTANT == Syms[y].type) {
			return mkleaf(OP_LIT, Syms[y].val);
		}

		// Array: make an OP_ADDR node with the
		// type being a pointer to the array's primitive type
		if (TARRAY == Syms[y].type) {
			n = mkleaf(OP_ADDR, y);
			lv->prim = pointerto(lv->prim);
			return n;
		}

		// If it's a struct or union, make an
		// OP_ADDR node to the struct/union member
		if (comptype(Syms[y].prim)) {
			n = mkleaf(OP_ADDR, y);
			lv->sym = 0;	// But not to the original
			return n;	// struct/union member
		}

		// An ordinary scalar variable, so make an IDENT node
		// for the symbol, and mark it as a true lvalue
		n = mkleaf(OP_IDENT, y);
		lv->addr = 1;
		return n;
	case INTLIT:
		// Literal integer, make an OP_LIT node with the value
		// and an int primary type. Move up to the next token.
		n = mkleaf(OP_LIT, Value);
		Token = scan();
		lv->prim = PINT;
		return n;
	case STRLIT:
		gendata();			// Switch to the data section
		lab = label();			// Make a new label and
		genlab(lab);			// output it
		k = 0;
		while (STRLIT == Token) {	// Allow successive "x" "y"
			gendefs(Text, Value);	// string literals
			k += Value-2;		// XXX: length - ""?
			Token = scan();		// until we hit something else
		}
		gendefb(0);			// Output the NUL terminator
		genalign(k+1);			// Align on a word boundary
		n = mkleaf(OP_LDLAB, lab);	// Node refers to the label
		lv->prim = CHARPTR;		// and is a char pointer
						// Not marked as addr=1
						// as the string is mutable
		return n;
	case LPAREN:
		Token = scan();			// Skip the '('
		n = exprlist(lv, 0);		// Parse the expression list
		rparen();			// Match the ')' and
		return n;			// return the node
	default:
		// It's a syntax error, skip tokens up to the next semicolon
		error("syntax error at: %s", Text);
		Token = synch(SEMI);
		return NULL;
	}
}

// Return true if the two types match.
// If they are identical, they match.
// Two int types (int and char) match.
// Any pointer matches a void pointer.
int typematch(int p1, int p2) {
	if (p1 == p2) return 1;
	if (inttype(p1) && inttype(p2)) return 1;
	if (!inttype(p1) && VOIDPTR == p2) return 1;
	if (VOIDPTR == p1 && !inttype(p2)) return 1;
	return 0;
}

/*
 * fnargs :=
 *	  asgmnt
 *	| asgmnt , fnargs
 */

// Parse the arguments for a function whose name
// is in the symbol table at position fn.
// Return the number of arguments in na and
// a sub-tree pointer as the return value.
static node *fnargs(int fn, int *na) {
	struct lvalue lv;
	int	*types;
	char	msg[100];
	int	sgn[MAXFNARGS+1];
	node	*n = NULL, *n2;

	// Get the list of types for each argument
	// if known, otherwise set empty for now
	types = (int *) (fn? Syms[fn].mtext: NULL);

	// Start with zero arguments, parse until a ')'
	*na = 0;
	while (RPAREN != Token) {
		// Parse the argument expression and
		// convert it into an rvalue. Join
		// the sequence of argument AST nodes
		// together with OP_GLUE nodes.
		n2 = asgmnt(&lv);
		n2 = rvalue(n2, &lv);
		n = mkbinop(OP_GLUE, n, n2);

		// SubC doesn't allow struct/union arguments.
		// Convert to a pointer to the struct/union.
		if (comptype(lv.prim)) {
			error("struct/union passed by value", NULL);
			lv.prim = pointerto(lv.prim);
		}

		// For this argument, check its type against
		// the type value from the function signature
		// i.e. the types pointer. Error if don't match.
		if (types && *types) {
			if (!typematch(*types, lv.prim)) {
				sprintf(msg, "wrong type in argument %d"
					" of call to: %%s",
					*na+1);
				error(msg, Syms[fn].name);
			}
			// Move up to type of next argument
			types++;
		}

		// In case the function doesn't have a prototype,
		// build our own list of types for each argument in sgn[]
		if (*na < MAXFNARGS) sgn[*na] = lv.prim, sgn[*na+1] = 0;
		(*na)++;
		// Scan the comma after the argument, but it's a syntax
		// error if we get a ')' after the comma
		if (COMMA == Token) {
			Token = scan();
			if (RPAREN == Token)
				error("trailing ',' in function call", NULL);
		}
		else
			break;
	}
	if (fn && TFUNCTION == Syms[fn].type && !Syms[fn].mtext) {
		Syms[fn].mtext = galloc((*na+1) * sizeof(int), 1);
		memcpy(Syms[fn].mtext, sgn, (*na+1) * sizeof(int));
	}
	rparen();
	return n;
}

// Given a primitive type p, return
// the type which is a pointer to p,
// or -1 if no pointer can be formed
int deref(int p) {
	int	y;

	switch (p) {
	case INTPP:	return INTPTR;
	case INTPTR:	return PINT;
	case CHARPP:	return CHARPTR;
	case CHARPTR:	return PCHAR;
	case VOIDPP:	return VOIDPTR;
	case VOIDPTR:	return PCHAR;
	case FUNPTR:	return PCHAR;
	}
	y = p & ~STCMASK;
	switch (p & STCMASK) {
	case STCPP:	return STCPTR | y;
	case STCPTR:	return PSTRUCT | y;
	case UNIPP:	return UNIPTR | y;
	case UNIPTR:	return PUNION | y;
	}
	return -1;
}

// Given an AST node n, return an rvalue node
// which points at this node. Also set lv
// to reflect the type of this pointer
static node *indirection(node *n, struct lvalue *lv) {
	int	p;

	// Convert any lvalue to a value
	n = rvalue(n, lv);

	// Prevent void pointer dereferences
	if (VOIDPTR == lv->prim)
		error("dereferencing void pointer", NULL);

	// If a pointer to the primitive type cannot be formed,
	// it's an error
	if ((p = deref(lv->prim)) < 0) {
		if (lv->sym)
			error("indirection through non-pointer: %s",
				Syms[lv->sym].name);
		else
			error("indirection through non-pointer", NULL);
		p = lv->prim;
	}

	// Otherwise set the dereferenced type with no lvalue symbol.
	// XXX: is it safe to leave addr unchanged, why not set to 0?
	lv->prim = p;
	lv->sym = 0;
	return n;
}

// Generic function to print an error
// message for a bad function call.
static void badcall(struct lvalue *lv) {
	if (lv->sym)
		error("call of non-function: %s",
			Syms[lv->sym].name);
	else
		error("call of non-function", NULL);
}

// Return true if the number of arguments to
// a function is OK. Either match the prototype,
// or XXX I'm not sure!
static int argsok(int na, int nf) {
	return na == nf || (nf < 0 && na >= -nf-1);
}

// Given an AST node n which is a struct/union, the type
// of this struct/union in lv, and if the access is a pointer
// (->) or non-pointer access (.) in ptr, return an AST
// node which gives access to the member that has just been scanned it.
static node *stc_access(node *n, struct lvalue *lv, int ptr) {
	int	y, p;
	node	*n2;

	// Back up the AST node in case we have to return it.
	// Get the type of struct/union this is. It has an address.
	n2 = n;
	p = lv->prim & STCMASK;
	lv->addr = 1;

	// If the last token isn't an identifier, we can't parse this.
	if (IDENT != Token) {
		Token = scan();
		error("struct/union member name expected after '%s'",
			ptr? "->": ".");
		return NULL;
	}

	// Find the member field in the struct/union.
	// Error if it doesn't exist
	y = findmem(lv->prim & ~STCMASK, Text);
	if (0 == y)
		error("struct/union has no such member: %s", Text);

	// If we have the offset of the member, build an OP_ADD
	// node with the OP_LIT value as its child.
	if ((PSTRUCT == p || STCPTR == p) && Syms[y].val) {
		n2 = mkleaf(OP_LIT, Syms[y].val);
		n2 = mkbinop(OP_ADD, n, n2);
	}
	
	// Read in the next token
	Token = scan();

	// Get the member's primitive type. If it's an array,
	// get a pointer to the base of the array.
	p = Syms[y].prim;
	if (TARRAY == Syms[y].type) {
		p = pointerto(p);
		lv->addr = 0;
	}

	// Update the expression's primitive type and
	// return the new tree that we constructed
	lv->prim = p;
	return n2;
}

/*
 * postfix :=
 *	  primary
 *	| postfix [ expr ]
 *	| postfix ( )
 *	| postfix ( fnargs )
 *	| postfix ++
 *	| postfix --
 *	| postfix . identifier
 *	| postfix -> identifier
 */

// Parse a postfix expression and return 
// a sub-tree representing it. Also return
// the lvalue details in lv.
static node *postfix(struct lvalue *lv) {
	node	*n = NULL, *n2;
	int	p, na;
	struct lvalue lv2;

	// Parse the primary expression first
	n = primary(lv);

	// Loop until we run out of postfix operators
	for (;;) {
		switch (Token) {
		case LBRACK:
			// Index into an array.
			// We might have several array indices
			while (LBRACK == Token) {
				// Perform an indirection on the
				// expression and get the new type
				n = indirection(n, lv);

				// Scan the following expression
				// parse it as an expression list
				// and get its rvalue and type in lv2
				Token = scan();
				n2 = exprlist(&lv2, 1);
				n2 = rvalue(n2, &lv2);
				p = lv->prim;

				// Error if not an integer
				if (PINT != lv2.prim)
					error("non-integer subscript", NULL);

				// Scale the fixed index by the size of
				// the elements in the array
				if (    PINT == p || INTPTR == p ||
					CHARPTR == p || VOIDPTR == p ||
					STCPTR == (p & STCMASK) ||
					UNIPTR == (p & STCMASK)
				) {
					n2 = mkunop(OP_SCALE, n2);
				}
				// Or scale the variable by the size of
				// the elements in the array
				else if (comptype(p)) {
					n2 = mkunop1(OP_SCALEBY,
						objsize(p, TVARIABLE, 1), n2);
				}

				// OP_ADD means add the array base address
				// plus the offset
				n = mkbinop(OP_ADD, n, n2);

				// Get the final right bracket, set that we
				// have an address for this expression but no
				// symbol as it's an offset into the array
				rbrack();
				lv->sym = 0;
				lv->addr = 1;
			}
			break;
		case LPAREN:
			// Function call. Move up to the first argument.
			Token = scan();

			// Scan and check all the arguments against any
			// existing function prototype.
			n = fnargs(lv->sym, &na);

			// Check we have the correct number of arguments
			if (lv->sym && TFUNCTION == Syms[lv->sym].type) {
				if (!argsok(na, Syms[lv->sym].size))
					error("wrong number of arguments: %s",
						Syms[lv->sym].name);

				// Make an OP_CALL node to the function
				// with the # of args in na and the list pointer in n
				n = mkunop2(OP_CALL, lv->sym, na, n);
			}
			else {
				// It's a function pointer call Make an
				//OP_CALR node instead but with same arguments
				if (lv->prim != FUNPTR) badcall(lv);
				n = mkunop2(OP_CALR, lv->sym, na, n);
				lv->prim = PINT;	// XXX: why PINT?
			}
			lv->addr = 0;			// Expression is an rvalue
			break;
		case INCR:
		case DECR: 
			// We must have an address (i.e a real lvalue)
			// otherwise it's an error. Can't ++ an rvalue!
			if (lv->addr) {
				if (INCR == Token)
					// Build a OP_POSTINC/DEC node
					// to change the symbol and return
					// the symbol's type
					n = mkunop2(OP_POSTINC, lv->prim,
						lv->sym, n);
				else
					n = mkunop2(OP_POSTDEC, lv->prim,
						lv->sym, n);
			}
			else
				error("lvalue required before '%s'", Text);

			// Get the next token and mark new tree as an rvalue
			Token = scan();
			lv->addr = 0;
			break;
		case DOT:
			// Struct/union member access, get the member's name
			Token = scan();
			// Construct the tree to access this member's value,
			// otherwise the expression before the '.' wasn't struct/union
			if (comptype(lv->prim))
				n = stc_access(n, lv, 0);
			else
				error("struct/union expected before '.'",
					NULL);
			break;
		case ARROW:
			// Struct/union pointer access, get the member's name
			Token = scan();
			p = lv->prim & STCMASK;

			// Error if expression before -> wasn't a pointer,
			// otherwise get the rvalue of the right-hand side and
			// construct the tree to access this member's value
			if (p == STCPTR || p == UNIPTR) {
				n = rvalue(n, lv);
				n = stc_access(n, lv, 1);
			}
			else
				error(
				 "struct/union pointer expected before '->'",
				 NULL);
			lv->sym = 0;
			break;
		default:
			// An ordinary primary expression, return its tree
			return n;
		}
	}
}

static node *prefix(struct lvalue *lv);

// Scan in type information following a "sizeof" and
// return an OP_LIT node with the size of the type.
static node *comp_size(void) {
	int	k = 0, y;
	struct lvalue lv;

	// For most keywords, set their size by hand.
	// Change to PTRSIZE if the keyword is followed by a '*'
	if (	CHAR == Token || INT == Token || VOID == Token ||
		STRUCT == Token || UNION == Token
	) {
		switch (Token) {
		case CHAR:	k = CHARSIZE; break;
		case INT:	k = INTSIZE; break;
		case STRUCT:
		case UNION:	k = primtype(Token, NULL);
				k = objsize(k, TVARIABLE, 1);
				break;
		}
		Token = scan();
		if (STAR == Token) {
			k = PTRSIZE;
			Token = scan();
			if (STAR == Token) Token = scan();
		}
		else if (0 == k) {
			error("sizeof(void) is unknown", NULL);
		}
	}

	// Otherwise it's a prefix expression. So parse that to
	// get the named symbol. Use objsize() to look up that
	// symbol's size, or an error if we can't look it up.
	else {
		prefix(&lv);
		y = lv.sym? lv.sym: 0;
		k = y? objsize(Syms[y].prim, Syms[y].type, Syms[y].size):
			objsize(lv.prim, TVARIABLE, 1);
		if (0 == k)
			error("cannot compute sizeof: %s",
				Text);
	}

	// Return the leaf node with this size
	return mkleaf(OP_LIT, k);
}

/*
 * prefix :=
 *	  postfix
 *	| ++ prefix
 *	| -- prefix
 *	| & cast
 *	| * cast
 *	| + cast
 *	| - cast
 *	| ~ cast
 *	| ! cast
 *	| SIZEOF ( type )
 *	| SIZEOF ( type * )
 *	| SIZEOF ( type * * )
 *	| SIZEOF ( IDENT )
 *
 * type :=
 *	  INT
 *	| CHAR
 *	| VOID
 *	| STRUCT IDENT
 *	| UNION IDENT
 */

// Parse a prefix expression and return 
// a sub-tree representing it. Also return
// the lvalue details in lv.
static node *prefix(struct lvalue *lv) {
	node	*n;
	int	t;

	switch (Token) {
	case INCR:
	case DECR:
		// Keep the old token and parse the prefix expression
		// after the ++ or -- using prefix() i.e. recursively.
		// Get the lvalue.
		t = Token;
		Token = scan();
		n = prefix(lv);
		// If it has an address (a real lvalue) build a
		// OP_PREINC or OP_PREDEC node with the lvalue symbol.
		// No address: an error
		if (lv->addr) {
			if (INCR == t)
				n = mkunop2(OP_PREINC, lv->prim,
					lv->sym, n);
			else
				n = mkunop2(OP_PREDEC, lv->prim,
					lv->sym, n);
		}
		else {
			error("lvalue expected after '%s'",
				t == INCR? "++": "--");
		}

		// Set the address to zero as it's now an rvalue
		lv->addr = 0;
		return n;
	case STAR:
		// Get the next token and scan the cast expression.
		// Convert that expression's type into a "value at"
		// type with indirection. This must point to an
		// address, so set the lvalue address to 1.
		Token = scan();
		n = cast(lv);
		n = indirection(n, lv);
		lv->addr = 1;
		return n;

	case PLUS:
		// Get the next token and scan the cast expression.
		// Make this into an rvalue. It has no address.
		Token = scan();
		n = cast(lv);
		n = rvalue(n, lv);
		if (!inttype(lv->prim))
			error("bad operand to unary '+'", NULL);
		lv->addr = 0;
		return n;
	case MINUS:
		// Get the next token and scan the cast expression.
		// Make this into an rvalue.
		// Add an OP_NEG node with the expression as child.
		Token = scan();
		n = cast(lv);
		n = rvalue(n, lv);
		if (!inttype(lv->prim))
			error("bad operand to unary '-'", NULL);
		n = mkunop(OP_NEG, n);
		lv->addr = 0;
		return n;
	case TILDE:
		// Get the next token and scan the cast expression.
		// Make this into an rvalue.
		// Add an OP_NOT node with the expression as child
		// to bitwise invert the expression.
		Token = scan();
		n = cast(lv);
		n = rvalue(n, lv);
		if (!inttype(lv->prim))
			error("bad operand to '~'", NULL);
		n = mkunop(OP_NOT, n);
		lv->addr = 0;
		return n;
	case XMARK:
		// Get the next token and scan the cast expression.
		// Make this into an rvalue.
		// Add an OP_LOGNOT node with the expression as child
		// to logically negate the expression.
		Token = scan();
		n = cast(lv);
		n = rvalue(n, lv);
		n = mkunop(OP_LOGNOT, n);
		lv->prim = PINT;
		lv->addr = 0;
		return n;
	case AMPER:
		// Get the next token and scan the cast expression.
		Token = scan();
		n = cast(lv);
		// If we have a symbol and an address, add an OP_ADDR
		// node with the lvalue expression as child.
		if (lv->addr) {
			if (lv->sym) n = mkunop1(OP_ADDR, lv->sym, n);
		}
		// Otherwise it could be an array element. If not, error.
		else if ((0 == lv->sym || Syms[lv->sym].type != TARRAY) &&
			 !comptype(lv->prim)
		) {
			error("lvalue expected after unary '&'", NULL);
		}
		// Change the expression's type to a pointer to it
		// with no address
		lv->prim = pointerto(lv->prim);
		lv->addr = 0;
		return n;
	case SIZEOF:
		// Scan and verify the next token is a '('
		Token = scan();
		lparen();
		// Get the computed size of the expression
		n = comp_size();
		// Scan and verify the next token is a ')'
		rparen();
		// We got an OP_LIT node, so it's type is PINT
		// with no address
		lv->prim = PINT;
		lv->addr = 0;
		return n;
	default:
		// We didn't recognise the first token, so
		// pass it to postfix()
		return postfix(lv);
	}
}

/*
 * cast :=
 *	  prefix
 *	| ( type ) prefix
 *	| ( type * ) prefix
 *	| ( type * * ) prefix
 *	| ( INT ( * ) ( ) ) prefix
 */

// Parse a cast expression and return 
// a sub-tree representing it. Also return
// the lvalue details in lv.
static node *cast(struct lvalue *lv) {
	int	t;
	node	*n;

	// We got a leading '(' token, get the next token
	if (LPAREN == Token) {
		Token = scan();

		// Check we have a type keyword following the '('
		if (	INT == Token || CHAR == Token || VOID == Token ||
			STRUCT == Token || UNION == Token
		) {
			// Get the primitive type of this token
			// and move up to the next token
			t = primtype(Token, NULL);
			Token = scan();
		}
		else {
			// Hmm, we didn't recognise that keyword
			// so push the token back into the queue
			// and go back to the '(' token. Parse
			// what follows with prefix().
			reject();
			Token = LPAREN;
			strcpy(Text, "(");
			return prefix(lv);
		}

		// At this point we have scanned the '(' '<type>' tokens.
		// If we scan '(' 'INT' '(' ...
		if (PINT == t && LPAREN == Token) {
			// Move up to the next token. Confirm it's a '*'.
			// Scan '(' ')' '(' after that, and set this
			// is a function pointer
			Token = scan();
			match(STAR, "int(*)()");
			rparen();
			lparen();
			rparen();
			t = FUNPTR;
		}
		// The only thing possibly left is one or two '*'s.
		// Scan them, and use pointerto() once or twice
		// to chnage the type as required
		else if (STAR == Token) {
			t = pointerto(t);
			Token = scan();
			if (STAR == Token) {
				t = pointerto(t);
				Token = scan();
			}
		}

		// Match the trailing ')', then parse the following
		// expression with prefix(). Set the type of this
		// expression to what we found here, t
		rparen();
		n = prefix(lv);
		lv->prim = t;
		return n;
	}
	// No leading '(', so pass it to prefix()
	else {
		return prefix(lv);
	}
}

// Convert operator tokens into AST operators
int binop(int tok) {
	switch(tok) {
	case AMPER:	return OP_BINAND;
	case CARET:	return OP_BINXOR;
	case EQUAL:	return OP_EQUAL;
	case GREATER:	return OP_GREATER;
	case GTEQ:	return OP_GTEQ;
	case LESS:	return OP_LESS;
	case LSHIFT:	return OP_LSHIFT;
	case LTEQ:	return OP_LTEQ;
	case MINUS:	return OP_SUB;
	case MOD:	return OP_MOD;
	case NOTEQ:	return OP_NOTEQ;
	case PIPE:	return OP_BINIOR;
	case PLUS:	return OP_PLUS;
	case RSHIFT:	return OP_RSHIFT;
	case SLASH:	return OP_DIV;
	case STAR:	return OP_MUL;
	default:	fatal("internal: unknown binop");
			return 0; /* notreached */
	}
}

// Given a binary operator token in op, a left and right child
// sub-tree in l and r, and two AST node arguments in p1 and p2,
// use the token to determine what binary AST node to make for
// these children. Return the new tree.
node *mkop(int op, int p1, int p2, node *l, node *r) {
	if (PLUS == op || MINUS == op) {
		return mkbinop2(binop(op), p1, p2, l, r);
	}
	else if (EQUAL == op || NOTEQ == op || LESS == op ||
		 GREATER == op || LTEQ == op || GTEQ == op)
	{
		return mkbinop1(binop(op), p1, l, r);
	}
	else {
		return mkbinop(binop(op), l, r);
	}
}

/*
 * term :=
 *	  cast
 *	| term * cast
 *	| term / cast
 *	| term % cast
 *
 * sum :=
 *	  term
 *	| sum + term
 *	| sum - term
 *
 * shift :=
 *	  sum
 *	| shift << sum
 *	| shift >> sum
 *
 * relation :=
 *	  shift
 *	| relation < shift
 *	| relation > shift
 *	| relation <= shift
 *	| relation >= shift
 *
 * equation :=
 *	  relation
 *	| equation == relation
 *	| equation != relation
 *
 * binand :=
 *	  equation
 *	| binand & equation
 *
 * binxor :=
 *	  binand
 *	| binxor ^ binand
 *
 * binor :=
 *	  binxor
 *	| binor '|' binxor
 *
 * binexpr :=
 *	  binor
 */

// Parse a binary expression and return 
// a sub-tree representing it. Also return
// the lvalue details in lv.
static node *binexpr(struct lvalue *lv) {
	// The binary operators have an order of predecence.
	// So we have a list of 9 operators and 10 types & trees.
	// We build the final AST tree to enforce the precedence order.
	int	ops[9];
	int	prims[10];
	int	sp = 0;
	int	a;
	struct lvalue lv2;
	node	*tree[10];

	// Parse the first expression with cast()
	// and get its type and if its a proper lvalue
	tree[0] = cast(lv);
	a = lv->addr;
	prims[0] = lv->prim;

	// Based on the token that follows this first expression
	while (SLASH == Token || STAR == Token || MOD == Token ||
		PLUS == Token || MINUS == Token || LSHIFT == Token ||
		RSHIFT == Token || GREATER == Token || GTEQ == Token ||
		LESS == Token || LTEQ == Token || EQUAL == Token ||
		NOTEQ == Token || AMPER == Token || CARET == Token ||
		PIPE == Token
	) {
		// Convert the previous expression into an rvalue
		tree[0] = rvalue(tree[0], lv);

		// While this token has same or lower precedence than
		// the last token
		while (sp > 0 && Prec[Token] <= Prec[ops[sp-1]]) {

			// XXX: I need to grok this further!
			tree[sp-1] = mkop(ops[sp-1], prims[sp-1], prims[sp],
					tree[sp-1], tree[sp]);
			prims[sp-1] = binoptype(ops[sp-1], prims[sp-1],
					prims[sp]);
			sp--;
		}

		// Save the operation for this node, scan the next token
		// Parse the next expression with cast()
		// and get its type and if its a proper lvalue
		ops[sp++] = Token;
		Token = scan();
		tree[sp] = cast(&lv2);
		tree[sp] = rvalue(tree[sp], &lv2);
		prims[sp] = lv2.prim;
		a = 0;
	}

	// XXX: I need to grok this further!
	while (sp > 0) {
		tree[sp-1] = mkop(ops[sp-1], prims[sp-1], prims[sp],
				tree[sp-1], tree[sp]);
		prims[sp-1] = binoptype(ops[sp-1], prims[sp-1], prims[sp]);
		sp--;
	}
	// Finally get the primitive type of the top of the new tree
	// and if there's an address, and return the top of the new tree
	lv->prim = prims[0];
	lv->addr = a;
	return tree[0];
}

/*
 * logand :=
 *	  binexpr
 *	| logand && binexpr
 *
 * logor :=
 *	  logand
 *	| logor '||' logand
 */

// Parse a logical expression and return 
// a sub-tree representing it. Also return
// the lvalue details in lv.
static node *cond2(struct lvalue *lv, int op) {
	struct lvalue lv2;
	int	lab = 0;
	node	*n, *n2 = NULL;
	int	tv = 1;

	// Make LOGAND higher precedence than LOGOR
	// by recursively parsing LOGAND operators first.
	// Parse the first expression into the n sub-tree.
	n = op == LOGOR? cond2(lv, LOGAND): binexpr(lv);

	// While we have a LOGAND or LOGOR token
	while (Token == op) {

		// Get a label
		if (!lab) lab = label();
		if (tv) notvoid(lv->prim), tv = 0;	// XXX: why

		// Convert the previous expression into an rvalue
		// and get the next token
		n = rvalue(n, lv);
		Token = scan();

		// Parse the expression after the operator and
		// convert the previous expression into an rvalue
		n2 = op == LOGOR? cond2(&lv2, LOGAND): binexpr(&lv2);
		n2 = rvalue(n2, &lv2);

		// If an LOGOR, make a tree with the two children
		// and a "branch if true to label" operation
		if (op == LOGOR)
			n = mkbinop1(OP_BRTRUE, lab, n, n2);

		// Otherwise, a "branch if false to label" operation
		else
			n = mkbinop1(OP_BRFALSE, lab, n, n2);
	}

	// If we have a label, make a tree which is:
	// OP_LAB -> OP_BOOL -> OP_BRTRUE -> child expressions
	// The final true/false value is an INT rvalue
	if (lab) {
		n = mkunop1(OP_LAB, lab, n);
		n = mkunop(OP_BOOL, n);
		lv->prim = PINT;
		lv->addr = 0;
	}
	return n;
}

/*
 * condexpr :=
 *	  logor
 *	| logor ? expr : condexpr
 */

// Parse a ternary expression and return 
// a sub-tree representing it. Also return
// the lvalue details in lv.
static node *cond3(struct lvalue *lv) {
	node	*n, *n2;
	int	p;
	struct lvalue lv2;
	int	l1 = 0, l2 = 0, tv = 1;

	// Parse the first expression using cond2()
	n = cond2(lv, LOGOR);
	p = 0;

	// Each time we find a following '?' token
	while (QMARK == Token) {

		// Convert the previous expression into an rvalue
		n = rvalue(n, lv);

		if (tv) notvoid(lv->prim), tv = 0;	// XXX: why?

		// XXX: more to add here
		l1 = label();
		if (!l2) l2 = label();
		Token = scan();
		n2 = exprlist(&lv2, 0);
		n2 = rvalue(n2, &lv2);
		n = mkbinop1(OP_BRFALSE, l1, n, n2);
		if (!p) p = lv2.prim;
		if (!typematch(p, lv2.prim))
			error("incompatible types in '?:'", NULL);
		colon();
		n2 = cond2(&lv2, LOGOR);
		n2 = rvalue(n2, &lv2);
		n = mkbinop(OP_GLUE, n, n2);
		if (QMARK != Token)
			if (!typematch(p, lv2.prim))
				error("incompatible types in '?:'", NULL);
	}
	if (l2) {
		n = mkunop1(OP_IFELSE, l2, n);
		lv->prim = p;
		lv->addr = 0;
	}
	return n;
}

// Convert an arithmetic token
// into an AST operation.
int arithop(int tok) {
	switch(tok) {
	case ASPLUS:	return PLUS;
	case ASMINUS:	return MINUS;
	case ASAND:	return AMPER;
	case ASOR:	return PIPE;
	case ASXOR:	return CARET;
	case ASMUL:	return STAR;
	case ASMOD:	return MOD;
	case ASDIV:	return SLASH;
	case ASLSHIFT:	return LSHIFT;
	case ASRSHIFT:	return RSHIFT;
	default:	fatal("internal: unknown assignment operator");
			return 0; /* notreached */
	}
}

/*
 * asgmnt :=
 *	  condexpr
 *	| condexpr = asgmnt
 *	| condexpr *= asgmnt
 *	| condexpr /= asgmnt
 *	| condexpr %= asgmnt
 *	| condexpr += asgmnt
 *	| condexpr -= asgmnt
 *	| condexpr <<= asgmnt
 *	| condexpr >>= asgmnt
 *	| condexpr &= asgmnt
 *	| condexpr ^= asgmnt
 *	| condexpr |= asgmnt
 */

// Parse the possible assignment expressions
// and return a sub-tree representing it.
// Also return the lvalue details in lv.
static node *asgmnt(struct lvalue *lv) {
	node	*n, *n2, *src;
	struct lvalue lv2, lvs;
	int	op;

	// Parse the conditional expression
	n = cond3(lv);

	// If it's followed by one of these tokens
	if (ASSIGN == Token || ASDIV == Token || ASMUL == Token ||
		ASMOD == Token || ASPLUS == Token || ASMINUS == Token ||
		ASLSHIFT == Token || ASRSHIFT == Token || ASAND == Token ||
		ASXOR == Token || ASOR == Token
	) {
		// Keep the token and scan the next token
		op = Token;
		Token = scan();

		// Parse the following expression
		// and convert it into a proper rvalue
		n2 = asgmnt(&lv2);
		n2 = rvalue(n2, &lv2);

		// If this is a straight assignment (no operation),
		// check that the lvalue/rvalue types match,
		// and generate an OP_ASSIGN node: sym = rvalue
		if (ASSIGN == op) {
			if (!typematch(lv->prim, lv2.prim))
				error("assignment from incompatible type",
					NULL);
			n = mkbinop2(OP_ASSIGN, lv->prim, lv->sym, n, n2);
		}
		else {
			// Copy the node from the left-hand side
			// and convert it into an rvalue to get its value
			memcpy(&lvs, lv, sizeof(lvs));
			src = rvalue(n, &lvs);

			// Build an node to perform the operation
			// on the copied rvalue and the second operand,
			// which generates a new rvalue n2
			n2 = mkop(arithop(op), lv->prim, lv2.prim,
				src, n2);
			// Generate an OP_ASSIGN node: sym = n2
			n = mkbinop2(OP_ASSIGN, lv->prim, lv->sym, n, n2);
		}
		// If the lvalue wasn't actually an lvalue, error
		if (!lv->addr)
			error("lvalue expected in assignment", Text);
		// The node we return holds the result of the assignment,
		// so it's no longer an lvalue
		lv->addr = 0;
	}
	return n;
}

/*
 * expr :=
 *	  asgmnt
 *	| asgmnt , expr
 */

// Parse which is possibly a list of expressions.
// Parse which is possibly a list of expressions
// and return a sub-tree representing the list.
// Also return the lvalue details in lv. Check
// it is not void if ckvoid is true.
static node *exprlist(struct lvalue *lv, int ckvoid) {
	node	*n, *n2 = NULL;
	int	p;

	// Parse the first expression with asgmnt()
	// and get its primitive type.
	n = asgmnt(lv);
	p = lv->prim;

	// If there's comma, convert to an rvalue()
	if (COMMA == Token) n = rvalue(n, lv);

	// Repeat while we have more comma-separated
	// expressions. Use OP_COMMA nodes to join
	// all the sub-trees together
	while (COMMA == Token) {
		Token = scan();
		n2 = asgmnt(lv);
		n2 = rvalue(n2, lv);
		p = lv->prim;
		n = mkbinop(OP_COMMA, n, n2);
	}

	// Check the type of the last expression is not void
	// and return the new AST tree
	if (ckvoid) notvoid(p);
	return n;
}

// Parse and emit an expression by
// calling exprlist(), then rvalue()
// to make it an rvalue, then emittree().
// If ckvoid, check that it's not a void
// expression. If markreturn, adjust the
// tree to have OP_RETURN at the root.
void expr(struct lvalue *lv, int ckvoid, int markreturn) {
	node	*n;

	Ndtop = 1;
	n = exprlist(lv, ckvoid);
	n = rvalue(n, lv);
	if (markreturn)
		n = mkunop(OP_RETURN, n);
	emittree(n);
}

// Parse and emit an expression,
// checking that it's not a void
// expression.
void rexpr(void) {
	struct lvalue lv;

	expr(&lv, 1, 0);
}

// Parse what should be a constant expression.
// Use fold_reduce() on the AST tree to
// calculate the final value and return it.
int constexpr(void) {
	node	*n;
	struct lvalue lv;

	Ndtop = 1;
	n = binexpr(&lv);
	notvoid(lv.prim);
	n = fold_reduce(n);
	if (NULL == n || OP_LIT != n->op) {
		error("constant expression expected", NULL);
		return 0;
	}
	return n->args[0];
}
