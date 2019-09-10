/*
 *	NMH's Simple C Compiler, 2011--2016
 *	Definitions
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "cg.h"
#include "sys.h"

#define VERSION		"2016-12-12"

#ifndef SCCDIR
 #define SCCDIR		"."
#endif

#ifndef AOUTNAME
 #define AOUTNAME	"a.out"
#endif

#define SCCLIBC		"%s/lib/libscc.a"

#define PREFIX		'C'
#define LPREFIX		'L'

#define INTSIZE		BPW
#define PTRSIZE		INTSIZE
#define CHARSIZE	1

#define TEXTLEN		512
#define NAMELEN		16

#define MAXFILES	32

#define MAXIFDEF	16
#define MAXNMAC		32
#define MAXCASE		256
#define MAXBREAK	16
#define MAXLOCINIT	32
#define MAXFNARGS	32

/* assert(NSYMBOLS < PSTRUCT) */
#define NSYMBOLS	1024
#define POOLSIZE	16384
#define NODEPOOLSZ	4096	/* ints */

/* types */
enum {
	TVARIABLE = 1,		// Variable
	TARRAY,			// Array
	TFUNCTION,		// Function
	TCONSTANT,		// Literal constant
	TMACRO,			// Pre-processor macro
	TSTRUCT			// Struct or union
};

/* primitive types */
enum {
	PCHAR = 1,		// unsigned char
	PINT,			// signed int
	CHARPTR,		// char pointer
	INTPTR,			// int pointer
	CHARPP,			// char pointer pointer
	INTPP,			// int pointer pointer
	PVOID,			// void
	VOIDPTR,		// void pointer
	VOIDPP,			// void pointer pointer
	FUNPTR,			// function pointer
	PSTRUCT = 0x2000,	// struct
	PUNION  = 0x4000,	// union
	STCPTR  = 0x6000,	// struct pointer
	STCPP   = 0x8000,	// struct pointer pointer
	UNIPTR  = 0xA000,	// union pointer
	UNIPP   = 0xC000,	// union pointer pointer
	STCMASK = 0xE000	// mask for struct/union bits
};

/* storage classes */
enum {
	CPUBLIC = 1,		// publicly visible symbol
	CEXTERN,		// extern symbol
	CSTATIC,		// static symbols in global context
	CLSTATC,		// static symbols in local context
	CAUTO,			// non-static local identifiers
	CSPROTO,		// function prototype
	CMEMBER,		// field of a struct/union
	CSTCDEF			// unused
};

/* lvalue structure */
			// Note that struct lvalue can also represent rvalues
struct lvalue {
        int sym;        // Symbol table slot number for the l/rvalue, or 0 if no symbol
        int prim;       // Primary type for the expression
        int addr;       // If true, the l/rvalue is associated with an address
};

/* debug options */
enum {
	D_LSYM = 1,
	D_GSYM = 2,
	D_STAT = 4
};

/* addressing modes */
enum {
	empty,
	addr_auto,
	addr_static,
	addr_globl,
	addr_label,
	literal,
	auto_byte,
	auto_word,
	static_byte,
	static_word,
	globl_byte,
	globl_word
};

/* compare instructions */
enum {
	cnone,
	equal,
	not_equal,
	less,
	greater,
	less_equal,
	greater_equal,
	below,
	above,
	below_equal,
	above_equal
};

/* boolean instructions */
enum {
	bnone,
	lognot,
	normalize
};

/* AST node */
struct node_stc {
	int		op;
	struct node_stc	*left, *right;
	int		args[1];
};

#define node	struct node_stc

/* tokens */
// The token numbers here must match up
// with the array index numbers in prec.h
enum {
	SLASH, STAR, MOD, PLUS, MINUS, LSHIFT, RSHIFT,
	GREATER, GTEQ, LESS, LTEQ, EQUAL, NOTEQ, AMPER,
	CARET, PIPE, LOGAND, LOGOR,

	ARROW, ASAND, ASXOR, ASLSHIFT, ASMINUS, ASMOD, ASOR, ASPLUS,
	ASRSHIFT, ASDIV, ASMUL, ASSIGN, AUTO, BREAK, CASE, CHAR, COLON,
	COMMA, CONTINUE, DECR, DEFAULT, DO, DOT, ELLIPSIS, ELSE, ENUM,
	EXTERN, FOR, IDENT, IF, INCR, INT, INTLIT, LBRACE, LBRACK,
	LPAREN, NOT, QMARK, RBRACE, RBRACK, REGISTER, RETURN, RPAREN,
	SEMI, SIZEOF, STATIC, STRLIT, STRUCT, SWITCH, TILDE, UNION,
	VOID, VOLATILE, WHILE, XEOF, XMARK,

	P_DEFINE, P_ELSE, P_ELSENOT, P_ENDIF, P_ERROR, P_IFDEF,
	P_IFNDEF, P_INCLUDE, P_LINE, P_PRAGMA, P_UNDEF
};

/* AST operators */
enum {
	OP_GLUE, OP_ADD, OP_ADDR, OP_ASSIGN, OP_BINAND, OP_BINIOR,
	OP_BINXOR, OP_BOOL, OP_BRFALSE, OP_BRTRUE, OP_CALL, OP_CALR,
	OP_COMMA, OP_DEC, OP_DIV, OP_EQUAL, OP_GREATER, OP_GTEQ,
	OP_IDENT, OP_IFELSE, OP_LAB, OP_LDLAB, OP_LESS, OP_LIT,
	OP_LOGNOT, OP_LSHIFT, OP_LTEQ, OP_MOD, OP_MUL, OP_NEG,
	OP_NOT, OP_NOTEQ, OP_PLUS, OP_PREDEC, OP_PREINC, OP_POSTDEC,
	OP_POSTINC, OP_RSHIFT, OP_RVAL, OP_SCALE, OP_SCALEBY, OP_SUB
};

