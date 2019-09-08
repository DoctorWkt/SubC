/*
 *	NMH's Simple C Compiler, 2011--2014
 *	Global variables
 */

#ifndef extern_
 #define extern_ extern
#endif

extern_ FILE	*Infile;
extern_ FILE	*Outfile;
extern_ int	Token;
extern_ char	Text[TEXTLEN+1];
extern_ int	Value;
extern_ int	Line;
extern_ int	Errors;
extern_ int	Syntoken;
extern_ int	Putback;
extern_ int	Rejected;
extern_ int	Rejval;
extern_ char	Rejtext[TEXTLEN+1];
extern_ char	*File;
extern_ char	*Basefile;
extern_ char	*Macp[MAXNMAC];
extern_ int	Macc[MAXNMAC];
extern_ int	Mp;
extern_ int	Expandmac;
extern_ int	Ifdefstk[MAXIFDEF], Isp;
extern_ int	Inclev;
extern_ int	Textseg;
extern_ int	Nodes[NODEPOOLSZ];
extern_ int	Ndtop;
extern_ int	Ndmax;

/* symbol table structure */
extern_ char	*Names[NSYMBOLS];	// Name of a symbol
extern_ int	Prims[NSYMBOLS];	// Primitive type of a symbol
extern_ char	Types[NSYMBOLS];	// Meta type of a symbol
extern_ char	Stcls[NSYMBOLS];	// Storage class of a symbol
extern_ int	Sizes[NSYMBOLS];	// Number of elements in the symbol
extern_ int	Vals[NSYMBOLS];		// Initial value of the symbol
extern_ char	*Mtext[NSYMBOLS];
extern_ int	Globs;			// Pointer to next empty global slot
extern_ int	Locs;			// Pointer to next empty local slot

extern_ int	Thisfn;

/* name list */
extern_ char	Nlist[POOLSIZE];
extern_ int	Nbot;
extern_ int	Ntop;

/* label stacks */
extern_ int	Breakstk[MAXBREAK], Bsp;
extern_ int	Contstk[MAXBREAK], Csp;
extern_ int	Retlab;

/* local init structure */
extern_ int	LIaddr[MAXLOCINIT];
extern_ int	LIval[MAXLOCINIT];
extern_ int	Nli;

/* synthesizer operand queue */
extern_ int	Q_type;
extern_ int	Q_val;
extern_ char	Q_name[NAMELEN+1];
extern_ int	Q_cmp;
extern_ int	Q_bool;

/* file collector */
extern_ char	*Files[MAXFILES];
extern_ char	Temp[MAXFILES];
extern_ int	Nf;

/* options */
extern_ int	O_verbose;
extern_ int	O_componly;
extern_ int	O_asmonly;
extern_ int	O_testonly;
extern_ int	O_stdio;
extern_ char	*O_outfile;
extern_ int	O_debug;
extern_ int	O_dumptree;
extern_ int	O_showtokens;
