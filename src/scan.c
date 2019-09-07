/*
 *	NMH's Simple C Compiler, 2011,2016
 *	Lexical analysis (scanner)
 */

#include "defs.h"
#include "data.h"
#include "decl.h"

// Get the next character from the input file.
int next(void) {
	int	c;

	if (Putback) {				// Use the character put
		c = Putback;			// back if there is one
		Putback = 0;
		return c;
	}
	if (Mp) {				// If we are expanding a macro
		if ('\0' == *Macp[Mp-1]) {	// and we've reached the end,
			Macp[Mp-1] = NULL;	// set the expanded ptr to NULL
			return Macc[--Mp];	// pop the stack and return the
		}				// first real char after the macro
		else {
			return *Macp[Mp-1]++;	// Return the next char from the
		}				// expanded macro
	}
	c = fgetc(Infile);			// Read from input file
	if ('\n' == c) Line++;			// Increment line count
	return c;
}

// Put back an unwanted character
void putback(int c) {
	Putback = c;
}

// Read in a hexadecimal constant from the input
static int hexchar(void) {
	int	c, h, n = 0, f = 0;

	// Loop getting characters
	while (isxdigit(c = next())) {
		// Convert from char to int value
		h = chrpos("0123456789abcdef", tolower(c));
		// Add to running hex value
		n = n * 16 + h;
		f = 1;
	}
	// We hit a non-hex character, put it back
	putback(c);
	// Flag tells us we never saw any hex characters
	if (!f)
		error("missing digits after '\\x'", NULL);
	if (n > 255)
		error("value out of range after '\\x'", NULL);
	return n;
}

// Return the next character from a character
// or string literal
static int scanch(void) {
	int	i, c, c2;

	// Get the next input character and interpret
	// metacharacters that start with a backslash
	c = next();
	if ('\\' == c) {
		switch (c = next()) {
		case 'a': return '\a';
		case 'b': return '\b';
		case 'f': return '\f';
		case 'n': return '\n';
		case 'r': return '\r';
		case 't': return '\t';
		case 'v': return '\v';
		case '\\': return '\\';
		case '"': return '"' | 256;	// XXX why 256?
		case '\'': return '\'';

		// Deal with octal constants by reading in
		// characters until we hit a non-octal digit.
		// Build up the octal value in c2 and count
		// # digits in i. Permit only 3 octal digits.
		case '0': case '1': case '2':
		case '3': case '4': case '5':
		case '6': case '7':
			for (i = c2 = 0; isdigit(c) && c < '8'; c = next()) {
				if (++i > 3) break;
				c2 = c2 * 8 + (c - '0');
			}
			putback(c);	// Put back the first non-octal char
			return c2;
		case 'x':
			return hexchar();
		default:
			scnerror("unknown escape sequence: %s", c);
			return ' ';
		}
	}
	else {
		return c;		// Just an ordinary old character!
	}
}

// Scan and return an integer literal
// value from the input file. Store
// the value as a string in Text.
static int scanint(int c) {
	int	val, radix, k, i = 0;

	val = 0;
	radix = 10;				// Assume radix is 10
	if ('0' == c) {				// But if starts with 0
		Text[i++] = '0';
		if ((c = next()) == 'x') {	// then 'x', it is
			radix = 16;		// radix 16
			Text[i++] = c;
			c = next();
		}
		else {
			radix = 8;		// Otherwise radix 8
		}
	}

	// Convert each character into an int value
	while ((k = chrpos("0123456789abcdef", tolower(c))) >= 0) {
		// Append to text and verify it is lower than the radix
		Text[i++] = c;
		if (k >= radix)
			scnerror("invalid digit in integer literal: %s", c);
		// Calculate the running value of the literal
		val = val * radix + k;
		c = next();
	}
	// We hit a non-integer character, put it back.
	// NUL terminate the Text string and return the value.
	putback(c);
	Text[i] = 0;
	return val;
}

// Scan in a string literal from the input file,
// and store it in buf[]. Return the length of
// the string. Surround the string with double quotes.
static int scanstr(char *buf) {
	int	i, c;

	// Put in the first double quote
	buf[0] = '"';
	// Loop while we have enough buf space
	for (i=1; i<TEXTLEN-2; i++) {
		// Get the next char and append to buf
		// Return when we hit the ending double quote
		if ((c = scanch()) == '"') {
			buf[i++] = '"';
			buf[i] = 0;
			return Value = i;
		}
		buf[i] = c;
	}
	// Ran out of buf[] space
	fatal("string literal too long");
	return 0;
}

// Scan an identifier from the input file and
// store it in buf[].Return the identifier's length
static int scanident(int c, char *buf, int lim) {
	int	i = 0;

	// Allow digits, alpha and underscores
	while (isalpha(c) || isdigit(c) || '_' == c) {
		// Error if we hit the identifier length limit,
		// else append to buf[] and get next character
		if (lim-1 == i) {
			error("identifier too long", NULL);
			i++;
		}
		else if (i < lim-1) {	// XX Why the test here?
			buf[i++] = c;
		}
		c = next();
	}
	// We hit a non-valid character, put it back.
	// NUL-terminate the buf[] and return the length
	putback(c);
	buf[i] = 0;
	return i;
}

// Skip past input that we don't need to deal with:
// whitespace, newlines XXX more information.
// Return the first character we do need to deal with
// XXX Add more comments
int skip(void) {
	int	c, p, nl;

	c = next();
	nl = 0;
	for (;;) {
		if (EOF == c) {
			strcpy(Text, "<EOF>");
			return EOF;
		}
		while (' ' == c || '\t' == c || '\n' == c ||
			'\r' == c || '\f' == c
		) {
			if ('\n' == c) nl = 1;
			c = next();
		}
		// Deal with preprocessor directives
		// if we have a '#' after a newline
		if (nl && c == '#') {
			preproc();
			c = next();
			continue;
		}
		nl = 0;
		if (c != '/')
			break;
		c = next();
		if (c != '*' && c != '/') {
			putback(c);
			c = '/';
			break;
		}
		if (c == '/') {
			while ((c = next()) != EOF) {
				if (c == '\n') break;
			}
                }
                else {
			p = 0;
			while ((c = next()) != EOF) {
				if ('/' == c && '*' == p) {
					c = next();
					break;
				}
				p = c;
			}
		}
	}
	return c;
}

// Given a word from the input, return the matching
// keyword token number or 0 if it's not a keyword.
// Switch on the first letter so that we don't have
// to waste time strcmp()ing against all the keywords.
static int keyword(char *s) {
	switch (*s) {
	case '#':
		switch (s[1]) {
		case 'd':
			if (!strcmp(s, "#define")) return P_DEFINE;
			break;
		case 'e':
			if (!strcmp(s, "#else")) return P_ELSE;
			if (!strcmp(s, "#endif")) return P_ENDIF;
			if (!strcmp(s, "#error")) return P_ERROR;
			break;
		case 'i':
			if (!strcmp(s, "#ifdef")) return P_IFDEF;
			if (!strcmp(s, "#ifndef")) return P_IFNDEF;
			if (!strcmp(s, "#include")) return P_INCLUDE;
			break;
		case 'l':
			if (!strcmp(s, "#line")) return P_LINE;
			break;
		case 'p':
			if (!strcmp(s, "#pragma")) return P_PRAGMA;
			break;
		case 'u':
			if (!strcmp(s, "#undef")) return P_UNDEF;
			break;
		}
		break;
	case 'a':
		if (!strcmp(s, "auto")) return AUTO;
		break;
	case 'b':
		if (!strcmp(s, "break")) return BREAK;
		break;
	case 'c':
		if (!strcmp(s, "case")) return CASE;
		if (!strcmp(s, "char")) return CHAR;
		if (!strcmp(s, "continue")) return CONTINUE;
		break;
	case 'd':
		if (!strcmp(s, "default")) return DEFAULT;
		if (!strcmp(s, "do")) return DO;
		break;
	case 'e':
		if (!strcmp(s, "else")) return ELSE;
		if (!strcmp(s, "enum")) return ENUM;
		if (!strcmp(s, "extern")) return EXTERN;
		break;
	case 'f':
		if (!strcmp(s, "for")) return FOR;
		break;
	case 'i':
		if (!strcmp(s, "if")) return IF;
		if (!strcmp(s, "int")) return INT;
		break;
	case 'r':
		if (!strcmp(s, "register")) return REGISTER;
		if (!strcmp(s, "return")) return RETURN;
		break;
	case 's':
		if (!strcmp(s, "sizeof")) return SIZEOF;
		if (!strcmp(s, "static")) return STATIC;
		if (!strcmp(s, "struct")) return STRUCT;
		if (!strcmp(s, "switch")) return SWITCH;
		break;
	case 'u':
		if (!strcmp(s, "union")) return UNION;
		break;
	case 'v':
		if (!strcmp(s, "void")) return VOID;
		if (!strcmp(s, "volatile")) return VOLATILE;
		break;
	case 'w':
		if (!strcmp(s, "while")) return WHILE;
		break;
	}
	return 0;
}

// Given a potential macro identifier,
// use findmac() to see if it is a macro.
// If not found or not marked as a macro,
// return 0. Otherwise expand the macro
// with playmac() and return 1.
static int macro(char *name) {
	int	y;

	y = findmac(name);
	if (!y || Types[y] != TMACRO)
		return 0;
	playmac(Mtext[y]);
	return 1;
}

// Scan and pre-process the input file.
// Return the value of the next token found.
static int scanpp(void) {
	int	c, t;

	if (Rejected != -1) {			// We have a previously
		t = Rejected;			// rejected token. Copy
		Rejected = -1;			// the token #, the text
		strcpy(Text, Rejtext);		// and value, and turn off
		Value = Rejval;			// the Rejected flag.
		return t;			// Return this rejected token
	}

	// Loop, but return when a token is found
	for (;;) {
		Value = 0;
		c = skip();				// Skip unwanted input
		memset(Text, 0, 4);
		Text[0] = c;

		// Use the first character to find 1-character
		// tokens, and to recognised n-character tokens.
		// Put back characters if we didn't find an n-char
		// token. Fill Text[] with the token's text.
		switch (c) {
		case '!':
			if ((c = next()) == '=') {
				Text[1] = '=';
				return NOTEQ;
			}
			else {
				putback(c);
				return XMARK;
			}
		case '%':
			if ((c = next()) == '=') {
				Text[1] = '=';
				return ASMOD;
			}
			else {
				putback(c);
				return MOD;
			}
		case '&':
			if ((c = next()) == '&') {
				Text[1] = '&';
				return LOGAND;
			}
			else if ('=' == c) {
				Text[1] = '=';
				return ASAND;
			}
			else {
				putback(c);
				return AMPER;
			}
		case '(':
			return LPAREN;
		case ')':
			return RPAREN;
		case '*':
			if ((c = next()) == '=') {
				Text[1] = '=';
				return ASMUL;
			}
			else {
				putback(c);
				return STAR;
			}
		case '+':
			if ((c = next()) == '+') {
				Text[1] = '+';
				return INCR;
			}
			else if ('=' == c) {
				Text[1] = '=';
				return ASPLUS;
			}
			else {
				putback(c);
				return PLUS;
			}
		case ',':
			return COMMA;
		case '-':
			if ((c = next()) == '-') {
				Text[1] = '-';
				return DECR;
			}
			else if ('=' == c) {
				Text[1] = '=';
				return ASMINUS;
			}
			else if ('>' == c) {
				Text[1] = '>';
				return ARROW;
			}
			else {
				putback(c);
				return MINUS;
			}
		case '/':
			if ((c = next()) == '=') {
				Text[1] = '=';
				return ASDIV;
			}
			else {
				putback(c);
				return SLASH;
			}
		case ':':
			return COLON;
		case ';':
			return SEMI;
		case '<':
			if ((c = next()) == '<') {
				Text[1] = '<';
				if ((c = next()) == '=') {
					Text[2] = '=';
					return ASLSHIFT;
				}
				else {
					putback(c);
					return LSHIFT;
				}
			}
			else if ('=' == c) {
				Text[1] = '=';
				return LTEQ;
			}
			else {
				putback(c);
				return LESS;
			}
		case '=':
			if ((c = next()) == '=') {
				Text[1] = '=';
				return EQUAL;
			}
			else {
				putback(c);
				return ASSIGN;
			}
		case '>':
			if ((c = next()) == '>') {
				Text[1] = '>';
				if ((c = next()) == '=') {
					Text[1] = '=';
					return ASRSHIFT;
				}
				else {
					putback(c);
					return RSHIFT;
				}
			}
			else if ('=' == c) {
				Text[1] = '=';
				return GTEQ;
			}
			else {
				putback(c);
				return GREATER;
			}
		case '?':
			return QMARK;
		case '[':
			return LBRACK;
		case ']':
			return RBRACK;
		case '^':
			if ((c = next()) == '=') {
				Text[1] = '=';
				return ASXOR;
			}
			else {
				putback(c);
				return CARET;
			}
		case '{':
			return LBRACE;
		case '|':
			if ((c = next()) == '|') {
				Text[1] = '|';
				return LOGOR;
			}
			else if ('=' == c) {
				Text[1] = '=';
				return ASOR;
			}
			else {
				putback(c);
				return PIPE;
			}
		case '}':
			return RBRACE;
		case '~':
			return TILDE;
		case EOF:
			strcpy(Text, "<EOF>");
			return XEOF;
		case '\'':
			Text[1] = Value = scanch();
			if ((c = next()) != '\'')
				error(
				 "expected '\\'' at end of char literal",
					NULL);
			Text[2] = '\'';
			return INTLIT;
		case '"':
			Value = scanstr(Text);
			return STRLIT;
		case '#':
			Text[0] = '#';
			scanident(next(), &Text[1], TEXTLEN-1);
			if ((t = keyword(Text)) != 0)
				return t;
			error("unknown preprocessor command: %s", Text);
			return IDENT;
		case '.':
			if ((c = next()) == '.') {
				Text[1] = Text[2] = '.';
				Text[3] = 0;
				if ((c = next()) == '.')
					return ELLIPSIS;
				putback(c);
				error("incomplete '...'", NULL);
				return ELLIPSIS;
			}
			putback(c);
			return DOT;
		default:
			if (isdigit(c)) {
				Value = scanint(c);
				return INTLIT;
			}
			else if (isalpha(c) || '_' == c) {
				Value = scanident(c, Text, TEXTLEN);

				// XXX Comments here
				if (Expandmac && macro(Text))
					break;
				if ((t = keyword(Text)) != 0)
					return t;
				return IDENT;
			}
			else {
				scnerror("funny input character: %s", c);
				break;
			}
		}
	}
}

// XXX Comments needed below about frozen(),
// Syntoken, macro expansion and Reject
int scan(void) {
	int	t;

	do {
		t = scanpp();
				// Error if we hit the end of a file
				// and we were in an #include'd file
				// with a non-empty #ifdef stack
				// as we never saw the matching #endif
		if (!Inclev && Isp && XEOF == t)
			fatal("missing '#endif'");
	} while (frozen(1));
	if (t == Syntoken)
		Syntoken = 0;
	return t;
}

// Return the next token from the input
// file without expanding macros
int scanraw(void) {
	int	t, oisp;

	oisp = Isp;		// Save the old ifdef stack pointer
	Isp = 0;		// Set the ifdef stack pointer to the stack bottom
				// XXX: why do the above?
	Expandmac = 0;		// Don't try to expand macros
	t = scan();		// Get the next input token
	Expandmac = 1;		// Re-enable macro expansion
	Isp = oisp;		// Restore the old ifdef stack pointer
	return t;		// and return the token found
}

// We hit a token but can't use it.
// Mark it as rejected. See the cast()
// function in expr.c.
void reject(void) {
	Rejected = Token;
	Rejval = Value;
	strcpy(Rejtext, Text);
}
