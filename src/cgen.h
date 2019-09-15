/*
 *	NMH's Simple C Compiler, 2011--2014
 *	Code generator interface
 */

void cgadd(void);		// Add secondary register to primary register
void cgalign(void);		// Align instructions in the text area
void cgand(void);		// Bitwise AND secondary register to primary register
void cgbool(void);		// Normalize truth value (i.e. to 0 or 1)
void cgbreq(int n);		// Branch to label n if registers equal
void cgbrfalse(int n);		// Branch to label n if primary register zero
void cgbrge(int n);		// Branch if primary register >= secondary register
void cgbrgt(int n);		// Branch if primary register > secondary register
void cgbrle(int n);		// Branch if primary register <= secondary register
void cgbrlt(int n);		// Branch if primary register < secondary register
void cgbrne(int n);		// Branch to label n if registers not equal
void cgbrtrue(int n);		// Branch to label n if primary register not zero
void cgbruge(int n);		// Branch if unsigned primary register >= secondary reg
void cgbrugt(int n);		// Branch if unsigned primary register > secondary reg
void cgbrule(int n);		// Branch if unsigned primary register <= secondary reg
void cgbrult(int n);		// Branch if unsigned primary register < secondary reg
void cgcall(char *s);		// Call function s
void cgcalr(void);		// Call function that the primary register points to
void cgcalswtch(void);		// Evaluate switch table
void cgcase(int v, int l);	// Case template for switch table
void cgclear(void);		// Clear the primary register
void cgclear2(void);		// Clear the secondary register
void cgdata(void);		// Switch output to the data section
void cgdec1ib(void);		// Pre-decrement indirect byte
void cgdec1iw(void);		// Pre-decrement indirect word
void cgdec1pi(int v);		// Pre-decrement pointer indirect
void cgdec2ib(void);		// Post-increment indirect byte
void cgdec2iw(void);		// Post-increment indirect word
void cgdec2pi(int v);		// Post-decrement pointer indirect
void cgdecgb(char *s);		// Decrement global byte
void cgdecgw(char *s);		// Decrement global word
void cgdeclb(int a);		// Decrement local byte
void cgdeclw(int a);		// Decrement local word
void cgdecpg(char *s, int v);	// Decrement pointer global
void cgdecpl(int a, int v);	// Decrement pointer local
void cgdecps(int a, int v);	// Decrement pointer local static
void cgdecsb(int a);		// Decrement local static byte
void cgdecsw(int a);		// Decrement local static word
void cgdefb(int v);		// Define byte value v in the data section
void cgdefc(int c);		// Define alphanumeric character in the data section
void cgdefl(int v);		// Define label v
void cgdefp(int v);		// Define pointer value v in the data section
void cgdefw(int v);		// Define word value v in the data section
void cgdiv(void);		// Divide primary register by secondary register
void cgentry(void);		// Set up a new function context
void cgeq(void);		// Equal to
void cgexit(void);		// Restore function caller's context and return
void cggbss(char *s, int z);	// Define a global BSS storage element
void cgge(void);		// Greater than or equal to
void cggt(void);		// Greater than
void cginc1ib(void);		// Pre-increment indirect byte
void cginc1iw(void);		// Pre-increment indirect word
void cginc1pi(int v);		// Pre-increment pointer indirect
void cginc2ib(void);		// Post-increment indirect byte
void cginc2iw(void);		// Post-increment indirect word
void cginc2pi(int v);		// Post-increment pointer indirect
void cgincgb(char *s);		// Increment global byte
void cgincgw(char *s);		// Increment global word
void cginclb(int a);		// Increment local byte
void cginclw(int a);		// Increment local word
void cgincpg(char *s, int v);	// Increment pointer global
void cgincpl(int a, int v);	// Increment pointer local
void cgincps(int a, int v);	// Increment pointer static
void cgincsb(int a);		// Increment local static byte
void cgincsw(int a);		// Increment local static word
void cgindb(void);		// Load indirect byte
void cgindw(void);		// Load indirect word
void cginitlw(int v, int a);	// Initialize local word
void cgior(void);		// Bitwise OR secondary register to primary register
void cgjump(int n);		// Jump to label n
void cglbss(char *s, int z);	// Define a local BSS storage element
void cgldga(char *s);		// Load global address
void cgldgb(char *s);		// Load global (public/static) byte
void cgldgw(char *s);		// Load global (public/static) word
void cgldinc(void);		// Load increment pointer to secondary register
void cgldla(int n);		// Load local address
void cgldlab(int id);		// Load label address
void cgldlb(int n);		// Load local byte
void cgldlw(int n);		// Load local word
void cgldsa(int n);		// Load local static address
void cgldsb(int n);		// Load local static byte
void cgldsw(int n);		// Load local static word
void cgldswtch(int n);		// Load switch table address to secondary register
void cgle(void); 		// Primary/secondary registers are less than or equal
void cglit(int v);		// Load literal (immediate) value into primary register
int  cgload2(void);		// Load value into secondary register
void cglognot(void);		// Logical NOT primary register
void cglt(void);		// Primary/secondary registers are less than
void cgmod(void);		// Modulo of primary register by secondary register
void cgmul(void);		// Multiply secondary register and primary register
void cgne(void);		// Primary/secondary registers are not equal
void cgneg(void);		// Negate the primary register
void cgnot(void);		// Bitwise invert the primary register
void cgpop2(void);		// Pop the secondary register off the stack
void cgpopptr(void);		// Pop indirect address into the secondary register
void cgpostlude(void);		// Generate the assembly postlude
void cgprelude(void);		// Generate the assembly prelude
void cgpublic(char *s);		// Announce s as a public name
void cgpush(void);		// Push primary register on the stack
void cgpushlit(int n);		// Push immediate value n on the stack
void cgscale(void);		// Scale the primary register by the machine word size
void cgscale2(void);		// Scale the primary register by the secondary register
void cgscale2by(int v);		// Scale the secondary register by the value v
void cgscaleby(int v);		// Scale the primary register by the value v
void cgshl(void);		// Arithmetic left-shift
void cgshr(void);		// Arithmetic right-shift
void cgstack(int n);		// Add n to the stack pointer
void cgstorgb(char *s);		// Store global byte
void cgstorgw(char *s);		// Store global word
void cgstorib(void);		// Store indirect byte
void cgstoriw(void);		// Store indirect word
void cgstorlb(int n);		// Store local byte
void cgstorlw(int n);		// Store local word
void cgstorsb(int n);		// Store local static byte
void cgstorsw(int n);		// Store local static word
void cgsub(void);		// Subtract secondary register from primary register
void cgswap(void);		// Swap primary and secondary registers
void cgxor(void);		// Bitwise XOR secondary register to primary register
void cgtext(void);		// Switch output to the text section
void cguge(void);		// Primary/secondary registers are >= unsigned
void cgugt(void);		// Primary/secondary registers are > unsigned
void cgule(void);		// Primary/secondary registers are <= unsigned
void cgult(void);		// Primary/secondary registers are < unsigned
void cgunscale(void);		// Unscale (divide by machine word size) primary register
void cgunscaleby(int v);	// Unscale (divide) primary register by value v
