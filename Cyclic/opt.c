/*
 *	NMH's Simple C Compiler, 2014
 *	Optimizer
 */

#include <limits.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

static int	Opt_sum_lim;

// Calculate the maximum value for an addition.
// Be careful not to exceed the compiler's internal
// limits, in case we are acting as a cross-compiler
// for a target with a bigger word size. Store the
// final value in Opt_sum_lim.
void opt_init(void) {
	int	i, k;

	Opt_sum_lim = 1;
	k = sizeof(int) < BPW? sizeof(int): BPW;
	k = k*8 - 1;
	for (i=1; i<k; i++)
		Opt_sum_lim <<= 1;
}

// Fold an AST tree with a binary operator
// and two INTLIT children. Return either 
// the original tree or a new leaf node.
static node *fold2(node *n) {
	int	v, vl, vr;

	// Get the values from each child
	vl = n->left->args[0];
	vr = n->right->args[0];
	switch (n->op) {

	// For addition and subtraction, ensure we don't
	// overflow our internal int variables. If we do,
	// return the original tree
	case OP_PLUS:
	case OP_ADD:	if (abs(vl) >= Opt_sum_lim || abs(vr) >= Opt_sum_lim)
				return n;
			v = vl + vr;
			break;
	case OP_SUB:	if (abs(vl) >= Opt_sum_lim || abs(vr) >= Opt_sum_lim)
				return n;
			v = vl - vr;
			break;

	// Sanity check. Do the multiplication followed by division.
	// Check we get the original result back. If not, return
	// the original tree
	case OP_MUL:	v = vl * vr;
			if (v / vl != vr) return n;
			if ((v & (Opt_sum_lim-1)) != v) return n;
			break;

	// Don't permit a divide by zero. Let the
	// resulting executable deal with it!
	case OP_DIV:	if (0 == vr) return n;
			v = vl / vr;
			break;
	case OP_MOD:	if (0 == vr) return n;
			v = vl % vr;
			break;
	case OP_BINAND:	v = vl & vr; break;
	case OP_BINIOR:	v = vl | vr; break;
	case OP_BINXOR:	v = vl ^ vr; break;

	// Don't permit shifts that are too big
	case OP_LSHIFT:	if (vr > BPW*8-1) return n;
			v = vl << vr;
			break;
	case OP_RSHIFT:	if (vr > BPW*8-1) return n;
			v = vl >> vr;
			break;

	// Calculate a true/false (1/0) value
	// for comparisons
	case OP_EQUAL:	v = vl == vr; break;
	case OP_NOTEQ:	v = vl != vr; break;
	case OP_LESS:	v = vl <  vr; break;
	case OP_GREATER:v = vl >  vr; break;
	case OP_LTEQ:	v = vl <= vr; break;
	case OP_GTEQ:	v = vl >= vr; break;
	default:	return n;
			break;
	}

	// Make an OP_LIT leaf node with new value
	return mkleaf(OP_LIT, v);
}

// Fold an AST tree with a unary operator
// and one INTLIT children. Return either 
// the original tree or a new leaf node.
static node *fold1(node *n) {
	int	v;

	// Get the child value. Do the
	// operation if recognised.
	// Return the new leaf node.
	v = n->left->args[0];
	switch (n->op) {
	case OP_NOT:	v = ~v; break;
	case OP_LOGNOT:	v = !v; break;
	case OP_NEG:	v = -v; break;
	case OP_SCALE:	v = v * BPW; break;
	default:	return n;
			break;	// XXX: why?
	}
	return mkleaf(OP_LIT, v);
}

// Attempt to reduce an AST tree
// with the root node n
static node *reduce(node *n) {
	int	k, i, lim, op, cl, cr, vl = 0, vr = 0;

	// Get the operation and any literal child values
	op = n->op;
	cl = OP_LIT == n->left->op;
	cr = OP_LIT == n->right->op;
	if (cl) vl = n->left->args[0];
	if (cr) vr = n->right->args[0];

	// Optimise away operations involving zero
	if ((OP_PLUS == op || OP_ADD == op) && cr && 0 == vr)	/* x+0 -> x */
		return n->left;
	if ((OP_PLUS == op || OP_ADD == op) && cl && 0 == vl)	/* 0+x -> x */
		return n->right;
	if (OP_SUB == op && cr && 0 == vr)			/* x-0 -> x */
		return n->left;
	if (OP_SUB == op && cl && 0 == vl)			/* 0-x -> -x */
		return mkunop(OP_NEG, n->right);
	if (OP_MUL == op && cl && 0 == vl)			/* 0*x -> 0 */
		return mkleaf(OP_LIT, 0);
	if (OP_MUL == op && cr && 0 == vr)			/* x*0 -> 0 */
		return mkleaf(OP_LIT, 0);

	// Look for multiply/divide operations
	// where one value is a power of two
	if (OP_MUL == op || OP_DIV == op) {
		lim = BPW * 8 - 1;

		// By looping across each power of two
		// from the least to most significant
		// bit in the target word size
		for (k=1,i=0; i<lim; i++, k<<=1) {

			// vr is a power of two
			if (cr && k == vr) {
				// So replace the operation with a bit shift
				if (OP_MUL == op)
					return mkbinop(OP_LSHIFT, n->left,
							mkleaf(OP_LIT, i));
				else
					return mkbinop(OP_RSHIFT, n->left,
							mkleaf(OP_LIT, i));
			}
			// vl is a power of two
			else if (cl && k == vl && OP_MUL == op) {
				// So replace the operation with a bit shift
				return mkbinop(OP_LSHIFT, n->right,
						mkleaf(OP_LIT, i));
			}
		}
	}
	return n;
}

// Attempt to fold and reduce an
// AST tree with the root node n
node *fold_reduce(node *n) {
	if (NULL == n) return NULL;

	// Fold & reduce the left child
	// then do the same on the right child
	n->left = fold_reduce(n->left);
	n->right = fold_reduce(n->right);

	// Both children are OP_LITs, do a fold2()
	if (n->left && OP_LIT == n->left->op &&
	    n->right && OP_LIT == n->right->op)
	{
		n = fold2(n);
	}

	// The left child is an OP_LIT, do a fold1()
	else if (n->left && OP_LIT == n->left->op)
		n = fold1(n);

	// Both children are OP_LITs, do a reduce()
	if (n->left && n->right &&
	    (OP_LIT == n->left->op || OP_LIT == n->right->op))
	{
		n = reduce(n);
	}
	return n;
}

// Recursively swap the left and right
// child trees around. Nils' book says
// the conditions are:
// the root node represents a commutative operation;
// it has a leaf node on the left side;
// it has a non-leaf node on the right side.
//
// The purpose is to minimise the pressure on
// register allocation and to reduce the amount
// of assembly code generated. Read Nils' book
// for well-written details.
node *reorder_ops(node *n) {
	int	op, lop, t;
	node	*tn;

	if (NULL == n) return NULL;

	// Reorder any operations on the left,
	// then on the right
	n->left = reorder_ops(n->left);
	n->right = reorder_ops(n->right);

	// If nothing got done, return the original tree
	if (NULL == n->right || NULL == n->left) return n;


	op = n->op;
	lop = n->left->op;

	// Conditions to meet:
	// 1. Either the left child is a literal, or
	//    an rvalue which is an identifier
	// 2. The operation is arithmetic or bitwise.
	if (	(OP_LIT == lop ||
		 (OP_RVAL == lop &&
		  n->left->left &&
		  OP_IDENT == n->left->left->op))
		&&
		(OP_ADD == op || OP_PLUS == op || OP_MUL == op ||
		 OP_BINAND == op || OP_BINIOR == op || OP_BINXOR == op ||
		 OP_EQUAL == op || OP_NOTEQ == op)
	) {
		// Switch the type of each sub-tree around
		if (OP_PLUS == op) {
			t = n->args[0];
			n->args[0] = n->args[1];
			n->args[1] = t;
		}

		// Swap the child nodes around
		tn = n->right;
		n->right = n->left;
		n->left = tn;
	}
	return n;
}

// Optimise an AST tree by
// folding and reducing all
// sub-trees, then reordering
// any operations where possible
node *optimize(node *n) {
	n = fold_reduce(n);
	n = reorder_ops(n);
	return n;
}
