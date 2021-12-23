#include "env.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <setjmp.h>

_Static_assert(sizeof(i64*) == sizeof(i64), "64 bit pointers");
_Static_assert(-1 >> 1 == -1, "sign-extended bit shifts");

// thanks !!

typedef i64 obj, *mem;

enum tag { // the 3 ls bits of each pointer are a type tag
 Hom = 0, Num = 1, Two = 2, Vec = 3,
 Str = 4, Tbl = 5, Sym = 6, Nil = 7 };

// a linked list of stack addresses containing live values
// that need to be preserved by garbage collection.
typedef struct frame *frame;
typedef struct root  *root;
typedef struct lips  *lips;

enum globl { // indices into a table of global constants
 Def, Cond, Lamb, Quote, Seq, Splat,
 Topl, Macs, Eval, Apply, Restart, NGlobs };

// the frame structure holds the current function context.
struct lips {
 obj ip, xp, *fp, *hp, *sp, // interpreter state
     syms, // symbol table
     glob[NGlobs]; // global variables
 i64 seed, count, // random state
     t0, len, *pool; // memory state
 root root; // gc protection list
 jmp_buf restart; // top level restart
};

// this structure holds runtime state.
// most runtime functions take a pointer to this as the
// first argument.

// this is the type of interpreter functions
typedef obj terp(lips, obj, mem, mem, mem, obj);
typedef terp **hom; // function pointer pointer

// a packed array of 4-byte strings.
extern const uint32_t *tnoms;

#define nil (~(obj)0)
#define W (sizeof(obj))
#define W2 (W*2)
#define TNom(t) ((char*)(tnoms+(t)))
#define tnom TNom
#define kind(x) ((x)&7)
#define N(x) getnum(x)
#define _N(x) putnum(x)
#define getnum(n) ((i64)(n)>>3)
#define putnum(n) (((obj)(n)<<3)+Num)
#define nump(x) (kind(x)==Num)
#define nilp(x) ((x)==nil)
#define mm(r) ((v->root=&((struct root){(r),v->root})))
#define um (v->root=v->root->next)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Width(t) b2w(sizeof(struct t))
