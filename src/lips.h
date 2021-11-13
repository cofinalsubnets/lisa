#include "env.h"
#include <stdarg.h>
#include <stdio.h>
#include <setjmp.h>
#include <stdlib.h>

// thanks !!

typedef i64 num, obj, *mem;
typedef struct two { obj a, b; } *two; // pairs
typedef struct tup { u64 len; obj xs[]; } *tup, *vec; // vectors
typedef struct str { u64 len; char text[]; } *str; // byte arrays
typedef struct sym { obj nom, code, l, r; } *sym; // symbols
typedef struct ent { obj key, val; struct ent *next; } *ent; // tables
typedef struct tbl { u64 len, cap; ent *tab; } *tbl;

enum tag { // the 3 ls bits of each pointer are a type tag
 Hom = 0, Num = 1, Two = 2, Vec = 3,
 Str = 4, Tbl = 5, Sym = 6, Nil = 7 };

enum globl { // indices into a table of global constants
 Def, Cond, Lamb, Quote, Seq, Splat,
 Topl, Macs, Eval, Apply, NGlobs };

// a linked list of stack addresses containing live values
// that need to be preserved by garbage collection.
typedef struct root { mem one; struct root *next; } *root;

// this structure holds runtime state.
// most runtime functions take a pointer to this as the
// first argument.
typedef struct lips {
 obj ip, xp, *fp, *hp, *sp, // interpreter state
     syms, // symbol table
     glob[NGlobs]; // global variables
 i64 seed, count, // random state
     t0, len, *pool; // memory state
 root root; // gc protection list
 jmp_buf *restart; // top level restart
} *lips;

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
#define Gh(x) gethom(x)
#define H(x)  gethom(x)
#define H_(x) puthom(x)
#define _H(x) puthom(x)
#define Ph(x) puthom(x)
#define Gn getnum
#define Pn putnum
#define N(x) getnum(x)
#define N_(x) putnum(x)
#define _N(x) putnum(x)
#define kind(x) ((x)&7)
#define gethom(x) ((hom)((x)-Hom))
#define puthom(x) ((obj)((x)+Hom))
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) ((obj)(x)+Two)
#define getnum(n) ((i64)(n)>>3)
#define putnum(n) (((obj)(n)<<3)+Num)
#define getsym(x) ((sym)((obj)(x)-Sym))
#define putsym(x) ((obj)(x)+Sym)
#define getvec(x) ((vec)((x)-Vec))
#define putvec(x) ((obj)(x)+Vec)
#define S(x) getstr(x)
#define _S(x) putstr(x)
//#define Y(x) getsym(x)
//#define _Y(x) putsym(x)
#define V(x) getvec(x)
#define _V(x) putvec(x)
#define T(x) gettbl(x)
#define _T(x) puttbl(x)
#define getstr(x) ((str)((obj)(x)-Str))
#define putstr(x) ((obj)(x)+Str)
#define gettbl(x) ((tbl)((obj)(x)-Tbl))
#define puttbl(x) ((obj)(x)+Tbl)
#define homp(x) (kind(x)==Hom)
#define strp(x) (kind(x)==Str)
#define nump(x) (kind(x)==Num)
#define twop(x) (kind(x)==Two)
#define symp(x) (kind(x)==Sym)
#define vecp(x) (kind(x)==Vec)
#define tblp(x) (kind(x)==Tbl)
#define nilp(x) ((x)==nil)
#define mm(r) ((v->root=&((struct root){(r),v->root})))
#define um (v->root=v->root->next)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Size(t) Width(t)
#define Width(t) b2w(sizeof(struct t))
#define Ip v->ip
#define Fp v->fp
#define Hp v->hp
#define Sp v->sp
#define Xp v->xp
#define If v->glob[Cond]
#define De v->glob[Def]
#define La v->glob[Lamb]
#define Qt v->glob[Quote]
#define Se v->glob[Seq]
#define Va v->glob[Splat]
#define Top v->glob[Topl]
#define Mac v->glob[Macs]
#define Eva v->glob[Eval]
#define App v->glob[Apply]

static Inline obj run_lips(lips v) {
  return (*H(v->ip))(v, v->ip, v->fp, v->sp, v->hp, v->xp); }

_Static_assert( sizeof(i64*) == sizeof(i64), "64 bit pointers");
_Static_assert( -1 >> 1 == -1, "sign-extended bit shifts");
