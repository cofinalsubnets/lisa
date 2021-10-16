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

// this structure is responsible for holding runtime state.
// most functions take a pointer to it as the first argument.
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
typedef terp **hom; // code pointer ; the internal function type

u64 llen(obj) NoInline;

u0
 reqsp(lips, u64),
 emit(lips, obj, FILE*),
 emsep(lips, obj, FILE*, char);

obj
 homnom(lips, obj),
 pair(lips, obj, obj),
 parse(lips, FILE*),
 read_file(lips, const char*),
 write_file(lips, const char*, const char*),
 intern(lips, obj),
 eval(lips, obj),
 string(lips, const char*);

// a packed array of 4-byte strings.
extern const uint32_t *tnoms;

#define nil (~(obj)0)
#define W (sizeof(obj))
#define W2 (W*2)
#define TNom(t) ((char*)(tnoms+(t)))
#define tnom TNom
#define Gh(x) gethom(x)
#define H(x) gethom(x)
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
#define A(o) gettwo(o)->a
#define B(o) gettwo(o)->b
#define X(o) gettwo(o)->a
#define Y(o) gettwo(o)->b
#define F(x) ((hom)(x)+1)
#define G(x) (*(hom)(x))
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define chars(x) getstr(x)->text
#define symnom(y) chars(getsym(y)->nom)
#define mm(r) ((v->root=&((struct root){(r),v->root})))
#define um (v->root=v->root->next)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define w2b(n) ((n)*W)
#define Size(t) (sizeof(struct t)/W)
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
#define Avail (Sp-Hp)

#define mix ((u64)2708237354241864315)
#define interns(v,c) intern(v,string(v,c))
#define SI static Inline
SI hom button(hom h) { while (*h) h++; return h; }
SI u0* bump(lips v, u64 n) { u0* x = v->hp; return v->hp += n, x; }
SI u0* cells(lips v, u64 n) { if (Avail < n) reqsp(v, n); return bump(v, n); }
SI u64 b2w(u64 b) { return b / W + (b % W && 1); }
#undef SI

_Static_assert(
 sizeof(intptr_t) == sizeof(int64_t),
 "pointers are not 64 bits");

_Static_assert(
 -1l == ((-1l<<8)>>8),
 "opposite bit-shifts on a negative integer yield a different result");
