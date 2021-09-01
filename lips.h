#ifndef LIPS_H
#define LIPS_H
#include "cursed.h"
#include <stdio.h>
#include <setjmp.h>
#include <stdlib.h>

_Static_assert(
 sizeof(intptr_t) == sizeof(int64_t),
 "pointers are not 64 bits");

_Static_assert(
 -1l == ((-1l<<8)>>8),
 "opposite bit-shifts on a negative integer yield a different result");

// thanks !!
typedef i64 obj, *mem;
#define non ((obj)0)
#define nil (~non)
#define W (sizeof(obj))
#define W2 (W*2)

typedef struct two { obj x, y; } *two; // pairs
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
typedef struct mroot { mem one; struct mroot *next; } *mroot;

// this structure is responsible for holding runtime state.
// most functions take a pointer to it as the first argument.
typedef struct lips {
 obj ip, xp, *fp, *hp, *sp; // vm state variables
 obj syms, glob; // symbols and globals
 mroot mem_root; // gc protection list
 i64 t0, seed, count, mem_len, *mem_pool; // memory data
 jmp_buf *restart; // top level restart
} *lips;

// this is the type of interpreter functions
typedef obj terp(lips, obj, mem, mem, mem, obj);
typedef terp **hom; // code pointer ; the internal function type

int lips_boot(lips);

u0
 reqsp(lips, u64),
 defprim(lips, const char *, terp*) NoInline,
 emit(lips, obj, FILE*),
 errp(lips, char*, ...),
 emsep(lips, obj, FILE*, char);

lips
 lips_init(lips),
 lips_open(void),
 lips_close(lips),
 lips_fin(lips);

obj
 sskc(lips, mem, obj),
 restart(lips),
 homnom(lips, obj),
 pair(lips, obj, obj),
 parse(lips, FILE*),
 intern(lips, obj),
 eval(lips, obj),
 compile(lips, obj),
 table(lips),
 spush(lips, obj) NoInline,
 tblset(lips, obj, obj, obj),
 tblget(lips, obj, obj),
 tbldel(lips, obj, obj),
 string(lips, const char*),
 lips_eval(lips, char *),
 script(lips, const char*, FILE*);

u64 llen(obj) NoInline, hc(lips, obj);
bool eql(obj, obj);

extern const uint32_t *tnoms;
#define tnom(t) ((char*)(tnoms+(t)))

#define kind(x) ((x)&7)
#define Gh(x) ((hom)((x)))
#define Ph(x) ((obj)(x))
#define Gn getnum
#define Pn putnum
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) ((obj)(x)+Two)
#define getnum(n) ((i64)(n)>>3)
#define putnum(n) (((obj)(n)<<3)+Num)
#define getsym(x) ((sym)((obj)(x)-Sym))
#define putsym(x) ((obj)(x)+Sym)
#define gettup(x) ((vec)((x)-Vec))
#define puttup(x) ((obj)(x)+Vec)
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
#define X(o) gettwo(o)->x
#define Y(o) gettwo(o)->y
#define XX(x) X(X(x))
#define XY(x) X(Y(x))
#define YX(x) Y(X(x))
#define YY(x) Y(Y(x))
#define F(x) ((hom)(x)+1)
#define G(x) (*(hom)(x))
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define chars(x) getstr(x)->text
#define symnom(y) chars(getsym(y)->nom)
#define mm(r) ((Safe=&((struct mroot){(r),Safe})))
#define um (Safe=Safe->next)
#define AR(x) gettup(x)->xs
#define AL(x) gettup(x)->len
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define w2b(n) ((n)*W)
#define Size(t) (sizeof(struct t)/W)
#define Ip v->ip
#define Fp v->fp
#define Hp v->hp
#define Sp v->sp
#define Safe v->mem_root
#define Xp v->xp
#define Pool v->mem_pool
#define Len v->mem_len
#define Dict Top
#define Syms (v->syms)
#define Glob v->glob
#define If AR(Glob)[Cond]
#define De AR(Glob)[Def]
#define La AR(Glob)[Lamb]
#define Qt AR(Glob)[Quote]
#define Se AR(Glob)[Seq]
#define Va AR(Glob)[Splat]
#define Top AR(Glob)[Topl]
#define Mac AR(Glob)[Macs]
#define Eva AR(Glob)[Eval]
#define App AR(Glob)[Apply]
#define Avail (Sp-Hp)
#define OK EXIT_SUCCESS
#define NO EXIT_FAILURE
#define xval(x) ((x)?NO:OK)

#define mix ((u64)2708237354241864315)
#define interns(v,c) intern(v,string(v,c))

static Inline hom button(hom h) {
 while (*h) h++;
 return h; }

static Inline u0* bump(lips v, u64 n) {
 u0* x = v->hp;
 return v->hp += n, x; }

static Inline u0* cells(lips v, u64 n) {
 if (Avail < n) reqsp(v, n);
 return bump(v, n); }

static Inline i64 hbi(u64 cap, u64 co) {
 return co % cap; }

static Inline ent hb(obj t, u64 code) {
 return gettbl(t)->tab[hbi(gettbl(t)->cap, code)]; }

static Inline obj spop(lips v) {
 return *Sp++; }

static Inline u64 b2w(u64 b) {
 return b / W + (b % W && 1); }
#endif
