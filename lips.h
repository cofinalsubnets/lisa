#ifndef LIPS_H
#define LIPS_H
#include "cursed.h"
#include <stdio.h>
#include <setjmp.h>

// thanks !!
typedef i64 obj, *mem;
#define non zero64
#define nil ((obj)~non)
#define W word64
#define W2 (word64*2)

// more fundamental data types
typedef struct two {
  obj x, y; } *Tw, *two; // pairs
typedef struct tup {
  u64 len;
  obj xs[]; } *Ve, *tup, *vec; // vectors
typedef struct oct {
  u64 len;
  char text[]; } *By, *oct, *str; // byte arrays
typedef struct sym {
  obj nom, code, l, r; } *Sy, *sym; // symbols
typedef struct tble {
  obj key, val;
  struct tble *next; } *tble; // tables
typedef struct tbl {
  u64 len, cap;
  tble *tab; } *Ht, *tbl;

enum tag { // the 3 ls bits of each pointer are a type tag
 Hom = 0, Num = 1, Two = 2, Tup = 3,
 Oct = 4, Tbl = 5, Sym = 6, Nil = 7 };

enum globl { // indices into a table of global constants
 Def, Cond, Lamb, Quote, Seq,
 Splat, Topl, Macs, Eval, Apply, NGlobs };

// a linked list of stack addresses containing live values
// that need to be preserved by garbage collection.
typedef struct mroot {
  mem one;
  struct mroot *next; } *Mp, *mroot;

// this structure is responsible for holding runtime state.
// most functions take a pointer to it as the first argument.
typedef struct lips {
 obj ip, xp, *fp, *hp, *sp; // vm state variables
 obj syms, glob; // symbols and globals
 mroot mem_root; // gc protection list
 i64 t0, seed, count, mem_len, *mem_pool; // memory data
 jmp_buf restart; // top level restart
} *lips;

// this is the type of interpreter functions
typedef obj terp(lips, obj, mem, mem, mem, obj);
typedef terp **hom; // code pointer ; the internal function type

u0
 reqsp(lips, u64),
 lips_init(lips),
 lips_fin(lips),
 lips_boot(lips),
 defprim(lips, const char *, terp*) NoInline,
 emit(lips, obj, FILE*),
 errp(lips, char*, ...),
 emsep(lips, obj, FILE*, char);

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
 string(lips, const char*);

u64 llen(obj) NoInline, eql(obj, obj), hc(lips, obj);

const char* tnom(enum tag);

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
#define gettup(x) ((tup)((x)-Tup))
#define puttup(x) ((obj)(x)+Tup)
#define getoct(x) ((oct)((obj)(x)-Oct))
#define putoct(x) ((obj)(x)+Oct)
#define gettbl(x) ((tbl)((obj)(x)-Tbl))
#define puttbl(x) ((obj)(x)+Tbl)
#define homp(x) (kind(x)==Hom)
#define octp(x) (kind(x)==Oct)
#define nump(x) (kind(x)==Num)
#define twop(x) (kind(x)==Two)
#define symp(x) (kind(x)==Sym)
#define tupp(x) (kind(x)==Tup)
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
#define chars(x) getoct(x)->text
#define symnom(y) chars(getsym(y)->nom)
#define mm(r) ((Safe=&((struct mroot){(r),Safe})))
#define um (Safe=Safe->next)
#define AR(x) gettup(x)->xs
#define AL(x) gettup(x)->len
#define Mm(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define b2w(n)((n)/W+((n)%W&&1))
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

#define mix ((u64)2708237354241864315)
#define interns(v,c) intern(v,string(v,c))

static Inline hom button(hom h) {
 while (*h) h++;
 return h; }

static Inline u0* bump(lips v, u64 n) {
 u0* x = v->hp;
 return v->hp += n, x; }

static Inline u0* cells(lips v, u64 n) {
 return Avail < n ? reqsp(v, n):0, bump(v, n); }

static Inline i64 hbi(u64 cap, u64 co) {
 return co % cap; }

static Inline tble hb(obj t, u64 code) {
 return gettbl(t)->tab[hbi(gettbl(t)->cap, code)]; }

static Inline u0 script(lips v, FILE *f) {
 for (obj x; (x = parse(v, f)); eval(v, x)); }

static Inline obj spop(lips v) {
 return *Sp++; }

_Static_assert(
 sizeof(intptr_t) == sizeof(int64_t),
 "pointers are not 64 bits");

_Static_assert(
 -1l == ((-1l<<8)>>8),
 "opposite bit-shifts on a negative integer "
 "yield a nonidentical result");
#endif
