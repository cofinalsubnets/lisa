#ifndef LIPS_H
#define LIPS_H
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdbool.h>
#include <setjmp.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

// thanks !!
//
// first of all C makes you type waaaaaay too much
#define In inline __attribute__((always_inline))
#define Nin __attribute__((noinline))
#define Inline In
#define NoInline Nin
#define St static
#define Ty typedef
#define Sr struct
#define Un union
#define Ko const
#define En enum
#define Sz sizeof
#define R  return
#define El else
#define Sw switch
#define Bk break
#define Cu continue
#define Ks case
#define Df default
#define Wh while
#define Fo for

typedef intptr_t O, obj, i64, Z, *M, *mem;
typedef uintptr_t N, u64;
typedef void _, u0;
typedef char Ch;
typedef FILE *Io;
#define Ob (O)
#define non (Ob 0)
#define nil (Ob -1)
#define W sizeof(obj) // pointer arithmetic unit
#define W2 (2*W)

// more fundamental data types
typedef struct two {
  obj x, y; } *Tw, *two; // pairs
typedef struct tup {
  i64 len;
  obj xs[]; } *Ve, *tup, *vec; // vectors
typedef struct oct {
  i64 len;
  char text[]; } *By, *oct, *str; // byte arrays
typedef struct sym {
  obj nom, code, l, r; } *Sy, *sym; // symbols
typedef struct tble {
  obj key, val;
  struct tble *next; } *tble; // tables
typedef struct tbl {
  i64 len, cap;
  tble *tab; } *Ht, *tbl;

enum tag { // the 3 ls bits of each pointer are a type tag
 Hom = 0, Num = 1, Two = 2, Tup = 3,
 Oct = 4, Tbl = 5, Sym = 6, Nil = 7 };

enum globl { // indices into a table of global constants
 Def, Cond, Lamb, Quote, Seq,
 Splat, Topl, Macs, Eval, Apply, NGlobs };

// a linked list of stack addresses containing live values
// that need to be preserved by garbage collection.
typedef struct root { mem one; struct root *next; } *Mp, *root;

// this structure is responsible for holding runtime state.
// most functions take a pointer to it as the first argument.
typedef struct lips {
 O ip, xp, *fp, *hp, *sp; // vm state variables
 O syms, glob; // symbols and globals
 Mp mem_root; // gc protection list
 Z t0, count, mem_len, *mem_pool; // memory data
 jmp_buf restart; // top level restart
} *V, *lips;

// this is the type of interpreter functions
typedef obj terp(lips, obj, mem, mem, mem, obj);
typedef terp *T, **H, **hom; // code pointer ; the internal function type

lips
 initialize(int, Ko Ch**),
 bootstrap(lips),
 finalize(lips);

u0
 bcpy(_*, Ko _*, N),
 wcpy(_*, Ko _*, N),
 fill(_*, O, N),
 emit(V, O, Io),
 script(V, Io),
 errp(V, Ko Ch*, ...),
 emsep(V, O, Io, Ch),
 reqsp(V, Z);

obj
 err(V, O, Ko Ch*, ...),
 linitp(V, O, M),
 snoc(V, O, O),
 sskc(V, M, O),
 restart(V),
 homnom(V, O),
 pair(V, O, O),
 parse(V, Io),
 intern(V, O),
 eval(V, O),
 table(V),
 tblset(V, O, O, O),
 tblget(V, O, O),
 tbldel(V, O, O),
 tblkeys(V, O),
 string(V, Ko Ch*);

i64 idx(obj, obj),
    llen(obj);
int eql(O, O);

const char* tnom(enum tag);

#define kind(x) ((x)&7)
#define Gh(x) ((H)((x)))
#define Ph(x) (Ob(x))
#define Gn getnum
#define Pn putnum
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) (Ob(x)+Two)
#define getnum(n) ((Z)(n)>>3)
#define putnum(n) ((Ob(n)<<3)+Num)
#define getsym(x) ((sym)((O)(x)-Sym))
#define putsym(x) (Ob(x)+Sym)
#define gettup(x) ((tup)((x)-Tup))
#define puttup(x) (Ob(x)+Tup)
#define getoct(x) ((oct)((O)(x)-Oct))
#define putoct(x) (Ob(x)+Oct)
#define gettbl(x) ((tbl)((O)(x)-Tbl))
#define puttbl(x) (Ob(x)+Tbl)
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
#define F(x) ((H)(x)+1)
#define G(x) (*(H)(x))
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define chars(x) getoct(x)->text
#define symnom(y) chars(getsym(y)->nom)
#define mm(r) ((Safe=&((struct root){(r),Safe})))
#define um (Safe=Safe->next)
#define LEN(x) (Sz((x))/Sz(*(x)))
#define AR(x) gettup(x)->xs
#define AL(x) gettup(x)->len
#define Mm(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define b2w(n)((n)/W+((n)%W&&1))
#define w2b(n) ((n)*W)
#define Szr(t) (Sz(Sr t)/W)
#define Size(t) Szr(t)
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

#define mix ((N)2708237354241864315)

St In H button(H h) {
 Wh (*h) h++;
 R h; }

St In _* bump(V v, Z n) { _* x;
 R x = v->hp, v->hp += n, x; }

St In _* cells(V v, Z n) {
 R Avail < n ? reqsp(v, n):0, bump(v, n); }

St In Z hbi(Z cap, N co) { R co % cap; }

St In tble hb(O t, N code) {
 R gettbl(t)->tab[hbi(gettbl(t)->cap, code)]; }

_Static_assert(
 Sz(O) >= 8,
 "pointers are smaller than 64 bits");

_Static_assert(
 -9 == (((-9)<<12)>>12),
 "opposite bit-shifts on a negative integer "
 "yield a nonidentical result");

#endif
