#ifndef LIPS_H
#define LIPS_H
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
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

Ty intptr_t
 obj, num, *mem, O, Z, *M;
Ty uintptr_t N;
Ty void _, Vd;
Ty char Ch;
Ty FILE *Io;
#define Ob (O)
#define non (Ob 0)
#define nil (~non)
#define W Sz(obj) // pointer arithmetic unit
#define W2 (2*W)

// more fundamental data types
Ty Sr two { O x, y; } *Tw, *two; // pairs
Ty Sr tup { Z len; O xs[]; } *Ve, *tup; // vectors
Ty Sr oct { Z len; Ch text[]; } *By, *oct; // byte arrays
Ty Sr sym { O nom, code, l, r; } *Sy, *sym; // symbols

Ty Sr tble { O key, val; Sr tble *next; } *tble;
Ty Sr tbl { Z len, cap; tble *tab; } *Ht, *tbl;

Ty Sr root { mem one; Sr root *next; } *root;
// the 3 ls bits of each pointer are a type tag
En type {
 Hom = 0, Num = 1, Two = 2, Tup = 3,
 Oct = 4, Tbl = 5, Sym = 6, Nil = 7 };

En globl {
 Def, Cond, Lamb, Quote, Seq,
 Splat, Topl, Macs, Eval, Apply, NGlobs };

// this is the structure responsible for holding runtime
// state. a pointer to it as an argument to almost every
// function in lips.
Ty Sr V {
  O ip, xp, *fp, *hp, *sp; // vm state variables
  O syms, glob; // globals
  root mem_root; // memory
  Z t0, count, mem_len, *mem_pool;
  jmp_buf restart; } // top level restart
 *rt, *vm, *V;

// this is the type of interpreter functions
Ty O terp(V, O, M, M, M, O);
Ty terp **hom, **H; // code pointer ; the internal function type

V initialize(int, Ko Ch**),
  bootstrap(V),
  finalize(V);

_ scr(V, FILE*),
  emit(V, obj, FILE*),
  errp(V, Ko Ch*, ...),
  emsep(V, obj, FILE*, Ch),
  reqsp(V, num);

O err(V, obj, Ko Ch*, ...),
  restart(rt),
  homnom(V, obj),
  pair(V, obj, obj),
  parse(V, FILE*),
  intern(V, obj),
  eval(V, obj),
  table(rt),
  tblset(V, obj, obj, obj),
  tblget(V, obj, obj),
  tbldel(V, obj, obj),
  tblkeys(V, obj),
  string(V, Ko char*);
Z llen(obj);
int eql(obj, obj);

Ko Ch* tnom(En type);

#define kind(x) ((x)&7)
#define Gh(x) ((hom)((x)))
#define Ph(x) (Ob(x))
#define Gn getnum
#define Pn putnum
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) (Ob(x)+Two)
#define getnum(n) ((num)(n)>>3)
#define putnum(n) ((Ob(n)<<3)+Num)
#define getsym(x) ((sym)((obj)(x)-Sym))
#define putsym(x) (Ob(x)+Sym)
#define gettup(x) ((tup)((x)-Tup))
#define puttup(x) (Ob(x)+Tup)
#define getoct(x) ((oct)((obj)(x)-Oct))
#define putoct(x) (Ob(x)+Oct)
#define gettbl(x) ((tbl)((obj)(x)-Tbl))
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
#define F(x) ((hom)(x)+1)
#define G(x) (*(hom)(x))
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
#define with(...) Mm(__VA_ARGS__)
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

#define mix 2708237354241864315ul

St In hom button(hom h) {
 Wh (*h) h++;
 R h; }

St In _* bump(V v, Z n) { _* x;
 R x = v->hp, v->hp += n, x; }

St In _* cells(V v, Z n) {
 R Avail < n ? reqsp(v, n):0, bump(v, n); }

O compile(vm, obj);

#define NOM "lips"

_Static_assert(
  Sz(O) >= 8,
  "pointers are smaller than 64 bits");
  
_Static_assert(
  -9 == (((Ob-9)<<32)>>32),
  "opposite bit-shifts on a negative number "
  "yield a nonidentical result");
#endif
