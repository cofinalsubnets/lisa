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
#define Vd void
#define __ void
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
#define Ks case

Ty intptr_t // pointer type on the host platform
  obj,  // data supertype
  num,  // integers, distinguished for clarity
  *mem; // data pointer
#define O (obj)
#define non (O 0)
#define nil (~non)
#define W Sz(obj) // pointer arithmetic unit
#define W2 (2*W)

// this is the structure responsible for holding runtime
// state. a pointer to it as an argument to almost every
// function in lips.
Ty Sr rt *rt, *vm;
// this is the type of interpreter functions
Ty obj terp(rt, obj, mem, mem, mem, obj);
Ty terp **hom; // code pointer ; the internal function type

// more fundamental data types
Ty Sr two { obj x, y; } *two; // pairs
Ty Sr tup { num len; obj xs[]; } *tup; // vectors
Ty Sr oct { num len; char text[]; } *oct; // byte arrays
Ty Sr sym { obj nom, code, l, r; } *sym; // symbols

Ty Sr tble { obj key, val; Sr tble *next; } *tble;
Ty Sr tbl { num len, cap; tble *tab; } *tbl;

Ty Sr spec {
  Sr spec *sp;
  obj (*cp)(rt, obj), nom; } *spec;

Ty Sr root { mem one; Sr root *next; } *root;
Sr rt {
  obj ip, xp, *fp, *hp, *sp; // vm state variables
  obj syms, glob; // globals
  root mem_root; // memory
  num t0, count, mem_len, *mem_pool;
  jmp_buf restart; }; // top level restart

// the 3 ls bits of each pointer are a type tag
En type {
  Hom = 0, Num = 1, Two = 2, Tup = 3,
  Oct = 4, Tbl = 5, Sym = 6, Nil = 7 };

En globl {
  Def, Cond, Lamb, Quote, Seq, Splat, Topl, Macs,
  Eval, Apply, NGlobs };

rt initialize(),
   bootstrap(rt),
   finalize(rt);

__ scr(vm, FILE*),
   emit(rt, obj, FILE*),
   errp(rt, Ko char*, ...),
   emsep(rt, obj, FILE*, char),
   reqsp(rt, num),
   *bump(rt, num),
   *cells(rt, num);

obj err(rt, obj, Ko char*, ...),
    restart(rt),
    homnom(rt, obj),
    pair(rt, obj, obj),
    parse(rt, FILE*),
    intern(rt, obj),
    eval(rt, obj),
    table(rt),
    tblset(rt, obj, obj, obj),
    tblget(rt, obj, obj),
    tbldel(rt, obj, obj),
    tblkeys(rt, obj),
    string(rt, Ko char*);
num llen(obj);
int eql(obj, obj);

Ko char *tnom(En type);

#define kind(x) ((x)&7)
#define Gh(x) ((hom)((x)))
#define Ph(x) (O(x))
#define Gn getnum
#define Pn putnum
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) (O(x)+Two)
#define getnum(n) ((num)(n)>>3)
#define putnum(n) ((O(n)<<3)+Num)
#define getsym(x) ((sym)((obj)(x)-Sym))
#define putsym(x) (O(x)+Sym)
#define gettup(x) ((tup)((x)-Tup))
#define puttup(x) (O(x)+Tup)
#define getoct(x) ((oct)((obj)(x)-Oct))
#define putoct(x) (O(x)+Oct)
#define gettbl(x) ((tbl)((obj)(x)-Tbl))
#define puttbl(x) (O(x)+Tbl)
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

extern Ko uint64_t mix;

St In hom button(hom h) {
  while (*h) h++;
  R h; }

obj compile(vm, obj);

#ifndef NOM
#define NOM "lips"
#endif

_Static_assert(
  Sz(obj) >= 8,
  "pointers are less than 64 bits");
  
_Static_assert(
  -9 == (((num)-9<<32)>>32),
  "opposite bit-shifts on a negative number "
  "yield a nonidentical result");
#endif
