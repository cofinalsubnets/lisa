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
// first of all, C makes you type too much
#define In inline __attribute__((always_inline))
#define Nin __attribute__((noinline))
#define Inline In
#define NoInline Nin
#define St static
#define Vd void
#define Ty typedef
#define Sr struct
#define Un union

// "obj" is the data supertype. it's synonymous with
// the pointer integer type on the host platform, but for
// clarity the two are usually distinguished.
Ty intptr_t obj, num, *mem;
#define non ((obj)0)
#define nil (~non)
#define Word sizeof(non)

// this is the structure responsible for holding runtime
// state. a pointer to it as an argument to almost every
// function in lips.
Ty Sr rt *rt, *vm;

// this is a pointless container around the function type
Ty Un hom *hom;
Ty obj terp(rt, hom, mem, mem, mem, obj);
// unfortunately the function type can't just be terp**
// because of mandatory type indirection. a case against
// strong static typing?
Un hom { terp *g; };

// now some more fundamental data typesj
Ty Sr two { obj x, y; } *two;
Ty Sr tup { num len; obj xs[]; } *tup;
Ty Sr oct { num len; char text[]; } *oct;
Ty Sr sym { obj nom, code, l, r; } *sym;

typedef struct spec {
  struct spec *sp;
  obj (*cp)(rt, obj), nom; } *spec;
typedef struct tble { obj key, val; struct tble *next; } *tble;
typedef struct tbl { num len, cap; tble *tab; } *tbl;

Ty Sr root { mem one; Sr root *next; } *root;
struct rt {
  obj ip, xp, *fp, *hp, *sp; // vm state variables
  obj syms, glob; // globals
  root mem_root; // memory
  num t0, count, mem_len, *mem_pool;
  // top level restart
  jmp_buf restart; };

// the 3 ls bits of each pointer are a type tag
enum type {
  Hom = 0, Num = 1, Two = 2, Tup = 3,
  Oct = 4, Tbl = 5, Sym = 6, Nil = 7 };

enum globl {
  Def, Cond, Lamb, Quote, Seq, Splat, Topl, Macs,
  Eval, Apply, NGlobs };

rt initialize(),
   bootstrap(rt),
   finalize(rt);

void scr(vm, FILE*),
     emit(rt, obj, FILE*),
     errp(rt, const char*, ...),
     emsep(rt, obj, FILE*, char),
     reqsp(rt, num),
     *bump(rt, num),
     *cells(rt, num);

obj err(rt, obj, const char*, ...),
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
    string(rt, const char*);
num llen(obj);
int eql(obj, obj);

const char *tnom(enum type);

#define kind(x) ((x)&7)
#define Gh(x) ((hom)((x)-Hom))
#define Ph(x) ((obj)(x)+Hom)
#define Gn getnum
#define Pn putnum
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) ((obj)(x)+Two)
#define getnum(n) ((num)(n)>>3)
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
#define F(x) (Gh(x)+1)
#define G(x) Gh(x)->g
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define chars(x) getoct(x)->text
#define symnom(y) chars(getsym(y)->nom)
#define mm(r) ((Safe=&((struct root){(r),Safe})))
#define um (Safe=Safe->next)
#define LEN(x) (sizeof((x))/sizeof(*(x)))
#define AR(x) gettup(x)->xs
#define AL(x) gettup(x)->len
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define b2w(n)((n)/Word+((n)%Word&&1))
#define w2b(n) ((n)*Word)
#define Size(t) (sizeof(struct t)/Word)
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

extern const uint64_t mix;

St In hom button(hom h) {
  while (h->g) h++;
  return h; }

hom compile(vm v, obj x);

#ifndef NOM
#define NOM "lips"
#endif

_Static_assert(
  sizeof(obj) >= 8,
  "pointers are less than 64 bits");
  
_Static_assert(
  -9 == (((num)-9<<32)>>32),
  "opposite bit-shifts on a negative number "
  "yield a nonidentical result");
#endif
