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
typedef int64_t obj, num, *mem;
#define non ((obj)0)
#define nil (~non)
#define Word sizeof(non)

typedef struct root { mem one; struct root *next; } *root;
// can't just be terp** :( bc C
typedef union hom *hom;
typedef struct rt *rt, *vm;
typedef obj terp(rt, hom, mem, mem, mem, obj);
union hom { terp *g; };
typedef struct two { obj x, y; } *two;
typedef struct tup { num len; obj xs[]; } *tup;
typedef struct oct { num len; char text[]; } *oct;
typedef struct sym { obj nom, code, l, r; } *sym;

typedef struct spec {
  struct spec *sp;
  obj (*cp)(rt, obj), nom; } *spec;
typedef struct tble { obj key, val; struct tble *next; } *tble;
typedef struct tbl { num len, cap; tble *tab; } *tbl;

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
     phomn(rt, obj, FILE*),
     vferrp(rt, FILE*, const char*, va_list), // lol
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
    interns(rt, const char*),
    eval(rt, obj),
    table(rt),
    tbl_set(rt, obj, obj, obj),
    tbl_get(rt, obj, obj),
    tbl_del(rt, obj, obj),
    tbl_keys(rt, obj),
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
#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

#ifndef NOM
#define NOM "lips"
#endif
extern const uint64_t mix;
