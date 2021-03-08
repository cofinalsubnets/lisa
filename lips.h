#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <setjmp.h>
#include <string.h>
#include <time.h>

// thanks !!
typedef int64_t obj, num, *mem;
#define non ((obj)0)
#define nil (~non)
#define Word sizeof(non)

typedef struct root { mem one; struct root *next; } *root;
typedef struct rt {
  obj ip, xp, *fp, *hp, *sp; // vm state variables
  obj dict, syms, syn, cdict; // globals
  root mem_root; // memory
  num t0, count, mem_len, *mem_pool;
  // top level restart
  jmp_buf restart; } *rt, *vm;

// can't just be terp** :( bc C
typedef union hom *hom;
typedef obj terp(rt, hom, mem, mem, mem, obj);
union hom { terp *g; };
typedef struct two { obj x, y; } *two;
typedef struct tup { num len; obj xs[]; } *tup;
typedef struct oct { num len; char text[]; } *oct;
typedef struct sym { obj nom, code, next; } *sym;

typedef struct tble { obj key, val; struct tble *next; } *tble;
typedef struct tbl { num len, cap; tble *tab; } *tbl;

// the 3 ls bits of each pointer are a type tag
enum type {
  Hom = 0, Num = 1, Two = 2, Tup = 3,
  Oct = 4, Tbl = 5, Sym = 6, Nil = 7 };

enum syn {
  Def = 0, Cond = 1, Lamb = 2, Quote = 3, Seq = 4, Splat = 5,
  NSyns };

rt initialize();

void finalize(rt),
     emit(rt, obj, FILE*),
     vferrp(rt, FILE*, const char*, obj, const char*, va_list), // lol
     psyms(rt, obj),
     errp(rt, const char*, obj, const char*, ...),
     emsep(rt, obj, FILE*, char),
     reqsp(rt, num),
     *bump(rt, num),
     *cells(rt, num);

obj assq(rt, obj, obj),
    ldel(rt, obj, obj),
    err(rt, const char*, obj, const char*, ...),
    restart(rt),
    reset(rt),
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
    mdict(rt),
    string(rt, const char*);
num llen(obj);

const char *tnom(enum type);
extern const char *t_nom[];

#define kind(x) ((x)&7)
#define gethom(x) ((hom)((x)-Hom))
#define puthom(x) ((obj)(x)+Hom)
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
#define F(x) (gethom(x)+1)
#define G(x) gethom(x)->g
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
#define Dict v->dict
#define Syms v->syms
#define Syn v->syn
#define If AR(v->syn)[Cond]
#define De AR(v->syn)[Def]
#define La AR(v->syn)[Lamb]
#define Qt AR(v->syn)[Quote]
#define Se AR(v->syn)[Seq]
#define Va AR(v->syn)[Splat]
#define Avail (Sp-Hp)
#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
