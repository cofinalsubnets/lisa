#ifndef _lips_h
#define _lips_h
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// thanks !!

typedef void u0;
typedef bool u1;

#define BWDQ(_) _(8) _(16) _(32) _(64)
#define I(n) typedef int##n##_t i##n; typedef uint##n##_t u##n;
BWDQ(I)
#undef I
typedef i8 i1;
typedef i64 ob;


// the 3 least bits of each pointer are a type tag
enum class { Hom = 0, Num = 1, Two = 2, Vec = 3,
             Str = 4, Tbl = 5, Sym = 6, Nil = 7 };

// current function stack frame
typedef struct fr { ob clos, retp, subd, argc, argv[]; } *fr;

// list of addresses of live values preserved by garbage
// collection
typedef struct mm { ob*it; struct mm*et; } *mm;

// indices to a global (thread-local) table of constants
enum { Def, Cond, Lamb, Quote, Seq, Splat,
       Topl, Macs, Eval, Apply, NGlobs };

typedef struct yo *yo; // よ
typedef struct mo *mo, *en;
// this structure holds runtime state.
// most runtime functions take a pointer to this as the
// first argument.
struct mo {
  yo ip;
  fr fp;
  ob *hp, *sp, xp; // interpreter state
  ob syms, glob[NGlobs], // symbols & globals
     rand, // random state
     t0, len, *pool; // memory state
  mm mm; // gc protection list
};

// this is the type of interpreter functions
// FIXME en yo fr ob* ob* ob
typedef ob ll(mo, ob, ob*, ob*, ob*, ob);
typedef ll vm;
struct yo { ll *ll, *sh[]; }; // puLLback / puSHout
typedef struct str { u64 len; char text[]; } *str;
typedef struct sym { ob nom, code, l, r; } *sym;
typedef struct ent { ob key, val; struct ent *next; } *ent; // tables
typedef struct tbl { u64 len, cap; ent *tab; } *tbl;
typedef struct vec { u64 len; ob xs[]; } *vec;
typedef struct two { ob a, b; } *two;

// a packed array of 4-byte strings.
extern const uint32_t *tnoms;
mo ini(void);
u64 hash(mo, ob);
ob eval(mo, ob), homnom(mo, ob),
   analyze(mo, ob), sequence(mo, ob, ob),
   string(mo, const char*), intern(mo, ob),
   interns(mo, const char*), sskc(mo, ob*, ob),
   tblkeys(mo, ob), table(mo), tbl_set(mo, ob, ob, ob),
   tbl_set_s(mo, ob, ob, ob), tbl_get(mo, ob, ob),
   pair(mo, ob, ob),
   parse(mo, FILE*),
   read_path(mo, const char*), read_file(mo, FILE*);
void fin(mo), *cells(mo, u64);
bool eql(ob, ob), please(mo, u64);

#define nil (~(ob)0)
#define bind(v, x) if(!((v)=(x)))return 0

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define SI static Inline
#define SNI static NoInline
#define memn(n)\
  static Inline u0 set##n(u0*_d,u##n i,u64 l) {\
    for(u##n*d=_d;l--;*d++=i); }\
  static Inline u0 cpy##n(u0*_d,const u0*_s, u64 l) {\
    u##n*d=_d; const u##n*s=_s; while (l--) *d++=*s++; }\
  static Inline u0 cpy##n##r(u0*_d,const u0*_s, u64 l) {\
    u##n*d=_d; const u##n*s=_s; while (l--) d[l]=s[l]; }\
  static Inline u0 mov##n(u0*_d,const u0*_s, u64 l) {\
    if (_d<_s) cpy##n(_d, _s, l);\
    else if (_d>_s) cpy##n##r(_d, _s, l); }
BWDQ(memn)
#undef memn
#undef BWDQ
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define A(o) gettwo(o)->a
#define B(o) gettwo(o)->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define Avail (v->sp-v->hp)
#define mm(r) ((v->mm=&((struct mm){(r),v->mm})))
#define um (v->mm=v->mm->et)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Width(t) b2w(sizeof(struct t))

#define Q(_) ((_)&(sizeof(ob)-1))
static Inline i64 lcprng(i64 s) {
  const i64 steele_vigna_2021 = 0xaf251af3b0f025b5ll;
  return (s * steele_vigna_2021 + 1) >> 8; }
static Inline u64 b2w(u64 b) {
  return b / sizeof(ob) + (b % sizeof(ob) && 1); }

#define nilp(_) ((_)==nil)
#define tnom(_) ((const char*)(tnoms+(_)))

#define nump(_) (Q(_)==Num)
#define strp(_) (Q(_)==Str)
#define getnum(_) ((_)>>3)
#define putnum(_) (((_)<<3)+Num)
#define getstr(_) ((str)((_)-Str))
#define putstr(_) ((ob)(_)+Str)
#define F(_) ((yo)(_)+1)
#define G(_) (((yo)(_))->ll)
#define homp(_) (Q(_)==Hom)
#define puthom(_) ((ob)(_))
#define gethom(_) ((yo)(_))
//sym.h
static Inline sym getsym(ob x) { return (sym) (x - Sym); }
static Inline ob putsym(u0 *y) { return (ob) y + Sym; }
static Inline u1 symp(ob x) { return Q(x) == Sym; }
static Inline tbl gettbl(ob x) { return (tbl) (x - Tbl); }
static Inline ob puttbl(tbl t) { return (ob) t + Tbl; }
static Inline vec getvec(ob x) { return (vec) (x - Vec); }
static Inline ob putvec(vec v) { return (ob) v + Vec; }

static Inline two gettwo(ob x) { return (two) (x - Two); }
static Inline ob puttwo(u0 *x) { return (ob) x + Two; }
static Inline u1 twop(ob x) { return Q(x) == Two; }
static Inline u64 llen(ob l) { for (u64 i = 0;; l = B(l), i++)
                      if (!twop(l)) return i; }


extern void (*writers[])(mo, ob, FILE*);
static Inline void emit(mo v, ob x, FILE *o) { writers[Q(x)](v, x, o); }
static Inline void emsep(en v, ob x, FILE *o, char s) { emit(v, x, o), fputc(s, o); }
_Static_assert(sizeof(void*) == sizeof(int64_t), "64 bit pointers");
_Static_assert(-1 >> 1 == -1, "sign-extended bit shifts");
#endif
