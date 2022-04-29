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
typedef enum { Hom = 0, Num = 1, Two = 2, Vec = 3,
               Str = 4, Tbl = 5, Sym = 6, Nil = 7 } class;

// current function stack frame
typedef struct fr { ob clos, retp, subd, argc, argv[]; } *fr;

// list of addresses of live values preserved by garbage
// collection
typedef struct mm { ob*it; struct mm*et; } *mm;

// indices to a global (thread-local) table of constants
enum { Def, Cond, Lamb, Quote, Seq, Splat,
       Topl, Macs, Eval, Apply, NGlobs };

// this structure holds runtime state.
// most runtime functions take a pointer to this as the
// first argument.
typedef struct en {
  ob ip, xp, *fp, *hp, *sp; // interpreter state
  ob syms, glob[NGlobs], // symbols & globals
     rand, // random state
     t0, len, *pool; // memory state
  mm mm; // gc protection list
} *en;

// this is the type of interpreter functions
typedef struct yo *yo;
typedef ob vm(en, ob, ob*, ob*, ob*, ob);
struct yo { vm *ll, *sh[]; };
typedef struct str { u64 len; char text[]; } *str;
typedef struct sym { ob nom, code, l, r; } *sym;
typedef struct ent { ob key, val; struct ent *next; } *ent; // tables
typedef struct tbl { u64 len, cap; ent *tab; } *tbl;
typedef struct vec { u64 len; ob xs[]; } *vec;
typedef struct two { ob a, b; } *two;

// a packed array of 4-byte strings.
extern const uint32_t *tnoms;
en li_ini(u0);
u64 hash(en, ob);
ob eval(en, ob), homnom(en, ob),
   analyze(en, ob), sequence(en, ob, ob),
   string(en, const char*), intern(en, ob),
   interns(en, const char*), sskc(en, ob*, ob),
   tblkeys(en, ob), table(en), tbl_set(en, ob, ob, ob),
   tbl_set_s(en, ob, ob, ob), tbl_get(en, ob, ob),
   pair(en, ob, ob),
   parse(en, FILE*),
   read_path(en, const char*), read_file(en, FILE*);
u0 li_fin(en), *cells(en, u64), emit(en, ob, FILE*);
u1 eql(ob, ob), please(en, u64),
 write_file(en, const char*, const char*);

#define nil (~(ob)0)
#define bind(v, x) if(!((v)=(x)))return 0

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define SI static Inline
#define SNI static NoInline
#define memn(n)\
  SI u0 set##n(u0*_d,u##n i,u64 l) {\
    for(u##n*d=_d;l--;*d++=i); }\
  SI u0 cpy##n(u0*_d,const u0*_s, u64 l) {\
    u##n*d=_d; const u##n*s=_s; while (l--) *d++=*s++; }\
  SI u0 cpy##n##r(u0*_d,const u0*_s, u64 l) {\
    u##n*d=_d; const u##n*s=_s; while (l--) d[l]=s[l]; }\
  SI u0 mov##n(u0*_d,const u0*_s, u64 l) {\
    if (_d<_s) cpy##n(_d, _s, l);\
    else if (_d>_s) cpy##n##r(_d, _s, l); }
BWDQ(memn)
#undef memn
#undef BWDQ
#define S(x) getstr(x)
#define _S(x) putstr(x)
#define H(x)  gethom(x)
#define _H(x) puthom(x)
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define Y(x) getsym(x)
#define _Y(x) putsym(x)
#define mix ((u64)2708237354241864315)
#define T(x) gettbl(x)
#define _T(x) puttbl(x)
#define V(x) getvec(x)
#define _V(x) putvec(x)
#define N(x) getnum(x)
#define _N(x) putnum(x)
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

SI class Q(ob _) { return _ & (sizeof(void*) - 1); }
// linear congruential pseudorandom number generator
// the multiplier comes from "Computationally Easy, Spectrally
// Good Multipliers for Congruential Pseudorandom Number
// Generators" by Steele & Vigna
SI i64 lcprng(i64 s) { return (s * 0xaf251af3b0f025b5ll + 1) >> 8; }

SI u64 w2b(u64 w) { return w * 8; }
SI u64 b2w(u64 b) { return b / 8 + (b % 8 && 1); }

SI u1 nilp(ob x) { return x == nil; }
SI const char *tnom(class t) { return (const char*) (tnoms + t); }

//num.h
SI u1 nump(ob x) { return Q(x) == Num; }
SI i64 getnum(ob x) { return x >> 3; }
SI ob putnum(i64 n) { return (n << 3) + Num; }
//str.h
SI str getstr(ob x) { return (str) (x - Str); }
SI ob putstr(str s) { return (ob) s + Str; }
SI u1 strp(ob x) { return Q(x) == Str; }
//hom.h
SI yo F(yo h) { return (yo) h->sh; }
SI vm *G(yo h) { return h->ll; }
SI yo gethom(ob x) { return (yo) (x - Hom); }
SI ob puthom(yo h) { return (ob) h + Hom; }
SI yo button(yo h) { while (G(h)) h = F(h); return h; }
SI u1 homp(ob x) { return Q(x) == Hom; }
//sym.h
SI sym getsym(ob x) { return (sym) (x - Sym); }
SI ob putsym(u0 *y) { return (ob) y + Sym; }
SI u1 symp(ob x) { return Q(x) == Sym; }
SI tbl gettbl(ob x) { return (tbl) (x - Tbl); }
SI ob puttbl(tbl t) { return (ob) t + Tbl; }
SI u1 tblp(ob x) { return Q(x) == Tbl; }
SI vec getvec(ob x) { return (vec) (x - Vec); }
SI ob putvec(vec v) { return (ob) v + Vec; }
SI u1 vecp(ob x) { return Q(x) == Vec; }

SI two gettwo(ob x) { return (two) (x - Two); }
SI ob puttwo(u0 *x) { return (ob) x + Two; }
SI u1 twop(ob x) { return Q(x) == Two; }
SI u64 llen(ob l) {
  for (u64 i = 0;; l = B(l), i++)
    if (!twop(l)) return i; }


SI u0 emsep(en v, ob x, FILE *o, char s) { emit(v, x, o), fputc(s, o); }
_Static_assert(sizeof(void*) == sizeof(int64_t), "64 bit pointers");
_Static_assert(-1 >> 1 == -1, "sign-extended bit shifts");
#endif
