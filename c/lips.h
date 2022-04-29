#ifndef _lips_h
#define _lips_h
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// thanks !!

#define BWDQ(_) _(8) _(16) _(32) _(64)

#define I(n) typedef int##n##_t i##n; typedef uint##n##_t u##n;
BWDQ(I)
#undef I

typedef void u0;
typedef bool u1;
typedef i8 i1;
typedef i64 ob, obj;

// the 3 least bits of each pointer are a type tag
typedef enum { Hom = 0, Num = 1, Two = 2, Vec = 3,
               Str = 4, Tbl = 5, Sym = 6, Nil = 7 } class;

// current function stack frame
typedef struct fr { ob clos, retp, subd, argc, argv[]; } *fr;

// list of addresses of live values preserved by garbage
// collection
typedef struct root { ob* one; struct root *next; } *root;

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
  root root; // gc protection list
} *en;

// this is the type of interpreter functions
typedef struct yo *yo;
typedef ob vm(en, ob, ob*, ob*, ob*, ob);
struct yo { vm *ll, *sh[]; };

// a packed array of 4-byte strings.
extern const uint32_t *tnoms;

#define nil (~(ob)0)
#define Word (sizeof(ob))
#define word Word
#define kind(x) ((x)&7)
#define bind(v, x) if(!((v)=(x)))return 0

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define SI static Inline
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

// linear congruential pseudorandom number generator
// the multiplier comes from "Computationally Easy, Spectrally
// Good Multipliers for Congruential Pseudorandom Number
// Generators" by Steele & Vigna
SI i64 lcprng(i64 s) { return (s * 0xaf251af3b0f025b5ll + 1) >> 8; }

SI u64 w2b(u64 w) { return w * 8; }
SI u64 b2w(u64 b) { return b / 8 + (b % 8 && 1); }

SI u1 nilp(ob x) { return x == nil; }
SI const char *tnom(class t) { return (const char*) (tnoms + t); }

//cmp.h
u1 eql(ob, ob);

//num.h
SI u1 nump(ob x) { return kind(x) == Num; }
SI i64 getnum(ob x) { return x >> 3; }
SI ob putnum(i64 n) { return (n << 3) + Num; }
#define N(x) getnum(x)
#define _N(x) putnum(x)
//str.h
typedef struct str { u64 len; char text[]; } *str;
ob string(en, const char*);
SI str getstr(ob x) { return (str) (x - Str); }
SI ob putstr(str s) { return (ob) s + Str; }
SI u1 strp(ob x) { return kind(x) == Str; }
#define S(x) getstr(x)
#define _S(x) putstr(x)
//hom.h
SI yo F(yo h) { return (yo) h->sh; }
SI vm *G(yo h) { return h->ll; }
SI yo gethom(ob x) { return (yo) (x - Hom); }
SI ob puthom(yo h) { return (ob) h + Hom; }
SI yo button(yo h) { while (G(h)) h = F(h); return h; }
SI u1 homp(ob x) { return kind(x) == Hom; }
#define H(x)  gethom(x)
#define _H(x) puthom(x)
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
ob eval(en, ob), homnom(en, ob), analyze(en, ob), sequence(en, ob, ob);
//sym.h
typedef struct sym { ob nom, code, l, r; } *sym;
ob intern(en, ob), interns(en, const char*), sskc(en, ob*, ob);
#define Y(x) getsym(x)
#define _Y(x) putsym(x)
SI sym getsym(ob x) { return (sym) (x - Sym); }
SI ob putsym(u0 *y) { return (ob) y + Sym; }
SI u1 symp(ob x) { return kind(x) == Sym; }
// tbl.h
typedef struct ent { ob key, val; struct ent *next; } *ent; // tables
typedef struct tbl { u64 len, cap; ent *tab; } *tbl;
u64 hash(en, ob);
ob tblkeys(en, ob),
   table(en),
   tbl_set(en, ob, ob, ob),
   tbl_set_s(en, ob, ob, ob),
   tbl_get(en, ob, ob);
SI tbl gettbl(ob x) { return (tbl) (x - Tbl); }
SI ob puttbl(tbl t) { return (ob) t + Tbl; }
SI u1 tblp(ob x) { return kind(x) == Tbl; }
#define mix ((u64)2708237354241864315)
#define T(x) gettbl(x)
#define _T(x) puttbl(x)
// vec.h
typedef struct vec { u64 len; ob xs[]; } *vec;
#define V(x) getvec(x)
#define _V(x) putvec(x)
SI vec getvec(ob x) { return (vec) (x - Vec); }
SI ob putvec(vec v) { return (ob) v + Vec; }
SI u1 vecp(ob x) { return kind(x) == Vec; }

// two.h
typedef struct two { ob a, b; } *two;
ob pair(en, ob, ob);
#define A(o) gettwo(o)->a
#define B(o) gettwo(o)->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
//#define W(x) gettwo(x)
//#define _W(w) puttwo(w)
SI two gettwo(ob x) { return (two) (x - Two); }
SI ob puttwo(u0 *x) { return (ob) x + Two; }
SI u1 twop(ob x) { return kind(x) == Two; }
SI u64 llen(ob l) {
  for (u64 i = 0;; l = B(l), i++)
    if (!twop(l)) return i; }

en li_ini(u0);
u0 li_fin(en);

//mem.h
#define Avail (v->sp-v->hp)
#define mm(r) ((v->root=&((struct root){(r),v->root})))
#define um (v->root=v->root->next)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Width(t) b2w(sizeof(struct t))
u0 *cells(en, u64);
u1 please(en, u64);

//read.h
ob parse(en, FILE*),
   read_path(en, const char*),
   read_file(en, FILE*);
//write.h
u0 emit(en, ob, FILE*);
u1 write_file(en, const char*, const char*);
SI u0 emsep(en v, ob x, FILE *o, char s) { emit(v, x, o), fputc(s, o); }
_Static_assert(sizeof(void*) == sizeof(int64_t), "64 bit pointers");
_Static_assert(-1 >> 1 == -1, "sign-extended bit shifts");
#endif
