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
typedef i64 obj, *mem;

// the 3 least bits of each pointer are a type tag
typedef enum { Hom = 0, Num = 1, Two = 2, Vec = 3,
               Str = 4, Tbl = 5, Sym = 6, Nil = 7 } class;

// current function stack frame
typedef struct frame {
  obj clos, retp, subd, argc, argv[]; } *frame;

// list of addresses of live values preserved by garbage
// collection
typedef struct root { mem one; struct root *next; } *root;

// indices to a global (thread-local) table of constants
enum {
 Def, Cond, Lamb, Quote, Seq, Splat,
 Topl, Macs, Eval, Apply, NGlobs };

// this structure holds runtime state.
// most runtime functions take a pointer to this as the
// first argument.
typedef struct lips {
  obj ip, xp, *fp, *hp, *sp, // interpreter state
      syms, glob[NGlobs], // symbols & globals
      rand, count, // random state
      t0, len, *pool; // memory state
  root root; // gc protection list
} *lips;

// this is the type of interpreter functions
typedef obj terp(lips, obj, mem, mem, mem, obj);
typedef terp **hom; // function pointer pointer

// a packed array of 4-byte strings.
extern const uint32_t *tnoms;

#define nil (~(obj)0)
#define Word (sizeof(obj))
#define word Word
#define kind(x) ((x)&7)
#define bind(v, x) if(!((v)=(x)))return 0

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
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

// linear congruential pseudorandom number generator
// the multiplier comes from "Computationally Easy, Spectrally
// Good Multipliers for Congruential Pseudorandom Number
// Generators" by Steele & Vigna
static Inline i64 lcprng(i64 s) {
  return (s * 0xaf251af3b0f025b5ll + 1) >> 8; }

static Inline u64 w2b(u64 w) { return w * 8; }
static Inline u64 b2w(u64 b) { return b / 8 + (b % 8 && 1); }

static Inline u1 nilp(obj x) { return x == nil; }
static Inline const char *tnom(class t) { return (const char*) (tnoms + t); }

lips li_ini(u0);
u0 li_fin(lips);

_Static_assert(sizeof(void*) == sizeof(int64_t), "64 bit pointers");
_Static_assert(-1 >> 1 == -1, "sign-extended bit shifts");
#endif
