#ifndef _medi_h
#define _medi_h
#include <stdint.h>
#define BWDQ(_) _(8) _(16) _(32) _(64)
#define I(n) typedef int##n##_t i##n; typedef uint##n##_t u##n;
BWDQ(I)
#undef I

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define memn(n)\
  static Inline void set##n(void*_d,u##n i,u64 l) {\
    for(u##n*d=_d;l--;*d++=i); }\
  static Inline void cpy##n(void*_d,const void*_s, u64 l) {\
    u##n*d=_d; const u##n*s=_s; while (l--) *d++=*s++; }\
  static Inline void cpy##n##r(void*_d,const void*_s, u64 l) {\
    u##n*d=_d; const u##n*s=_s; while (l--) d[l]=s[l]; }\
  static Inline void mov##n(void*_d,const void*_s, u64 l) {\
    if (_d<_s) cpy##n(_d, _s, l);\
    else if (_d>_s) cpy##n##r(_d, _s, l); }
BWDQ(memn)
#undef memn
#undef BWDQ

static Inline ob lcprng(ob s) {
  const ob steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }
#endif
