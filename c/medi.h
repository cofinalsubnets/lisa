#ifndef _medi_h
#define _medi_h
// mediterranean C
#include <stdint.h>
#define BWDQ(_) _(8) _(16) _(32) _(64)
#define I(n) typedef int##n##_t i##n; typedef uint##n##_t u##n;
BWDQ(I)
#undef I

// functions for null-terminated byte strings
static inline __attribute__((always_inline))
  int64_t scmp(const char *a, const char *b) {
    for (;;a++, b++) if (!(*a && *a == *b)) return *a - *b; }

static inline __attribute__((always_inline))
  int64_t slen(const char *s) {
    for (u64 l = 0;;l++) if (!*s++) return l; }

static inline __attribute__((always_inline))
  int64_t sidx(const char *s, char c) {
    for (i64 i = 0; *s; s++, i++) if (*s == c) return i;
    return -1; }

#define memn(n)\
  static inline __attribute__((always_inline))\
    void set##n(void*_d,u##n i,u64 l) {\
      for(u##n*d=_d; l--; *d++=i); }\
    \
  static inline __attribute__((always_inline))\
    void cpy##n(void*_d,const void*_s, u64 l) {\
      u##n*d=_d;\
      const u##n*s=_s;\
      while (l--) *d++=*s++; }\
    \
  static inline __attribute__((always_inline))\
    void cpy##n##r(void*_d,const void*_s, u64 l) {\
      u##n*d=_d;\
      const u##n*s=_s;\
      while (l--) d[l]=s[l]; }\
    \
  static inline __attribute__((always_inline))\
    void mov##n(void*_d,const void*_s, u64 l) {\
      if (_d<_s) cpy##n(_d, _s, l);\
      else if (_d>_s) cpy##n##r(_d, _s, l); }

BWDQ(memn)

#undef memn
#undef BWDQ

// case folding
#define coff ('a'-'A')
static inline __attribute__((always_inline))
  char cmin(char c) {
    return c >= 'A' && c <= 'Z' ? c + ('a'-'A') : c; }

static inline __attribute__((always_inline))
  char cmaj(char c) {
   return c >= 'a' && c <= 'z' ? c + ('A'-'a') : c; }

static inline __attribute__((always_inline))
  int64_t lcprng(int64_t s) {
    const int64_t steele_vigna_2021 = 0xaf251af3b0f025b5;
    return (s * steele_vigna_2021 + 1) >> 8; }
#endif
