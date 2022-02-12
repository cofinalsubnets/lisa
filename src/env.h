#include <stdint.h>
#include <stdbool.h>
#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define BWDQ(_) _(8) _(16) _(32) _(64)

typedef void u0;
typedef bool u1;
#define I(n) \
 typedef int##n##_t i##n; typedef uint##n##_t u##n;
BWDQ(I)
#undef I

// ASCII case folding
static Inline char cmin(char c) {
  return 'A' <= c && c <= 'Z' ? c - 'A' + 'a' : c; }
static Inline char cmaj(char c) {
  return 'a' <= c && c <= 'z' ? c - 'a' + 'A' : c; }

// functions for null-terminated byte strings
static Inline i64 scmp(const char *a, const char *b) {
  for (;; a++, b++) if (!(*a && *a == *b)) return *a - *b; }
static Inline u64 slen(const char *s) {
  for (u64 l = 0;;l++) if (!*s++) return l; }
static Inline i64 sidx(const char *s, char c) {
  for (i64 i = 0; *s; s++, i++) if (*s == c) return i;
  return -1; }

// linear congruential pseudorandom number generator
// the multiplier comes from "Computationally Easy, Spectrally
// Good Multipliers for Congruential Pseudorandom Number
// Generators" by Steele & Vigna
#define LCPRNG(s) (((s) * 0xaf251af3b0f025b5ll + 1) >> 8)
static Inline i64 lcprng(i64 *s) { return *s = LCPRNG(*s); }

static Inline u64 w2b(u64 w) { return w * 8; }
static Inline u64 b2w(u64 b) { return b / 8 + (b % 8 && 1); }
// mem{set,cpy,mov} analogs are defined for
// 8, 16, 32 and 64 bit items
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
