#include "cursed.h"
// freestanding substitutes for some libc functionality.
//
// mem{set,cpy} analogs
#define memn(n)\
 u0 set##n(u0*_d,u##n i,u64 l) {\
  for(u##n*d=_d;l--;*d++=i); }\
 u0 cpy##n(u0*_d,const u0*_s, u64 l) {\
  u##n*d=_d; const u##n*s=_s;\
  while (l--) *d++=*s++; }\
 u0 cpy##n##r(u0*_d,const u0*_s, u64 l) {\
  u##n*d=_d+l; const u##n*s=_s+l;\
   while (l--) *--d=*--s; }\
 u0 mov##n(u0*_d,const u0*_s, u64 l) {\
  if (_d<_s) cpy##n(_d, _s, l);\
  else if (_d>_s) cpy##n##r(_d, _s, l); }
Bwdq(memn)

// functions for null-terminated byte strings
i64 scmp(const char *a, const char *b) {
 for (;;a++, b++) if (!(*a && *a == *b)) return *a - *b; }
u64 slen(const char *s) {
 for (u64 l = 0;;l++) if (!*s++) return l; }
i64 sidx(const char *s, char c) {
 for (i64 i = 0; *s; s++, i++) if (*s == c) return i;
 return -1; }

// ASCII case folding
#define coff ('a'-'A')
char cmin(char c) {
 return c >= 'A' && c <= 'Z' ? c + coff : c; }
char cmaj(char c) {
 return c >= 'a' && c <= 'z' ? c - coff : c; }

// the multiplier comes from "Computationally Easy, Spectrally
// Good Multipliers for Congruential Pseudorandom Number
// Generators" by Steele & Vigna
#define mul 0xaf251af3b0f025b5
i64 lcprng(i64 *s) {
 return (*s = *s * mul + 1) >> 8; }
