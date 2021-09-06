#ifndef _pre_h
#define _pre_h
#include <stdint.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdnoreturn.h>

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define zero64 ((u64)0)
#define word64 8
#define rotr64(x, n) (((x)<<(64-(n)))|((x)>>(n)))
#define BWDQ(_) _(8) _(16) _(32) _(64)

typedef void u0;

#define I(n) \
 typedef int##n##_t i##n;\
 typedef uint##n##_t u##n;
BWDQ(I)
#undef I


// mem{set,cpy,mov} analogs are defined for
// 8, 16, 32 and 64 bit items
#define M(n) u0\
 set##n(u0*, u##n, u64),\
 cpy##n(u0*, const u0*, u64),\
 cpy##n##r(u0*, const u0*, u64),\
 mov##n(u0*, const u0*, u64);
BWDQ(M)
#undef M


// ASCII case folding
#define coff ('a'-'A')
static Inline char cmin(char c) {
 return c >= 'A' && c <= 'Z' ? c + coff : c; }
static Inline char cmaj(char c) {
 return c >= 'a' && c <= 'z' ? c - coff : c; }
#undef coff

// linear congruential pseudorandom number generator
// the multiplier comes from "Computationally Easy, Spectrally
// Good Multipliers for Congruential Pseudorandom Number
// Generators" by Steele & Vigna
#define mul (0xaf251af3b0f025b5)
static Inline i64 lcprng(i64 *s) { return (*s = *s * mul + 1) >> 8; }
#undef mul

// functions for null-terminated byte strings
static Inline i64 scmp(const char *a, const char *b) {
 for (;;a++, b++) if (!(*a && *a == *b)) return *a - *b; }
static Inline u64 slen(const char *s) {
 for (u64 l = 0;;l++) if (!*s++) return l; }
static Inline i64 sidx(const char *s, char c) {
 for (i64 i = 0; *s; s++, i++) if (*s == c) return i;
 return -1; }

#endif
