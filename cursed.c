#include "cursed.h"
// freestanding substitutes for some libc functionality.
//
// mem{set,cpy} analogs
u0 rep64(u0* _d, u64 i, u64 l) { // fill with a word
 for (u64 *d = _d; l--; *d++=i); }
u0 cpy64(u0* _d, const u0* _s, u64 l) { // copy words
 u64 *d = _d; const u64 *s = _s;
 while (l--) *d++=*s++; }
u0 cpy8(u0* _d, const u0* _s, u64 l) { // copy bytes
 u8 *d = _d; const u8 *s = _s;
 while (l--) *d++ = *s++; }

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
i64 lcprng(i64 *s) {
 return (*s = *s * 0xaf251af3b0f025b5 + 1) >> 8; }
