#ifndef CURSED_H
#define CURSED_H
#include <float.h>
#include <stdint.h>
#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdalign.h>
#include <stdnoreturn.h>
#include <iso646.h>

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define Packed __attribute__((packed))
#define Asm asm volatile
#define Bwdq(_) _(8) _(16) _(32) _(64)
#define zero64 ((u64)0)
#define word64 8

typedef void u0;

#define inttype(n) \
  typedef int##n##_t i##n;\
  typedef uint##n##_t u##n;
Bwdq(inttype)
#undef inttype

// cursed c standard library declarations
//
// mem{set,cpy,mov} analogs are defined for
// 8, 16, 32 and 64 bit items
#define memn(n) u0\
 set##n(u0*, u##n, u64),\
 cpy##n(u0*, const u0*, u64),\
 cpy##n##r(u0*, const u0*, u64),\
 mov##n(u0*, const u0*, u64);
Bwdq(memn)
#undef memn

// null-terminated string functions
u64 slen(const char*);              // string length
i64 scmp(const char*, const char*), // string compare
    sidx(const char*, char);        // index of character

char cmaj(char), cmin(char); // ASCII case folding

i64 lcprng(i64*); // random numbers
#endif
