// this is the header for cursed C, a library for 64 bit C
// systems programming. it includes the freestanding C library
// headers, some cursed types and macros, and prototypes for
// cursed functions that define an analogous but not strictly
// compatible API to some of the functionality of libc.
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

typedef void u0;

#define inttype(n) \
  typedef int##n##_t i##n;\
  typedef uint##n##_t u##n
inttype(8); inttype(16); inttype(32); inttype(64);
#undef inttype

#define zero64 ((u64)0)
#define word64 8

// cursed c standard library declarations

// null-terminated string functions
u64 slen(const char*);              // string length
i64 scmp(const char*, const char*), // string compare
    sidx(const char*, char);        // index of character

// ASCII case folding
char cmaj(char), cmin(char);

// random number generation
i64 lcprng(i64*);

// mem{cpy,set}
u0 cpy8(u0*, const u0*, u64),
   cpy64(u0*, const u0*, u64),
   rep64(u0*, u64, u64);
#endif
