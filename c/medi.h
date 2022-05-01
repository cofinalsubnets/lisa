#ifndef _medi_h
#define _medi_h
// mediterranean C
#include <stdint.h>

// functions for null-terminated byte strings
static inline __attribute__((always_inline))
  intptr_t scmp(const char *a, const char *b) {
    for (;;a++, b++) if (!(*a && *a == *b)) return *a - *b; }

static inline __attribute__((always_inline))
  intptr_t slen(const char *s) {
    for (intptr_t l = 0;;l++) if (!*s++) return l; }

static inline __attribute__((always_inline))
  void setptr(void *_d, intptr_t i, uintptr_t l) {
    for (intptr_t *d = _d; l--; *d++ = i); }
static inline __attribute__((always_inline))
  void cpyptr(void *_d, const void *_s, uintptr_t l) {
    intptr_t *d = _d;
    const intptr_t *s = _s;
    while (l--) *d++ = *s++; }
static inline __attribute__((always_inline))
  void rcpyptr(void *_d, const void *_s, uintptr_t l) {
    intptr_t *d = _d;
    const intptr_t *s = _s;
    while (l--) d[l] = s[l]; }

static inline __attribute__((always_inline))
  intptr_t lcprng(intptr_t s) {
    const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
    return (s * steele_vigna_2021 + 1) >> 8; }
#endif
