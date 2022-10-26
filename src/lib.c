#include "lisa.h"

// freestanding libc substitutes

intptr_t lcprng(intptr_t s) {
  const int64_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }

void setw(void *x, uintptr_t i, size_t l) {
  for (uintptr_t *d = x; l--; *d++ = i); }

void cpyw(void *x, const void *y, size_t l) {
  uintptr_t *d = x;
  const uintptr_t *s = y;
  while (l--) *d++ = *s++; }

void rcpyw(void *x, const void *y, size_t l) {
  uintptr_t *d = (uintptr_t*) x + (l - 1);
  const uintptr_t *s = (const uintptr_t*) y + (l - 1);
  while (l--) *d-- = *s--; }

void cpy8(void *x, const void *y, size_t l) {
  size_t quot = l / sizeof(uintptr_t),
         rem = l % sizeof(uintptr_t);
  cpyw(x, y, quot);
  uint8_t *d = (uint8_t*) (((uintptr_t*)x) + quot);
  const uint8_t *s = (uint8_t*) (((uintptr_t*)y)+quot);
  while (rem--) *d++ = *s++; }

char cmin(char c) {
  return c >= 'A' && c <= 'Z' ? c + ('a'-'A') : c; }

size_t slen(const char *s) {
  for (size_t l = 0;;l++) if (!*s++) return l; }

int scmp(const char *a, const char *b) {
  for (;;a++, b++) if (!(*a && *a == *b)) return *a - *b; }
