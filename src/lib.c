#include "la.h"
intptr_t lcprng(intptr_t s) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }
void setw(void *x, intptr_t i, size_t l) {
  for (intptr_t *d = x; l--; *d++ = i); }

void cpyw(void *x, const void *y, size_t l) {
  intptr_t *d = x;
  const intptr_t *s = y;
  while (l--) *d++ = *s++; }

void rcpyw(void *x, const void *y, size_t l) {
  intptr_t *d = (ob*) x + (l - 1);
  const intptr_t *s = (const intptr_t*) y + (l - 1);
  while (l--) *d-- = *s--; }

#define coff ('a'-'A')
char cmin(char c) {
 return c >= 'A' && c <= 'Z' ? c + coff : c; }
size_t slen(const char *s) {
 for (size_t l = 0;;l++) if (!*s++) return l; }
int scmp(const char *a, const char *b) {
 for (;;a++, b++) if (!(*a && *a == *b)) return *a - *b; }
