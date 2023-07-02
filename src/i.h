#ifndef _l_i_h
#define _l_i_h
// thanks !!
//
#include "l.h"
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

typedef intptr_t ob, word;
typedef struct l_state {
  intptr_t *hp, *sp;
  union X *ip;

  // memory state
  uintptr_t len;
  intptr_t *pool, *loop;

  struct mm { intptr_t *addr; struct mm *next; } *safe;
#ifdef _l_mm_static
  uintptr_t t0;
#else
  union { uintptr_t t0; intptr_t *cp; };
#endif
} *state;

typedef enum status {
  Eof = -1,
  Ok = 0,
  DomainError,
  OomError,
} vm(state, union X*, word*, word*);

typedef union X {
  vm *ap;
  word x;
  union X *m;
} X, *verb;

typedef struct two *two;
typedef struct str *str;

// plain threads have a tag at the end
struct tag { X *null, *head, end[]; } *mo_tag(X*);

struct two {
  vm *ap;
  struct methods *mtd;
  word _[2];
} *pair(state, word, word),
  *two_ini(void*, word, word);

struct str {
  vm *ap;
  struct methods *mtd;
  uintptr_t len;
  char text[];
} *strof(state, const char*),
  *str_ini(void*, size_t);

bool
  eq_two(state, word, word),
  eq_str(state, word, word),
  eql(state, word, word),
  please(state, size_t);

word
  push1(state, word),
  push2(state, word, word);

void
  tx_str(state, FILE*, word),
  tx_two(state, FILE*, word),
  *cells(state, size_t),
  transmit(state, FILE*, word);

enum status
  data(state, X*, word*, word*),
  eval(state, word),
  receive(state, FILE*),
  receive2(state, const char*);

extern struct methods {
  word (*evac)(state, word, word*, word*);
  void (*walk)(state, word, word*, word*),
       (*emit)(state, FILE*, word);
  bool (*equi)(state, word, word);
} two_methods, str_methods;

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((word)(_)>>1)
#define putnum(_) (((word)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((struct mm){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define A(o) ((two)(o))->_[0]
#define B(o) ((two)(o))->_[1]
#define pop1(f) (*((f)->sp++))
#define nilp(_) ((_)==nil)
#define nump(_) ((word)(_)&1)
#define homp(_) (!nump(_))
#define Quote "`"
#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)
#define mtd(x) ((struct methods*)(((word*)((x)))[1]))


#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
static Inline bool datp(verb h) { return h->ap == data; }
static Inline bool hstrp(verb h) { return datp(h) && mtd(h) == &str_methods; }
static Inline bool htwop(verb h) { return datp(h) && mtd(h) == &two_methods; }
static Inline bool strp(word _) { return homp(_) && hstrp((X*) _); }
static Inline bool twop(word _) { return homp(_) && htwop((X*) _); }
// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(word), r = b % sizeof(word);
  return q + (r ? 1 : 0); }


static Inline void *bump(state f, size_t n) {
  void *x = f->hp; return f->hp += n, x; }

_Static_assert(-1 >> 1 == -1, "sign extended shift");
_Static_assert(sizeof(union X*) == sizeof(union X), "size");
#endif
