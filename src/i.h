#ifndef _l_i_h
#define _l_i_h
// thanks !!
//
#include "l.h"
#include <stdint.h>
#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stdio.h>

typedef intptr_t ob, word;
typedef struct l_state {
  intptr_t *hp, *sp;
  union mo *ip;
  // memory state
  uintptr_t len;
  intptr_t *pool, *loop;
  union { intptr_t *cp; uintptr_t t0; };
  struct ll { intptr_t *addr; struct ll *next; } *safe;
} *li, *state;

typedef enum status {
  Eof = -1,
  Ok = 0,
  DomainError,
  OomError, } status;

typedef union {
  intptr_t num;
  union mo *ptr; } obj;

typedef union mo {
  enum status (*ap)(state, union mo*, word*, word*);
  word x;
  union mo *m;
  obj ob;
} *mo, *verb;
struct tag {
  void **null;
  mo head;
  union mo end[]; };

struct methods {
  ob (*evac)(state, ob, ob*, ob*);
  void (*walk)(state, ob, ob*, ob*),
       (*emit)(state, FILE*, ob);
  bool (*equi)(state, ob, ob); };

typedef struct two {
  enum status (*act)(state, verb, word*, word*);
  struct methods *typ;
  word _[2]; } *two;

typedef struct str {
  enum status (*act)(state, verb, word*, word*);
  struct methods *typ;
  uintptr_t len;
  char text[]; } *str;

_Static_assert(sizeof(obj) == sizeof(word), "sizes");
_Static_assert(sizeof(union mo) == sizeof(word), "union size");
_Static_assert(-1 >> 1 == -1, "signed shift");

bool
  eql(state, word, word),
  please(state, size_t);
two pair(state, word, word);
str strof(state, const char*);
enum status
  eval(state, word),
  data(state, verb, word*, word*),
  self_test(state),
  receive(state, FILE*),
  receive2(state, const char*);
void
  *cells(state, size_t),
  transmit(state, FILE*, ob);
ob pushn(state, size_t, ...);
str str_ini(void*, size_t);
two two_ini(void*, ob, ob);

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((word)(_)>>1)
#define putnum(_) (((word)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((struct ll){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define A(o) ((two)(o))->_[0]
#define B(o) ((two)(o))->_[1]
#define pop1(f) (*((f)->sp++))
#define nilp(_) ((_)==nil)
#define nump(_) ((word)(_)&1)
#define homp(_) (!nump(_))
#define Quote "`"
#define Vm(n, ...) enum status n(state f, verb ip, word *hp, word *sp, ##__VA_ARGS__)
#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)
#define gettyp(x) ((struct methods*)(((word*)((x)))[1]))
typedef Vm(vm);
Vm(gc, size_t);


#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
extern struct methods two_methods, str_methods;
static Inline bool datp(verb h) { return h->ap == data; }
static Inline bool hstrp(verb h) { return datp(h) && gettyp(h) == &str_methods; }
static Inline bool htwop(verb h) { return datp(h) && gettyp(h) == &two_methods; }
static Inline bool strp(word _) { return homp(_) && hstrp((mo) _); }
static Inline bool twop(word _) { return homp(_) && htwop((mo) _); }
// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  ldiv_t _ = ldiv(b, sizeof(word));
  return _.quot + (_.rem ? 1 : 0); }

static Inline struct tag *mo_tag(verb k) {
  while (k->x) k++;
  return (struct tag*) k; }

static Inline void *bump(state f, size_t n) {
  void *x = f->hp; return f->hp += n, x; }
#endif
