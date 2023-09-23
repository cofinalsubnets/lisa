// thanks !!
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

typedef struct G *state;
typedef union cell *cell, *thread;
typedef intptr_t word, *heap, *stack;
typedef enum status {
  Eof = -1, Ok = 0, Dom, Oom,
} vm(state, thread, heap, stack);

struct G {
  union cell *ip;
  word *hp, *sp;
  word len, *pool, *loop;
  struct mm { intptr_t *addr; struct mm *next; } *safe;
  union { uintptr_t t0; intptr_t *cp; }; };

typedef union cell {
  vm *ap;
  word x;
  union cell *m;
} X, *verb;

enum data { Pair, String, };
// plain threads have a tag at the end
struct tag { union cell *null, *head, end[]; } *mo_tag(cell);
typedef struct pair {
  vm *ap;
  word typ;
  word _[2];
} *two, *pair;

typedef struct string {
  vm *ap;
  word typ;
  uintptr_t len;
  char text[];
} *str, *string;

pair cons(state, word, word), two_ini(void*, word, word);
string strof(state, const char*), str_ini(void*, size_t);

size_t llen(word);
long lidx(state, word, word);
verb mo_ini(void*, size_t);

void
  l_fin(state),
  *cells(state, size_t),
  transmit(state, FILE*, word);
bool eql(state, word, word), please(state, size_t);
enum status
  l_evals(state, const char*),
  l_ini(state),
  data(state, cell, word*, word*),
  eval(state, word),
  receive2(state, const char*),
  rx_cstr(state, const char*),
  rx_file(state, FILE*);

thread mo_n(state, size_t);

word
  list(state, ...),
  push1(state, word),
  push2(state, word, word);

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
#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define ptr(x) ((cell)(x))
#define datp(_) (ptr(_)->ap==data)
static Inline bool hstrp(verb h) { return datp(h) && h[1].x == String; }
static Inline bool htwop(verb h) { return datp(h) && h[1].x == Pair; }
static Inline bool strp(word _) { return homp(_) && hstrp((X*) _); }
static Inline bool twop(word _) { return homp(_) && htwop((X*) _); }
// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(word), r = b % sizeof(word);
  return q + (r ? 1 : 0); }

static Inline void *bump(state f, size_t n) {
  void *x = f->hp; return f->hp += n, x; }

vm ap, K, cur, ret, rec, yield, var, br, jump;

_Static_assert(-1 >> 1 == -1, "sign extended shift");
_Static_assert(sizeof(union cell*) == sizeof(union cell), "size");
