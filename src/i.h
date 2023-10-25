#ifndef _lisa_i_h
#define _lisa_i_h
#include "lisa.h"
#include <assert.h>
#include <time.h>
// thanks !!

typedef struct lisa *lisa, *state;
typedef uintptr_t size;
typedef intptr_t word, *heap, *stack;
typedef union cell *cell, *thread, *verb;
typedef enum status {
  Ok = 0,
  Dom,
  Oom,
  Eof = -1,
} status,
  code(state, thread, heap, stack);
struct lisa {
  // vm variables
  union cell { // instruction pointer
    code *ap;
    word x;
    union cell *m; } *ip;
  heap hp; // heap pointer
  stack sp; // stack pointer
  // environment
  word dict; // 
  // memory management
  word len, // size of each pool
       *pool, // on pool
       *loop; // off pool
  struct mm {
    intptr_t *addr; // stack address of value
    struct mm *next; // prior list
  } *safe;
  union {
    uintptr_t t0; // timestamp
    heap cp; }; }; // copy pointer

typedef uintptr_t size;
typedef code vm;
struct loop {
  union cell *null, *head, end[];
} *mo_tag(cell);

enum data { Pair, String, };
typedef struct pair {
  code *ap;
  word typ;
  word a, b;
} *two, *pair;

typedef enum order {
  Lt = -1, Eq = 0, Gt = 1,
} order(state, word, word);

typedef struct string {
  code *ap;
  word typ;
  size len;
  char text[];
} *string;

typedef FILE *source, *sink;

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((word)(_)>>1)
#define putnum(_) (((word)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((struct mm){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define A(o) ((two)(o))->a
#define B(o) ((two)(o))->b
#define pop1(f) (*((f)->sp++))
#define nilp(_) ((_)==nil)
#define nump(_) ((word)(_)&1)
#define homp(_) (!nump(_))
#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)
#define Unpack() (ip = f->ip, hp = f->hp, sp = f->sp)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define ptr(x) ((cell)(x))
#define datp(_) (ptr(_)->ap==data)
#define End ((intptr_t)0) // vararg sentinel

pair cons(state, word, word);
string
  buf_new(state),
  buf_grow(state, string),
  strof(state, const char*),
  str_ini(void*, size_t);

size_t llen(word);
long lidx(state, word, word);

void
  l_fin(state),
  *cells(state, size_t),
  transmit(state, sink, word);

bool
  eql(state, word, word),
  please(state, size_t);

enum status
  l_ini(lisa),
  eval(lisa, word),
  report(lisa, enum status),
#ifdef testing
  self_test(lisa),
#endif
  parse_source(state, source),
  read_source(state, source);

thread
  mo_n(state, size_t),
  mo_ini(void*, size_t);

word
  assoc(state, word, word),
  assq(state, word, word),
  dict_lookup(state, word),
  push1(state, word),
  push2(state, word, word),
  push3(state, word, word, word);
status
  P1(state, word),
  P2(state, word, word);

vm data, ap, tap, K, ref, curry, ret, yield, cond, jump,
   print, tie,
   eqp, not,
   lt, le, gt, ge,
   add;

static Inline bool hstrp(cell h) { return datp(h) && h[1].x == String; }
static Inline bool htwop(cell h) { return datp(h) && h[1].x == Pair; }
static Inline bool strp(word _) { return homp(_) && hstrp((cell) _); }
static Inline bool twop(word _) { return homp(_) && htwop((cell) _); }
// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(word), r = b % sizeof(word);
  return q + (r ? 1 : 0); }

static Inline void *bump(state f, size_t n) {
  void *x = f->hp; return f->hp += n, x; }

_Static_assert(-1 >> 1 == -1, "sign extended shift");
_Static_assert(sizeof(union cell*) == sizeof(union cell), "size");
#endif
