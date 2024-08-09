#ifndef _l_i_h
#define _l_i_h
#include "l.h"
#include <assert.h>
#include <time.h>
// thanks !!

typedef struct core *state, *core;
typedef intptr_t word, *heap, *stack;
typedef union cell *cell, *thread, *verb;
typedef enum status { Eof = -1, Ok = 0, Dom, Oom, } status,
  code(core, thread, heap, stack);
typedef struct input *input;
typedef struct output *output;

typedef uintptr_t size;
struct core {
  // vm variables
  thread ip; // instruction pointer
  heap hp; // heap pointer
  stack sp; // stack pointer
  // environment
  word dict, macro; //
  word count, rand;


  // memory management
  word len, // size of each pool
       *pool, // on pool
       *loop; // off pool
  struct mm { // gc save list
    intptr_t *addr; // stack address of value
    struct mm *next; // prior list
  } *safe;
  union { // gc state
    uintptr_t t0; // end time of last gc
    heap cp; }; }; // gc copy pointer

union cell {
  code *ap;
  word x;
  union cell *m; };

typedef code vm;
struct loop {
  union cell *null, *head, end[];
} *mo_tag(cell);

enum data { Pair, String, };
typedef struct two {
  code *ap;
  word typ;
  word a, b;
} *two, *pair;

typedef enum ord { Lt = -1, Eq = 0, Gt = 1, }
  ord(core, word, word);

typedef struct string {
  code *ap;
  word typ;
  size len;
  char text[];
} *string;

typedef FILE *source, *sink;

typedef struct typ {
  word (*copy)(core, word, word*, word*);
  void (*evac)(core, word, word*, word*);
  bool (*equal)(core, word, word);
  void (*emit)(core, FILE*, word);
//  string (*show)(core, word);
//  intptr_t (*hash)(core, word);
} *typ;

extern struct typ typ_two, typ_str;

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
#define ptr(x) ((thread)(x))
#define datp(_) (ptr(_)->ap==data)
#define End ((word)0) // vararg sentinel

two
  ini_two(two, word, word),
  cons(core, word, word);
string
  ini_str(string, size_t),
  buf_new(core),
  buf_grow(core, string),
  strof(core, const char*),
  str_ini(void*, size_t);

size_t llen(word);
long lidx(core, word, word);

void
  *l_malloc(size_t),
  l_free(void*),
  l_fin(core),
  *cells(core, size_t),
  transmit(core, sink, word);

bool
  eql(core, word, word),
  please(core, size_t);


status
  l_ini(core),
  eval(core, word),
  report(core, status),
  read1(core, FILE*),
  reads(core, FILE*);

thread
  thd(core, size_t, ...),
  mo_n(core, size_t),
  mo_ini(void*, size_t);

word
  dict_lookup(core, word),
  assoc(core, word, word),
  lookup(core, word, word),
  pushs(core, size_t, ...);

status gc(core, thread, heap, stack, size_t);
vm data, ap, tap, K, ref, cur, ret, yield, cond, jump,
   print, not,
   p2, apn, tapn,
   Xp, Np, Sp, mbind,
   ssub, sget, slen,
   pr, ppr, spr, pspr, prc,
   xons, car, cdr,
   lt, le, eq, gt, ge,
   seek, peek, poke, trim, thda,
   add, sub, mul, quot, rem;

static Inline bool hstrp(cell h) { return datp(h) && h[1].x == String; }
static Inline bool htwop(cell h) { return datp(h) && h[1].x == Pair; }
static Inline bool strp(word _) { return homp(_) && hstrp((cell) _); }
static Inline bool twop(word _) { return homp(_) && htwop((cell) _); }
// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(word), r = b % sizeof(word);
  return q + (r ? 1 : 0); }

static Inline void *bump(core f, size_t n) {
  void *x = f->hp; return f->hp += n, x; }

static Inline void println(core f, word x, FILE *out) {
  transmit(f, out, x);
  fputc('\n', out); }

_Static_assert(-1 >> 1 == -1, "sign extended shift");
_Static_assert(sizeof(union cell*) == sizeof(union cell), "size");
#define Vm(n, ...) enum status\
  n(core f, thread ip, heap hp, stack sp, ##__VA_ARGS__)
#define Have(n) if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)
#define L() printf("# %s:%d\n", __FILE__, __LINE__)
#define LL(f, x) (printf("# %s:%d ", __FILE__, __LINE__), println(f, x, stdout))
#endif
