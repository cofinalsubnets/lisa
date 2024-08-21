#ifndef _l_i_h
#define _l_i_h
#include "l.h"
#include <assert.h>
#include <time.h>
// thanks !!

// one thread of execution
typedef struct core *state, *core;
// basic data type
typedef intptr_t word, *heap, *stack;
// also represented as a union for structured access
typedef union cell *cell, *thread;
typedef struct symbol *symbol;

// return type of outer C interface functions
typedef enum status {
  Eof = -1, Ok = 0, Oom, } status,
  // vm function type
  vm(core, thread, heap, stack);

struct core {
  // vm registers
  thread ip; // instruction pointer
  heap hp; // heap pointer
  stack sp; // stack pointer
  // environment
  word dict, macro; //
  word count, rand;
  symbol symbols;

  // memory management
  word len, // size of each pool
       *pool, // on pool
       *loop; // off pool
  struct mm { // gc save list
    word *addr; // stack address of value
    struct mm *next; // prior list
  } *safe;
  union { // gc state
    uintptr_t t0; // end time of last gc
    heap cp; }; }; // gc copy pointer

typedef struct typ *typ;
typedef vm code;
union cell {
  vm *ap;
  word x;
  union cell *m;
  typ typ; };

struct tag {
  union cell *null, *head, end[];
} *ttag(cell);

typedef word l_mtd_copy(core, word, word*, word*);
typedef void l_mtd_evac(core, word, word*, word*);
typedef bool l_mtd_equal(core, word, word);
typedef void l_mtd_emit(core, FILE*, word);
typedef word l_mtd_hash(core, word);
l_mtd_copy cp;
//typedef string l_mtd_show(core, word);
//typedef intptr_t l_mtd_hash(core, word);
typedef struct typ {
  l_mtd_copy *copy;
  l_mtd_evac *evac;
  l_mtd_equal *equal;
  l_mtd_emit *emit;
  //l_mtd_show *show;
  l_mtd_hash *hash;
} *typ;

typedef struct pair {
  vm *ap;
  typ typ;
  word a, b;
} *two, *pair;

typedef enum ord { Lt = -1, Eq = 0, Gt = 1, }
  ord(core, word, word);

typedef struct string {
  code *ap;
  typ typ;
  size_t len;
  char text[];
} *string;

typedef struct symbol {
  vm *ap;
  typ typ;
  string nom;
  word code;
  struct symbol *l, *r;
} *symbol;

typedef struct table {
  vm *ap;
  typ typ;
  uintptr_t len, cap;
  struct table_entry {
    word key, val;
    struct table_entry *next;
  } **tab;
} *table;

typedef FILE *source, *sink;
typedef struct char_in {
  int (*getc)(core, struct char_in*);
  void (*ungetc)(core, struct char_in*, char);
  bool (*eof)(core, struct char_in*);
} *input;
typedef struct char_out {
  void (*putc)(core, struct char_out*, char);
} *output;

extern struct typ typ_two, typ_str, sym_typ;

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

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define R(o) ((cell)(o))
#define ptr R
#define datp(_) (ptr(_)->ap==data)
#define End ((word)0) // vararg sentinel

pair
  ini_two(two, word, word),
  pairof(core, word, word);
string
  ini_str(string, size_t),
  strof(core, const char*);
table
  new_table(core),
  table_set(core, table, word, word);
word
  table_get(core, table, word, word),
  table_del(core, table, word, word);
symbol
  symof(core, const char*),
  intern(core, string),
  gensym(core);

thread
  thd(core, size_t, ...),
  mo_n(core, size_t),
  mo_ini(void*, size_t);

void
  *l_malloc(size_t),
  l_free(void*);

void
  l_fin(core),
  *bump(core, size_t),
  *cells(core, size_t),
  transmit(core, sink, word);

bool
  eql(core, word, word),
  please(core, size_t);

status
  l_ini(core),
  eval(core, word),
  read1(core, FILE*),
  reads(core, FILE*);

intptr_t liprng(core);

word pushs(core, size_t, ...);
word hash(core, word);

status gc(core, thread, heap, stack, size_t);
vm data, ap, tap, K, ref, cur, ret, yield, cond, jump,
   print, not,
   p2, apn, tapn,
   Xp, Np, Sp, mbind,
   ssub, sget, slen,
   pr, ppr, spr, pspr, prc,
   cons, car, cdr,
   lt, le, eq, gt, ge,
   seek, peek, poke, trim, thda,
   add, sub, mul, quot, rem;

static Inline bool hstrp(cell h) { return datp(h) && (typ) h[1].x == &typ_str; }
static Inline bool htwop(cell h) { return datp(h) && (typ) h[1].x == &typ_two; }
static Inline bool strp(word _) { return homp(_) && hstrp((cell) _); }
static Inline bool twop(word _) { return homp(_) && htwop((cell) _); }
// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(word), r = b % sizeof(word);
  return q + (r ? 1 : 0); }
static Inline word ror(word x, word n) {
  return (x << ((8 * sizeof(word)) - n)) | (x >> n); }

_Static_assert(-1 >> 1 == -1, "sign extended shift");
_Static_assert(sizeof(union cell*) == sizeof(union cell), "size");
#define Vm(n, ...) enum status\
  n(core f, thread ip, heap hp, stack sp, ##__VA_ARGS__)
#define Have(n) if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define L() printf("# %s:%d\n", __FILE__, __LINE__)
#define mix ((uintptr_t)2708237354241864315)
#define bind(n, x) if (!(n = (x))) return 0
#define bounded(a, b, c) ((word)(a)<=(word)(b)&&(word)(b)<(word)(c))
#define gettyp(x) R(x)[1].typ
#endif
