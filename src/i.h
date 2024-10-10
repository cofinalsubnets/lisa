#ifndef _l_i_h
#define _l_i_h
#include "l.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <stdio.h>
// thanks !!

// one thread of execution
typedef l_core state, core;
// basic data type
typedef l_word word, *heap, *stack;
// also represented as a union for structured access
typedef union cell *cell, *thread;
typedef struct symbol *symbol;

// return type of outer C interface functions
typedef enum status {
  Eof = -1, Ok = 0, Oom, } status,
  // vm function type
  vm(core, thread, heap, stack);

typedef struct table *table;

struct l_core {
  // vm registers
  thread ip; // instruction pointer
  heap hp; // heap pointer
  stack sp; // stack pointer
  // environment
  table dict, macro;
  word count, rand;
  symbol symbols;

  // memory management
  bool (*please)(core, size_t);
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

//typedef string l_mtd_show(core, word);
//typedef intptr_t l_mtd_hash(core, word);
typedef struct typ {
  word (*copy)(core, word, word*, word*);
  void (*evac)(core, word, word*, word*);
  bool (*equal)(core, word, word);
  void (*emit)(core, FILE*, word);
  //l_mtd_show *show;
  intptr_t (*hash)(core, word);
} *typ;
word cp(core, word, word*, word*); // for recursive use by evac functions
bool literal_equal(core, word, word);

typedef struct pair {
  vm *ap;
  typ typ;
  word a, b;
} *two, *pair;

typedef enum ord { Lt = -1, Eq = 0, Gt = 1, }
  ord(core, word, word);

typedef struct string {
  vm *ap;
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

extern struct typ pair_type, string_type, symbol_type, table_type;

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((word)(_)>>1)
#define putnum(_) (((word)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((struct mm){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define Protect MM
#define Unprotect UM
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
#define Pack(f) (f->ip = ip, f->hp = hp, f->sp = sp)
#define Unpack(f) (ip = f->ip, hp = f->hp, sp = f->sp)

pair
  ini_pair(pair, word, word),
  pairof(core, word, word);
string
  ini_str(string, size_t),
  literal_string(core, const char*);
table
  new_table(core),
  table_set(core, table, word, word);
word
  table_get(core, table, word, word),
  table_del(core, table, word, word);

symbol
  literal_symbol(core, const char*),
  intern(core, string);

thread
  thd(core, size_t, ...),
  mo_n(core, size_t),
  mo_ini(void*, size_t);

void
  *bump(core, size_t),
  *cells(core, size_t),
  copy_from(core, word*, size_t),
  transmit(core, sink, word);

size_t llen(word);
long lidx(core, word, word);
word
  lassoc(core, word, word),
  lconcat(core, word, word),
  rlconcat(core, word, word);

bool
  eql(core, word, word),
  static_please(core, size_t),
  libc_please(core, size_t);

status
  l_ini(core, bool (*)(core, size_t), size_t, word*),
  eval(core),
  read1(core, FILE*);

intptr_t l_rand(core);

word pushs(core, size_t, ...);
word hash(core, word);

status gc(core, thread, heap, stack, size_t);
vm print, not, rng,
   p2, gensym,
   Xp, Np, Sp, defmacro,
   ssub, sget, slen,
   symbol_of_string, string_of_symbol,
   pr, ppr, spr, pspr, prc,
   cons, car, cdr,
   lt, le, eq, gt, ge,
   tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda,
   add, sub, mul, quot, rem,
   data, curry;

#define dtyp(x) R(x)[1].typ
#define gettyp dtyp
static Inline bool hstrp(cell h) { return datp(h) && dtyp(h) == &string_type; }
static Inline bool htwop(cell h) { return datp(h) && dtyp(h) == &pair_type; }
static Inline bool htblp(cell h) { return datp(h) && dtyp(h) == &table_type; }
static Inline bool hsymp(cell h) { return datp(h) && dtyp(h) == &symbol_type; }
static Inline bool strp(word _) { return homp(_) && hstrp((cell) _); }
static Inline bool twop(word _) { return homp(_) && htwop((cell) _); }
static Inline bool tblp(word _) { return homp(_) && htblp((cell) _); }
static Inline bool symp(word _) { return homp(_) && hsymp((cell) _); }
// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(word), r = b % sizeof(word);
  return q + (r ? 1 : 0); }
_Static_assert(-1 >> 1 == -1, "sign extended shift");
_Static_assert(sizeof(union cell*) == sizeof(union cell), "size");
#define Vm(n, ...) enum status\
  n(core f, thread ip, heap hp, stack sp, ##__VA_ARGS__)
#define Have(n) if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)
#define L() printf("# %s:%d\n", __FILE__, __LINE__)
#define HashK ((uintptr_t)2708237354241864315)
#define mix HashK
#define bind(n, x) if (!(n = (x))) return 0
#define bounded(a, b, c) ((word)(a)<=(word)(b)&&(word)(b)<(word)(c))
#define RetN(n, x) (ip = (thread) sp[n], sp[n] = (x), ip->ap(f, ip, hp, sp + n))
#define op RetN
#define Do(...) ((__VA_ARGS__), ip->ap(f, ip, hp, sp))
#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))
#endif
