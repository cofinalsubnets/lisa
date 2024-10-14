#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

// thanks !!
// basic data type
typedef intptr_t word, *heap, *stack;
// one thread of execution
typedef struct l_core *l_core, *state, *core;
typedef union cell *cell, *thread;
typedef enum l_status {
  Eof = -1,
  Ok = 0,
  Oom = 1,
} l_status, status, vm(core, thread, heap, stack);

// also represented as a union for structured access
typedef struct symbol *symbol;

// character input interface
typedef struct char_in {
  int (*getc)(core, struct char_in*);
  void (*ungetc)(core, struct char_in*, char);
  bool (*eof)(core, struct char_in*);
  word data[];
} *input;

// character output interface
typedef struct char_out {
  void (*putc)(core, struct char_out*, char);
  word data[];
} *output;

// basic data type method table
typedef struct typ {
  word (*copy)(core, word, word*, word*);
  void (*evac)(core, word, word*, word*);
  bool (*equal)(core, word, word);
  void (*emit)(core, output, word);
  //l_mtd_show *show;
  intptr_t (*hash)(core, word);
} *typ;
union cell {
  vm *ap;
  word x;
  union cell *m;
  void *cast; };


// basic data types

// pairs
typedef struct pair {
  vm *ap; typ typ;
  word a, b;
} *two, *pair;

// strings
typedef struct string {
  vm *ap; typ typ;
  size_t len;
  char text[];
} *string;

// symbols
typedef struct symbol {
  vm *ap; typ typ;
  string nom;
  word code;
  struct symbol *l, *r;
} *symbol;

// hash tables
typedef struct table {
  vm *ap; typ typ;
  uintptr_t len, cap;
  struct table_entry {
    word key, val;
    struct table_entry *next;
  } **tab;
} *table;

// runtime core data structure -- 1 core = 1 thread of execution
struct l_core {
  // vm registers
  thread ip; // instruction pointer
  heap hp; // heap pointer
  stack sp; // stack pointer
  // environment
  table dict, macro; // global environment and macros
  word rand; // random seed
  symbol symbols; // internal symbols

  // memory management
  bool (*please)(core, size_t); // gc routine
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

extern struct typ pair_type, string_type, symbol_type, table_type;
extern struct char_in std_input;
extern struct char_out std_output, std_error;

pair
  ini_pair(pair, word, word),
  pairof(core, word, word);
table
  new_table(core),
  table_set(core, table, word, word);
symbol
  literal_symbol(core, const char*),
  intern(core, string);

void
  l_close(core),
  *bump(core, size_t),
  *cells(core, size_t),
  copy_from(core, word*, size_t),
  transmit(core, output, word),
  report(core, output, status),
  print_num(core, output, intptr_t, int);

word
  table_get(core, table, word, word),
  pushs(core, size_t, ...),
  hash(core, word),
  cp(core, word, word*, word*); // for recursive use by evac functions

core l_open(void);
bool
  eql(core, word, word),
  static_please(core, size_t),
  libc_please(core, size_t),
  literal_equal(core, word, word);

status
  eval(core),
  read1i(core, input),
  gc(core, thread, heap, stack, size_t);

vm print, not, rng,
   p2, gensym, ev0,
   Xp, Np, Sp, defmacro,
   ssub, sget, slen, scat,
   symbol_of_string, string_of_symbol,
   prc,
   cons, car, cdr,
   lt, le, eq, gt, ge,
   tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda,
   add, sub, mul, quot, rem,
   data, curry;

#define dtyp(x) ((typ)ptr(x)[1].m)
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
#define ptr(o) ((cell)(o))
#define datp(_) (ptr(_)->ap==data)
#define Pack(f) (f->ip = ip, f->hp = hp, f->sp = sp)
#define Unpack(f) (ip = f->ip, hp = f->hp, sp = f->sp)
#define Vm(n, ...) enum l_status n(core f, thread ip, heap hp, stack sp, ##__VA_ARGS__)
#define Have(n) if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)
#define mix ((uintptr_t)2708237354241864315)
#define bind(n, x) if (!(n = (x))) return 0
#define bounded(a, b, c) ((word)(a)<=(word)(b)&&(word)(b)<(word)(c))
#define op(n, x) (ip = (thread) sp[n], sp[n] = (x), ip->ap(f, ip, hp, sp + n))
#define Do(...) ((__VA_ARGS__), ip->ap(f, ip, hp, sp))

static Inline bool hstrp(cell h) { return datp(h) && dtyp(h) == &string_type; }
static Inline bool htwop(cell h) { return datp(h) && dtyp(h) == &pair_type; }
static Inline bool htblp(cell h) { return datp(h) && dtyp(h) == &table_type; }
static Inline bool hsymp(cell h) { return datp(h) && dtyp(h) == &symbol_type; }
static Inline bool strp(word _) { return homp(_) && hstrp((cell) _); }
static Inline bool twop(word _) { return homp(_) && htwop((cell) _); }
static Inline bool tblp(word _) { return homp(_) && htblp((cell) _); }
static Inline bool symp(word _) { return homp(_) && hsymp((cell) _); }
static Inline void outputs(core f, output o, const char *s) {
  while (*s) o->putc(f, o, *s++); }
static Inline struct tag {
  union cell *null, *head, end[];
} *ttag(thread k) {
  while (k->ap) k++;
  return (struct tag*) k; }

//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
static Inline thread mo_ini(void *_, size_t len) {
  struct tag *t = (struct tag*) ((cell)_ + len);
  return t->null = NULL, t->head = _; }

static Inline string ini_str(string s, size_t len) {
  s->ap = data, s->typ = &string_type, s->len = len;
  return s; }


// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(word), r = b % sizeof(word);
  return q + (r ? 1 : 0); }
_Static_assert(-1 >> 1 == -1, "sign extended shift");
_Static_assert(sizeof(union cell*) == sizeof(union cell), "size");
