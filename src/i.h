
// thanks !!
//
#include <stdint.h>

typedef intptr_t ob, word, Z;
typedef uintptr_t size, N;
typedef struct carrier {
  intptr_t *hp, *sp;
  union mo *ip;
  // memory state
  uintptr_t len;
  intptr_t *pool, *loop;
  union { intptr_t *cp; uintptr_t t0; };
  struct ll { intptr_t *addr; struct ll *next; } *safe;
} *li, *O, *state;

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>

typedef enum status {
  Eof = EOF,
  Ok = 0,
  DomainError = EDOM,
  OomError = ENOMEM, } status;

typedef union mo {
  status (*ap)(state, union mo*, word*, word*);
  union mo *m;
  word x;
} *mo, *verb;
_Static_assert(sizeof(union mo) == sizeof(word), "union size");

status act(state, verb, word*, word*);

void li_fin(state);
status li_ini(state), li_go(state);

#ifdef testing
#define li_assert assert
#else
#define li_assert(...)
#endif

enum status self_test(O);


struct tag {
  void **null;
  mo head;
  union mo end[]; };

typedef struct two {
  status (*act)(state, verb, word*, word*);
  struct methods *typ;
  ob _[2]; } *two;

typedef struct str {
  status (*act)(state, verb, word*, word*);
  struct methods *typ;
  uintptr_t len;
  char text[]; } *str;

struct methods {
  ob (*evac)(O, ob, ob*, ob*);
  void (*walk)(O, ob, ob*, ob*),
       (*emit)(O, FILE*, ob);
  bool (*equi)(O, ob, ob); };

extern struct methods two_methods, str_methods;

bool nequi(O, ob, ob),
     eq_two(O, ob, ob),
     eq_str(O, ob, ob);
static ob cp_two(O, ob, ob*, ob*),
   cp_str(O, ob, ob*, ob*);
static void wk_two(O, ob, ob*, ob*),
     wk_str(O, ob, ob*, ob*),
     tx_two(O, FILE*, ob),
     tx_str(O, FILE*, ob);
enum status gc(O, mo, ob*, ob*, uintptr_t);
void transmit(O, FILE*, ob);
bool eql(O, ob, ob),
     please(O, uintptr_t),
     kstrq(str, const char*),
     pushs(O, ...);
enum status receive(O, FILE*), receive2(state, char*);
mo thd(O, ...), mo_n(O, uintptr_t);
ob list(O, ...);
two pair(O, ob, ob);
str strof(O, const char*);
verb compile_expression(state f, word x);
status curry(state, verb, word*, word*),
       apply(state, verb, word*, word*),
       branch(state, verb, word*, word*),
       Kj(state, verb, word*, word*),
       K(state, verb, word*, word*),
       ref(state, verb, word*, word*),
       yield(state, verb, word*, word*),
       jump(state, verb, word*, word*),
       recn(state, verb, word*, word*),
       retn(state, verb, word*, word*);

#define End ((intptr_t)0)

#define getnum(_) ((ob)(_)>>1)
#define putnum(_) (((ob)(_)<<1)|1)
_Static_assert(-1 >> 1 == -1, "signed shift");

#define nil putnum(0)
#define T putnum(-1)

#define mm(r) ((v->safe=&((struct ll){(ob*)(r),v->safe})))
#define um (v->safe=v->safe->next)
#define MM(f,r) ((f->safe=&((struct ll){(ob*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define F(_) ((mo)(_)+1)
#define G(_) (*(mo)(_))

#define A(o) ((two)(o))->_[0]
#define B(o) ((two)(o))->_[1]

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

static Inline void *bump(state f, size n) {
  void *x = f->hp;
  f->hp += n;
  return x; }

static Inline word pop1(state f) { return *f->sp++; }
word push1(state, word);
static Inline size avail(state f) { return f->sp - f->hp; }

static Inline void *cells(state f, size n) {
  return avail(f) < n && !please(f, n) ? 0 : bump(f, n); }

static Inline bool nilp(word _) { return _ == nil; }
static Inline bool nump(word _) { return _ & 1; }
static Inline bool homp(word _) { return !nump(_); }

static Inline bool datp(verb h) { return h->ap == act; }
#define gettyp(x) ((struct methods*)(((ob*)((x)))[1]))
static Inline bool hstrp(verb h) { return datp(h) && gettyp(h) == &str_methods; }
static Inline bool htwop(verb h) { return datp(h) && gettyp(h) == &two_methods; }
static Inline bool strp(word _) { return homp(_) && hstrp((mo) _); }
static Inline bool twop(word _) { return homp(_) && htwop((mo) _); }

static Inline size b2w(size b) {
  size_t q = b / sizeof(ob), r = b % sizeof(ob);
  return r ? q + 1 : q; }
#define Width(_) b2w(sizeof(_))

#define Quote "`"
#define Cond "?"
#define Lambda "\\"
#define println(x) (transmit(f,stdout,x),puts(""))
