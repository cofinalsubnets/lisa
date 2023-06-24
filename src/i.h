// thanks !!
//
#include "ll.h"
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
} *li, *O, *state;

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

_Static_assert(sizeof(obj) == sizeof(word), "sizes");

typedef struct two {
  enum status (*act)(state, verb, word*, word*);
  struct methods *typ;
  word _[2]; } *two;

typedef struct str {
  enum status (*act)(state, verb, word*, word*);
  struct methods *typ;
  uintptr_t len;
  char text[]; } *str;

two pair(state, word, word);
str strof(state, const char*);
enum status
  data(state, verb, word*, word*),
  self_test(state),
  receive(state, FILE*),
  receive2(state, const char*);
void *cells(state, size_t);
#define Width(_) b2w(sizeof(_))


#define getnum(_) ((word)(_)>>1)
#define putnum(_) (((word)(_)<<1)|1)

#define nil putnum(0)

#define MM(f,r) ((f->safe=&((struct ll){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))

#define A(o) ((two)(o))->_[0]
#define B(o) ((two)(o))->_[1]

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

word push1(state, word);
bool push2(state, word, word);
#define pop1(f) (*((f)->sp++))
#define nilp(_) ((_)==nil)
#define nump(_) ((word)(_)&1)
#define homp(_) (!nump(_))
extern struct methods two_methods, str_methods;
static Inline bool datp(verb h) { return h->ap == data; }
#define gettyp(x) ((struct methods*)(((word*)((x)))[1]))
static Inline bool hstrp(verb h) { return datp(h) && gettyp(h) == &str_methods; }
static Inline bool htwop(verb h) { return datp(h) && gettyp(h) == &two_methods; }
static Inline bool strp(word _) { return homp(_) && hstrp((mo) _); }
static Inline bool twop(word _) { return homp(_) && htwop((mo) _); }
#define Quote "`"

enum status eval(state, word);
_Static_assert(sizeof(union mo) == sizeof(word), "union size");
_Static_assert(-1 >> 1 == -1, "signed shift");
void transmit(state, FILE*, ob);
void
  wk_two(O, ob, ob*, ob*),
  wk_str(O, ob, ob*, ob*),
  tx_two(O, FILE*, ob),
  tx_str(O, FILE*, ob);


struct methods {
  ob (*evac)(state, ob, ob*, ob*);
  void (*walk)(state, ob, ob*, ob*),
       (*emit)(state, FILE*, ob);
  bool (*equi)(state, ob, ob); };
// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  ldiv_t _ = ldiv(b, sizeof(word));
  return _.quot + (_.rem ? 1 : 0); }
str str_ini(void*, size_t);
