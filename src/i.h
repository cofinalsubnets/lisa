// thanks !!
//
#include "ll.h"
#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>

typedef intptr_t ob, word;
typedef uintptr_t size;
typedef struct carrier {
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

typedef union mo {
  enum status (*ap)(state, union mo*, word*, word*);
  union mo *m;
  word x; } *mo, *verb;

typedef struct two {
  status (*act)(state, verb, word*, word*);
  struct methods *typ;
  ob _[2]; } *two;

typedef struct str {
  status (*act)(state, verb, word*, word*);
  struct methods *typ;
  uintptr_t len;
  char text[]; } *str;

two pair(state, word, word);
str strof(state, const char*);
status data(state, verb, word*, word*),
       self_test(state),
       receive2(state, char*),
       yield(state, verb, word*, word*);

#define getnum(_) ((ob)(_)>>1)
#define putnum(_) (((ob)(_)<<1)|1)

#define nil putnum(0)

#define MM(f,r) ((f->safe=&((struct ll){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))

#define A(o) ((two)(o))->_[0]
#define B(o) ((two)(o))->_[1]

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

word push1(state, word);
#define pop1(f) (*((f)->sp++))
#define nilp(_) ((_)==nil)
#define nump(_) ((ob)(_)&1)
#define homp(_) (!nump(_))
extern struct methods two_methods, str_methods;
static Inline bool datp(verb h) { return h->ap == data; }
#define gettyp(x) ((struct methods*)(((ob*)((x)))[1]))
static Inline bool hstrp(verb h) { return datp(h) && gettyp(h) == &str_methods; }
static Inline bool htwop(verb h) { return datp(h) && gettyp(h) == &two_methods; }
static Inline bool strp(word _) { return homp(_) && hstrp((mo) _); }
static Inline bool twop(word _) { return homp(_) && htwop((mo) _); }
#define Quote "`"

_Static_assert(sizeof(union mo) == sizeof(word), "union size");
_Static_assert(-1 >> 1 == -1, "signed shift");
