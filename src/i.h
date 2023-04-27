#ifndef _l_i_h
#define _l_i_h

// thanks !!
//
#include <stdint.h>

typedef intptr_t ob;
typedef struct carrier {
  intptr_t *hp, *sp;
  union mo *ip;
  // memory state
  uintptr_t len;
  intptr_t *pool, *loop;
  union { intptr_t *cp; uintptr_t t0; };
  struct ll { intptr_t *addr; struct ll *next; } *safe;
} *li, *O;

enum status {
  Eof = -1, Ok,
  DomainError,
  OomError, };

typedef union mo {
  enum status (*ap)(O, union mo*, ob*, ob*);
  union mo *m;
  intptr_t x, *ptr;
} *mo;
_Static_assert(sizeof(union mo) == sizeof(intptr_t), "union size");

typedef enum status vm(O, mo, ob*, ob*);

void li_fin(O);
enum status li_ini(O), li_go(O);

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>

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
  vm *act; struct methods *typ;
  ob a, b; } *two;

typedef struct str {
  vm *act; struct methods *typ;
  uintptr_t len; char text[]; } *str;

struct methods {
  ob (*evac)(O, ob, ob*, ob*);
  void (*walk)(O, ob, ob*, ob*),
       (*emit)(O, FILE*, ob);
  bool (*equi)(O, ob, ob);
  enum status (*does)(O, mo, ob*, ob*); };

extern struct methods two_methods, str_methods;

bool nequi(O, ob, ob),
     eq_two(O, ob, ob),
     eq_str(O, ob, ob);
ob cp_two(O, ob, ob*, ob*),
   cp_str(O, ob, ob*, ob*);
void wk_two(O, ob, ob*, ob*),
     wk_str(O, ob, ob*, ob*),
     tx_two(O, FILE*, ob),
     tx_str(O, FILE*, ob);
vm act;
enum status gc(O, mo, ob*, ob*, uintptr_t);
void transmit(O, FILE*, ob);
bool eql(O, ob, ob),
     please(O, uintptr_t),
     pushs(O, ...);
enum status receive(O, FILE*);
mo thd(O, ...), mo_n(O, uintptr_t);
ob list(O, ...);
two pair(O, ob, ob);
str strof(O, const char*);

#define End ((intptr_t)0)
#define Width(_) b2w(sizeof(_))

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

#define A(o) ((two)(o))->a
#define B(o) ((two)(o))->b

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

static Inline void *bump(O f, uintptr_t n) {
  void *x = f->hp;
  return f->hp += n, x; }

static Inline intptr_t pop1(O f) { return *f->sp++; }
ob push1(O, ob);
static Inline intptr_t avail(O f) {
  li_assert(f->sp >= f->hp);
  return f->sp - f->hp; }

static Inline intptr_t height(O f) {
  return f->pool + f->len - f->sp; }

static Inline void *cells(O f, uintptr_t n) {
  return avail(f) < n && !please(f, n) ? 0 : bump(f, n); }

static Inline struct tag *mo_tag(mo k) {
  for (;; k++) if (!k->x) return (struct tag*) k; }

static Inline bool nilp(ob _) { return _ == nil; }
static Inline bool nump(ob _) { return _ & 1; }
static Inline bool homp(ob _) { return !nump(_); }

static Inline bool datp(mo h) { return h->ap == act; }
#define gettyp(x) ((struct methods*)(((ob*)((x)))[1]))
static Inline bool hstrp(mo h) { return datp(h) && gettyp(h) == &str_methods; }
static Inline bool htwop(mo h) { return datp(h) && gettyp(h) == &two_methods; }
static Inline bool strp(ob _) { return homp(_) && hstrp((mo) _); }
static Inline bool twop(ob _) { return homp(_) && htwop((mo) _); }

static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(ob), r = b % sizeof(ob);
  return r ? q + 1 : q; }

// this can give a false positive if x is a fixnum
static Inline bool livep(O v, ob x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static Inline mo mo_ini(void *_, uintptr_t len) {
  struct tag *t = (struct tag*) ((mo) _ + len);
  return t->null = NULL, t->head = _; }

static Inline two two_ini(void *_, ob a, ob b) {
  two w = _; return w->act = act, w->typ = &two_methods,
                    w->a = a, w->b = b, w; }

static Inline str str_ini(void *_, uintptr_t len) {
  str s = _; return s->act = act, s->typ = &str_methods,
                    s->len = len, s; }

#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)
#define Unpack() (ip = f->ip, hp = f->hp, sp = f->sp)
#define Have(n) if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)
#endif
