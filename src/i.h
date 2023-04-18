#ifndef _l_i_h
#define _l_i_h
#include <stdint.h>

typedef intptr_t ob;
typedef struct V {
  ob *hp, *sp;
  // memory state
  uintptr_t len;
  ob *pool, *loop;
  union { ob *cp; uintptr_t t0; };
  struct ll { ob *addr; struct ll *next; } *safe; } *li, *O;
typedef enum status {
  More = -2, Eof = -1, Ok,
  DomainError, ArityError,
  NameError, SyntaxError,
  SystemError, OomError,
} vm(O), (**mo)(O);

void li_fin(li);
enum status li_ini(li);

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>

_Static_assert(-1 >> 1 == -1, "signed shift");
_Static_assert(sizeof(size_t) == sizeof(void*), "size_t");

// thanks !!
//

enum data_type {
  Str = 1,
  Two, };

struct tag { vm **null, **head, *end[]; };
typedef struct two {
  vm *act; intptr_t typ;
  ob a, b; } *two;

typedef struct str {
  vm *act; intptr_t typ;
  uintptr_t len; char text[]; } *str;
vm act;
void transmit(li, FILE*, ob);
bool eql(li, ob, ob), please(li, size_t), pushs(li, ...);
enum status li_go(li), receive(li, FILE*);
mo thd(li, ...), mo_n(li, size_t);
two pair(li, ob, ob);
str strof(li, const char*);
ob *new_pool(size_t);

#define End ((ob)0)
#define Width(_) b2w(sizeof(_))

#define getnum(_) ((ob)(_)>>1)
#define putnum(_) (((ob)(_)<<1)|1)

#define nil putnum(0)
#define T putnum(-1)

#define Avail (v->sp-v->hp)
#define mm(r) ((v->safe=&((struct ll){(ob*)(r),v->safe})))
#define um (v->safe=v->safe->next)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)

#define F(_) ((mo)(_)+1)
#define G(_) (*(mo)(_))

#define A(o) ((two)(o))->a
#define B(o) ((two)(o))->b

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

static Inline void *bump(li v, size_t n) {
  void *x = v->hp;
  return v->hp += n, x; }

ob pop1(li), push1(li, ob);

static Inline void *cells(li v, size_t n) {
  return Avail < n && !please(v, n) ? 0 : bump(v, n); }

static Inline struct tag *mo_tag(mo k) {
  for (;; k++) if (!G(k)) return (struct tag*) k; }

static Inline bool nilp(ob _) { return _ == nil; }
static Inline bool nump(ob _) { return _ & 1; }
static Inline bool homp(ob _) { return !nump(_); }

static Inline bool datp(mo h) { return G(h) == act; }
#define gettyp(x) (((ob*)((x)))[1])
static Inline bool hstrp(mo h) { return datp(h) && gettyp(h) == Str; }
static Inline bool htwop(mo h) { return datp(h) && gettyp(h) == Two; }
static Inline bool strp(ob _) { return homp(_) && hstrp((mo) _); }
static Inline bool twop(ob _) { return homp(_) && htwop((mo) _); }

static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(ob), r = b % sizeof(ob);
  return r ? q + 1 : q; }

// this can give a false positive if x is a fixnum
static Inline bool livep(li v, ob x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static Inline mo mo_ini(void *_, size_t len) {
  struct tag *t = (struct tag*) ((mo) _ + len);
  return t->null = NULL, t->head = _; }

static Inline two two_ini(void *_, ob a, ob b) {
  two w = _; return w->act = act, w->typ = Two,
                    w->a = a, w->b = b, w; }

static Inline str str_ini(void *_, size_t len) {
  str s = _; return s->act = act, s->typ = Str,
                    s->len = len, s; }
#endif
