#ifndef _la_la_h
#define _la_la_h

#include "lisa.h"
#include "status.h"
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

// thanks !!
typedef struct la_carrier *la_carrier;
typedef intptr_t la_ob, ob;
typedef la_carrier la;
typedef struct mo *mo, *la_mo; // procedures
typedef struct sf *sf; // stack frame
typedef enum la_status vm(la, ob, mo, ob*, ob*, sf); // interpreter function type

// struct needed for type indirection
// around vm function pointer arrays
struct mo { vm *ap; };

// static method table for built-in types
typedef const struct mtbl {
  vm *does;
  bool (*equi)(la, ob, ob);
  intptr_t (*hash)(la, ob);
  long (*emit)(la, FILE*, ob);
  ob (*evac)(la, ob, ob*, ob*);
//  void (*walk)(la, ob, ob*, ob*);
} *mtbl;

vm disp; // special sentinel instruction for data threads
typedef struct header {
  vm *disp; // pointer to disp function
  mtbl mtbl;
} *header;


typedef struct two *two; // pairs
typedef struct str *str; // strings
typedef struct sym *sym; // symbols
typedef struct tbl *tbl; // hash tables

// grammar symbols
enum lex { Def, Cond, Lamb, Quote, Seq, Splat, Eval, LexN };

typedef struct keep *keep;

struct la_carrier {
  // vm state
  mo ip;
  sf fp;
  ob xp, *hp, *sp;

  tbl topl; // global scope
  sym syms, // symbol table // TODO use a hash
      lex[LexN]; // lexicon
  intptr_t rand;

  // gc state
  size_t len;
  ob *pool;
  keep safe;
  // TODO list of finalizers
  union {
    clock_t t0;
    ob *cp; // TODO copy pointer for cheney's algorithm
  } run;
};

// FIXME develop towards public API
enum la_status
  la_lib(la_carrier, const char*),
  la_call(la_carrier, la_mo, size_t),
  la_ev_f(la_carrier, FILE*);

void la_reset(la); // reset interpreter state

struct prim { vm *ap; const char *nom; };
extern const int64_t mix;
extern const struct prim prims[];
extern const struct mtbl
  mtbl_two, mtbl_str, mtbl_tbl, mtbl_sym;

#define getnum(_) ((ob)(_)>>1)
#define putnum(_) (((ob)(_)<<1)|1)

#define nil putnum(0)
#define T putnum(-1)

#define F(_) ((mo)(_)+1)
#define G(_) ((mo)(_))->ap
#define FF(x) F(F(x))
#define GF(x) G(F(x))

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

static Inline bool nilp(ob _) { return _ == nil; }
static Inline bool nump(ob _) { return _ & 1; }
static Inline bool homp(ob _) { return !nump(_); }
static Inline bool tblp(ob _) {
  return homp(_) && GF(_) == (vm*) &mtbl_tbl; }
static Inline bool strp(ob _) {
  return homp(_) && GF(_) == (vm*) &mtbl_str; }
static Inline bool twop(ob _) {
  return homp(_) && GF(_) == (vm*) &mtbl_two; }
static Inline bool symp(ob _) {
  return homp(_) && GF(_) == (vm*) &mtbl_sym; }

static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(ob), r = b % sizeof(ob);
  return r ? q + 1 : q; }

// this can give a false positive if x is a fixnum
static Inline bool livep(la v, ob x) {
  return (ob*) x >= v->pool &&
    (ob*) x < v->pool + v->len; }

_Static_assert(-1 == -1 >> 1, "signed >>");
_Static_assert(sizeof(size_t) == sizeof(void*),
  "size_t size == data pointer size");
_Static_assert(sizeof(vm*) == sizeof(void*),
  "function pointer size == data pointer size");

#endif
