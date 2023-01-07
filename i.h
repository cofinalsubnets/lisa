#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include "li.h"

// thanks !!
//
typedef struct V *la, *li;
typedef intptr_t I, ob;
typedef uintptr_t U;
typedef struct mo *mo; // procedures
                       //
typedef struct frame { // stack frame
  ob *clos; // closure pointer FIXME // use stack
  mo retp; // thread return address
  struct frame *subd; // stack frame of caller
  U argc; // argument count
  ob argv[]; } *sf, *frame;;

// interpreter type
typedef enum status vm(li, ob, mo, ob*, ob*, frame);

struct mo { vm *ap; };
struct tag { struct mo *null, *head, end[]; };

typedef const struct typ {
  vm *actn;
  bool (*equi)(li, I, I);
  intptr_t (*hash)(li, I);
  ob  (*evac)(li, I, I*, I*);
  //  void (*walk)(li, ob, ob*, ob*);
  void (*emit)(li, FILE*, I); } *typ;

typedef struct two {
  vm *act; typ typ;
  ob a, b; } *two;
typedef struct str {
  vm *act; typ typ;
  U len; char text[]; } *str;
typedef struct tbl { // hash tables
  vm *act; typ typ;
  U len, cap;
  struct tbl_e {
    ob key, val;
    struct tbl_e *next; } **tab; } *tbl;
typedef struct sym {
  vm *act; typ typ;
  str nom;
  intptr_t code;
  // symbols are interned into a binary search tree.
  // anonymous symbols (nom == 0) don't have branches.
  struct sym *l, *r; } *sym;

struct V {
  mo ip; frame fp;
  ob xp, *hp, *sp;

  // global variables & state
  tbl topl, macros; // global scope
  struct glob {
    sym define, cond, lambda, quote,
        begin, splat, eval; } lex;
  sym syms; // symbol table
  size_t rand;

  enum status
    status,
    (*exit)(li, enum status);

  // memory manager state
  U len;
  ob *pool;
  struct ll { ob *addr; struct ll *next; } *safe;
  union { ob *cp; size_t t0; }; };

size_t llen(ob);
intptr_t hash(li, ob);

enum status li_go(li);

mo mo_ini(void *, size_t),
   thd(li, ...),
   seq0(li, mo, mo),
   mo_n(li, size_t);
tbl mktbl(li),
    tbl_set(li, tbl, ob, ob);
two two_ini(void*, ob, ob),
    pair(li, ob, ob);
str str_ini(void*, size_t);
sym ini_anon(void*, uintptr_t),
    intern(li, sym*, str),
    symof(li, str);
ob hnom(li, mo),
   cp(li, ob, ob*, ob*), // copy something; used by type-specific copying functions
   *fresh_pool(size_t),
   tbl_get(li, tbl, ob, ob);

vm act;

enum status
  li_call(li, mo, size_t),
  receives(li, FILE*),
  load_file(li, FILE*),
  receive(li, FILE*);

void
  *cells(li, size_t),
  transmit(li, FILE*, ob),
  report(li, enum status);

bool 
  please(li, size_t),
  pushs(li, ...), // push args onto stack; true on success
  eql(li, ob, ob), // object equality
  neql(li, ob, ob); // always returns false

extern const struct typ
  two_typ, str_typ,
  tbl_typ, sym_typ;
#define Width(_) b2w(sizeof(_))

#define getnum(_) ((ob)(_)>>1)
#define putnum(_) (((ob)(_)<<1)|1)

#define nil putnum(0)
#define T putnum(-1)

#define Avail (v->sp - v->hp)
#define mm(r) ((v->safe = &((struct ll){(ob*)(r), v->safe})))
#define um (v->safe = v->safe->next)
#define with(y,...) (mm(&(y)), (__VA_ARGS__), um)

#define F(_) ((mo)(_)+1)
#define G(_) ((mo)(_))->ap
#define FF(x) F(F(x))
#define GF(x) G(F(x))

#define A(o) ((two)(o))->a
#define B(o) ((two)(o))->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))

#define Gc(n) static ob n(li v, ob x, ob *pool0, ob *top0)
#define mix ((intptr_t) 2708237354241864315)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

static Inline void *cpyw_r2l(void *dst, const void *src, size_t n) {
  while (n--) ((U*)dst)[n] = ((U*)src)[n];
  return dst; }

static Inline void *cpyw_l2r(void *dst, const void *src, size_t n) {
  for (size_t i = 0; i < n; i++) ((U*)dst)[i] = ((U*)src)[i];
  return dst; }

static Inline void *setw(void *d, intptr_t w, size_t n) {
  while (n) ((intptr_t*)d)[--n] = w;
  return d; }

static Inline struct tag *mo_tag(mo k) {
  for (;; k++) if (!G(k)) return (struct tag*) k; }

static Inline bool nilp(ob _) { return _ == nil; }
static Inline bool nump(ob _) { return _ & 1; }
static Inline bool homp(ob _) { return !nump(_); }

static Inline bool tblp(ob _) { return
  homp(_) && (typ) GF(_) == &tbl_typ; }
static Inline bool strp(ob _) { return
  homp(_) && (typ) GF(_) == &str_typ; }
static Inline bool twop(ob _) { return
  homp(_) && (typ) GF(_) == &two_typ; }
static Inline bool symp(ob _) { return
  homp(_) && (typ) GF(_) == &sym_typ; }

static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(ob), r = b % sizeof(ob);
  return r ? q + 1 : q; }

// this can give a false positive if x is a fixnum
static Inline bool livep(la v, ob x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static Inline intptr_t ror(intptr_t x, uintptr_t n) {
  return (x << ((8 * sizeof(intptr_t)) - n)) | (x >> n); }

static Inline void fputsn(const char *s, U n, FILE *o) {
  while (n--) putc(*s++, o); }

static Inline intptr_t lcprng(intptr_t s) {
  // this comes from a paper
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }

// unchecked allocator -- make sure there's enough memory!
static Inline void *bump(la v, size_t n) {
  void *x = v->hp;
  return v->hp += n, x; }
