#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include "li.h"
typedef struct V *la;

// thanks !!
typedef intptr_t I;
typedef uintptr_t U;
typedef I ob;
typedef struct mo *mo; // procedures
typedef struct frame *sf, *frame;

typedef enum status vm(la, ob, mo, ob*, ob*, frame); // interpreter function type
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

struct mo { vm *ap; };
struct tag { struct mo *null, *head, end[]; };

typedef struct two *two;
typedef struct str *str;
typedef struct tbl *tbl;
typedef struct sym *sym;

struct V {
  mo ip; frame fp;
  ob xp, *hp, *sp;

  // global variables & state
  tbl topl, macros; // global scope
  sym syms; // symbol table
  struct glob {
    sym define, cond, lambda, quote,
        begin, splat, eval; } lex;
  size_t rand;

  enum status (*exit)(la, enum status);

  // memory manager state
  U len;
  ob *pool;
  struct ll { ob *addr; struct ll *next; } *safe;
  union { ob *cp; size_t t0; }; };

struct frame { // stack frame
  ob *clos; // closure pointer FIXME // use stack
  mo retp; // thread return address
  struct frame *subd; // stack frame of caller
  U argc; // argument count
  ob argv[]; };

struct typ {
  vm *actn;
  bool (*equi)(la, I, I);
  intptr_t  (*hash)(la, I);
  void (*emit)(la, FILE*, I);
  //  void (*walk)(la, ob, ob*, ob*);
  ob  (*evac)(la, I, I*, I*); };

typedef const struct typ *typ;
struct str { vm *act; typ typ; U len; char text[]; };
struct sym {
  vm *act; typ typ;
  str nom;
  I code;
  // symbols are interned into a binary search tree.
  // anonymous symbols (nom == 0) don't have branches.
  struct sym *l, *r; };


#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

mo thd(la, ...),
   mo_ana(la, ob),
   mo_n(la, size_t)
   ;
enum status
  la_ev_x(la, ob) NoInline,
  receive(la, FILE*);
void transmit(la, FILE*, ob),
     report(la, enum status);
bool 
  please(la, U),
  pushs(la, ...), // push args onto stack; true on success
  eql(la, ob, ob), // object equality
  neql(la, ob, ob); // always returns false
ob tbl_get(la, tbl, ob, ob);
two pair(la, ob, ob);

extern const struct typ two_typ, str_typ, tbl_typ, sym_typ;
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
  size_t q = b / sizeof(ob),
         r = b % sizeof(ob);
  return r ? q + 1 : q; }

// this can give a false positive if x is a fixnum
static Inline bool livep(la v, ob x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static Inline intptr_t ror(intptr_t x, uintptr_t n) {
  return (x << ((8 * sizeof(intptr_t)) - n)) | (x >> n); }

struct two { vm *act; typ typ; ob a, b; };
size_t llen(ob);
struct tbl { // hash tables
  vm *act; typ typ;
  U len, cap;
  struct tbl_e {
    I key, val;
    struct tbl_e *next; } **tab; };
mo mo_ini(void *, size_t);
tbl mktbl(la), tbl_set(la, tbl, ob, ob), tbl_ini(void*, size_t, size_t, struct tbl_e**);
two two_ini(void*, ob, ob);
str str_ini(void*, size_t);
sym ini_anon(void*, uintptr_t);

void *cells(la, size_t), *bump(la, size_t);
static Inline void fputsn(const char *s, U n, FILE *o) {
  while (n--) putc(*s++, o); }

static Inline intptr_t lcprng(intptr_t s) {
  // this comes from a paper
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }

static Inline size_t tbl_load(tbl t) {
  return t->len / t->cap; }
static Inline size_t tbl_idx(U cap, U co) {
  return co & (cap - 1); }
intptr_t hash(la, ob);
sym intern(la, sym*, str);
void transmit(la, FILE*, ob);
vm act;
ob hnom(la, mo);
sym symof(struct V*, str);
ob cp(la, ob, ob*, ob*), // copy something; used by type-specific copying functions
   *fresh_pool(size_t);
