#include "lisa.h"
#include <stdlib.h>

// thanks !!

typedef la_ob ob;
typedef struct mo *mo; // procedures
typedef struct fr { ob clos, retp, subd, argc, argv[]; } *fr; // stack frame
#define Vm(n, ...) ob n(la v, ob xp, mo ip, ob *hp, ob *sp, fr fp)
typedef Vm(vm);
#define Gc(n) ob n(la v, ob x, ob *pool0, ob *top0)
Gc(cp);

struct mo { vm *ll; };

// static method table for built-in types
typedef const struct mtbl {
  vm *does;
  int (*emit)(la, FILE*, ob);
  ob (*copy)(la, ob, ob*, ob*);
  size_t (*hash)(la, ob);
} *mtbl;

// pairs
typedef struct two { vm *disp; mtbl mtbl; ob a, b; } *two;

// strings
// TODO maybe pre-hash strings for faster lookup & comparison
typedef struct str { vm *disp; mtbl mtbl; size_t len; char text[]; } *str;

// symbols
// FIXME this is a silly way to store internal symbols
// - it's slower than a hash table
// - anonymous symbols waste 2 words
typedef struct sym { vm *disp; mtbl mtbl; ob nom, code, l, r; } *sym;

// hash tables
typedef struct tbl { vm *disp; mtbl mtbl; ob *tab; size_t len, cap; } *tbl;

// grammar symbols
enum lex { Def, Cond, Lamb, Quote, Seq, Splat, Eval, LexN };

// linked list for gc protection
typedef struct keep { ob *it; struct keep *et; } *keep;

struct la {
  // vm state -- kept in CPU registers most of the time
  mo ip; // current thread
  fr fp; // top of control stack
  ob xp, // free register
     *hp, // top of heap
     *sp; // top of stack, free space = sp - hp

  // memory state
  keep safe; // list of C stack addresses to copy on gc
  size_t t0, // gc timestamp, governs len
         len; // size of pool
  ob *pool, // memory pool
     topl, // global namespace
     syms, // internal symbols
     lex[LexN]; // grammar symbols
  intptr_t rand;
  ob (*panic)(struct la*); };

la la_ini(void);
void la_fin(la);

// pairs
ob pair(la, ob, ob);
size_t llen(ob);

// hash tables
size_t hash(la, ob), hashb(const char*, size_t);
ob table(la),
   tbl_set(la, ob, ob, ob),
   tbl_get(la, ob, ob);

// strings & symbols
ob string(la, const char*),
   intern(la, ob),
   interns(la, const char*);

// functions
mo mkmo(la, size_t), // allocator
   ana_p(la, const char*, ob),
   button(mo); // get tag at end
ob hnom(la, ob); // try to get function name FIXME don't expose

#define Push(...) pushs(v, __VA_ARGS__, (ob) 0)
bool
  primp(ob), // is it a primitive function?
  pushs(la, ...), // push onto stack
  please(la, size_t), // gc interface
  eql(la, ob, ob); // logical equality

#define rx(...) la_rx_f(__VA_ARGS__)
int tx(la, FILE*, ob); // write sexp

// internal libc substitutes
intptr_t lcprng(intptr_t);
void setw(void*, uintptr_t, size_t),
     cpyw(void*, const void*, size_t),
     cpy8(void*, const void*, size_t);
char cmin(char);
size_t slen(const char*);
int scmp(const char*, const char*);

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
void errp(la, const char*, ...); // print an error
ob nope(la, const char*, ...) NoInline; // panic with error msg

#define nil putnum(0)
#define F(_) ((mo)(_)+1)
#define G(_) ((mo)(_))->ll
#define FF(x) F(F(x))
#define GF(x) G(F(x))
#define A(o) ((two)(o))->a
#define B(o) ((two)(o))->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define Avail (v->sp - v->hp)
#define mm(r) ((v->safe = &((struct keep){(r), v->safe})))
#define um (v->safe = v->safe->et)
#define with(y,...) (mm(&(y)), (__VA_ARGS__), um)
#define Width(t) b2w(sizeof(struct t))

#define T putnum(-1)


#define getnum(_) ((ob)(_)>>1)
#define putnum(_) (((ob)(_)<<1)|1)

struct prim { vm *go; const char *nom; };
extern const uint64_t mix;
extern const struct prim primitives[];
extern const struct mtbl s_mtbl_two, s_mtbl_str, s_mtbl_tbl, s_mtbl_sym;

#define mtbl_str (&s_mtbl_str)
#define mtbl_two (&s_mtbl_two)
#define mtbl_sym (&s_mtbl_sym)
#define mtbl_tbl (&s_mtbl_tbl)

static Inline bool nilp(ob _) { return _ == nil; }
static Inline bool nump(ob _) { return _ & 1; }
static Inline bool homp(ob _) { return !nump(_); }
static Inline bool tblp(ob _) { return homp(_) && GF(_) == (vm*) mtbl_tbl; }
static Inline bool strp(ob _) { return homp(_) && GF(_) == (vm*) mtbl_str; }
static Inline bool twop(ob _) { return homp(_) && GF(_) == (vm*) mtbl_two; }
static Inline bool symp(ob _) { return homp(_) && GF(_) == (vm*) mtbl_sym; }

static Inline size_t b2w(size_t b) {
  size_t quot = b / sizeof(ob), rem = b % sizeof(ob);
  return rem ? quot + 1 : quot; }

// this can give a false positive if x is a fixnum
static Inline bool livep(la v, ob x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

// unchecked allocator -- make sure there's enough memory!
static Inline void *bump(la v, size_t n) {
  void *x = v->hp;
  v->hp += n;
  return x; }

static Inline void *cells(la v, size_t n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

_Static_assert(-1 == -1 >> 1, "signed >>");
_Static_assert(sizeof(void*) == sizeof(size_t), "size_t matches pointer size");


static Inline size_t ror(size_t x, size_t n) {
  return (x<<((8*sizeof(size_t))-n))|(x>>n); }
