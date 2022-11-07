#include "lisa.h"
#include <stdlib.h>
#include <time.h>

// thanks !!

typedef la_point ob;
typedef la_carrier la;
typedef struct mo *mo; // procedures
typedef struct sf *sf; // stack frame
typedef ob vm(la, ob, mo, ob*, ob*, sf); // interpreter function type

// struct needed for type indirection
// around vm function pointer arrays
struct mo { vm *ap; };

// every dynamically allocated thread ends
// with a footer holding a pointer to its head
typedef struct tag {
  void *null; // always null
  struct mo
    *head, // pointer to head of thread
    end[]; // first address after thread
} *tag;

// static method table for built-in types
typedef const struct mtbl {
  vm *does;
  bool (*equi)(la, ob, ob);
  intptr_t (*hash)(la, ob);
  long (*emit)(la, FILE*, ob);
  ob (*evac)(la, ob, ob*, ob*);
//  void (*walk)(la, ob, ob*, ob*);
} *mtbl;

typedef struct header {
  vm *disp; // pointer to disp function
  mtbl mtbl;
} *header;


// stack frame
struct sf {
  ob *clos; // closure pointer FIXME
  // keep this on the stack outside the
  // frame so we don't waste space for
  // functions w/o closures.
  mo retp; // thread return address
  sf subd; // stack frame of caller
  size_t argc; // argument count
  ob argv[]; };


// pairs
typedef struct two {
  struct header head;
  ob a, b;
} *two;

// strings
typedef struct str {
  struct header head;
  size_t len;
  char text[];
} *str;

// symbols
// FIXME this is a silly way to store internal symbols
// - it's slower than a hash table
// - anonymous symbols waste 2 words
typedef struct sym {
  struct header head;
  str nom;
  intptr_t code;
  struct sym *l, *r;
} *sym;

// hash tables
typedef struct tbl {
  struct header head;
  size_t len, cap;
  ob *tab;
} *tbl;

// grammar symbols
enum lex {
  Def, Cond, Lamb, Quote, Seq, Splat, Eval, LexN };

// linked list for gc protection
typedef struct keep {
  void **addr;
  struct keep *next;
} *keep;

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
la_status la_lib(la_carrier, const char*);
la_point
  la_ev(la_carrier, la_point), // eval a value
  la_rx(la_carrier, FILE*); // read a value
long la_tx(la_carrier, FILE*, la_point); // write a value


// FIXME remove or hide these
ob hnom(la, mo);
ob cp(la, ob, ob*, ob*); // copy something; used by type-specific copying functions
bool primp(mo); // is it a primitive function?

bool please(la, size_t); // ask GC for available memory
void *bump(la, size_t), // allocate memory unchecked
     *cells(la, size_t), // allocate memory checked
     // word-size mem{set,cpy}
     *setw(void*, intptr_t, size_t),
     *cpyw(void*, const void*, size_t);

// pairs
two pair(la, ob, ob);

// namespace functions
ob nstbl(la), nsget(la, ob);
bool nsset(la, ob, ob);

// hash tables
intptr_t hash(la, ob);
tbl mktbl(la),
    tblset(la, tbl, ob, ob);
ob tblget(la, tbl, ob);

// string & symbol constructors
sym symof(la, str);
str strof(la, const char*);

// output functions:
// like la_tx, they return the number
// of bytes written or a negative number
// on error.
long fputstr(FILE*, str);  // like fputs

// functions
mo mkmo(la, size_t); // allocate a thread
tag motag(mo); // get tag at end

bool pushs(la, ...); // push args onto stack
ob tupl(la, ...); // collect args into tuple (data thread)

bool
  eql(la, ob, ob), // object equality
  neql(la, ob, ob); // always returns false

// linear congruential pseudorandom number generator
intptr_t lcprng(intptr_t);

void
  la_reset(la), // reset interpreter state
  errp(la, const char*, ...); // print an error with backtrace

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

#define A(o) ((two)(o))->a
#define B(o) ((two)(o))->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))

#define Avail (v->sp - v->hp)
#define mm(r) \
  ((v->safe = &((struct keep){(void**)(r), v->safe})))
#define um (v->safe = v->safe->next)
#define with(y,...) (mm(&(y)), (__VA_ARGS__), um)

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

#define Width(t) b2w(sizeof(struct t))

// this can give a false positive if x is a fixnum
static Inline bool livep(la v, ob x) {
  return (ob*) x >= v->pool &&
    (ob*) x < v->pool + v->len; }

_Static_assert(-1 == -1 >> 1, "signed >>");
_Static_assert(sizeof(size_t) == sizeof(void*),
  "size_t size == data pointer size");
_Static_assert(sizeof(vm*) == sizeof(void*),
  "function pointer size == data pointer size");

static Inline size_t ror(size_t x, size_t n) {
  return (x<<((8*sizeof(size_t))-n))|(x>>n); }


// these are vm functions used by C but not lisp.
#define cfns(_) _(gc) _(xdom) _(xoom) _(xnom) _(xary)
#define ninl(x, ...) vm x NoInline;
cfns(ninl)
#undef cfns

// used by the compiler but not exposed as primitives
#define i_internals(_)\
 _(call) _(ret) _(rec) _(jump) _(varg) _(disp)\
 _(arity) _(idno) _(idmo) _(idtwo) _(idtbl)\
 _(imm) _(immn1) _(imm0) _(imm1) _(imm2)\
 _(argn) _(arg0) _(arg1) _(arg2) _(arg3)\
 _(clon) _(clo0) _(clo1) _(clo2) _(clo3)\
 _(locn) _(loc0) _(loc1) _(loc2) _(loc3)\
 _(deftop) _(late)\
 _(setloc) _(defloc)\
 _(take) _(encl1) _(encl0)\
 _(twop_) _(nump_) _(nilp_) _(strp_)\
 _(tblp_) _(symp_) _(homp_)\
 _(add) _(sub) _(mul) _(quot) _(rem) _(neg)\
 _(sar) _(sal) _(band) _(bor) _(bxor)\
 _(lt) _(lteq) _(eq) _(gteq) _(gt)\
 _(tget) _(tset) _(thas) _(tlen)\
 _(cons) _(car) _(cdr)\
 _(poke)\
 _(br1) _(br0) _(bre) _(brn)\
 _(brl) _(brle) _(brge) _(brl2)\
 _(brle2) _(brg2) _(brg)\
 _(push) _(dupl)\

i_internals(ninl)

// primitive functions
// FIXME due to a hack ev must be the first item in this list
#define i_primitives(_)\
 _(ev_f, "ev") _(ap_f, "ap")\
  \
 _(nump_f, "nump") _(rand_f, "rand")\
 _(add_f, "+") _(sub_f, "-") _(mul_f, "*")\
 _(quot_f, "/") _(rem_f, "%")\
 _(sar_f, ">>") _(sal_f, "<<")\
 _(band_f, "&") _(bnot_f, "!") _(bor_f, "|") _(bxor_f, "^")\
  \
 _(twop_f, "twop") _(cons_f, "X") _(car_f, "A") _(cdr_f, "B")\
  \
 _(hom_f, "hom") _(homp_f, "homp")\
 _(poke_f, "poke") _(peekx_f, "peek")\
 _(seek_f, "seek") _(hfin_f, "hfin")\
  \
 _(tbl_f, "tbl") _(tblp_f, "tblp") _(tlen_f, "tlen")\
 _(tget_f, "tget") _(thas_f, "thas") _(tset_f, "tset")\
 _(tdel_f, "tdel") _(tkeys_f, "tkeys")\
  \
 _(str_f, "str") _(strp_f, "strp") _(slen_f, "slen")\
 _(ssub_f, "ssub") _(scat_f, "scat")\
 _(sget_f, "schr")\
  \
 _(sym_f, "sym") _(symp_f, "symp") _(ynom_f, "ynom")\
  \
 _(tx_f, ".") _(txc_f, "putc") _(rxc_f, "getc")\
  \
 _(eq_f, "=") _(lt_f, "<") _(lteq_f, "<=")\
 _(gteq_f, ">=") _(gt_f, ">") _(nilp_f, "nilp")\
  \
 _(xdom, "nope")

i_primitives(ninl)
#undef ninl

// " the interpreter "
// the arguments to a terp function collectively represent the
// runtime state, and the  return value is the result of the
// program. there are six arguments because that's the number
// that the prevalent unix calling convention on AMD64 (System
// V ABI) says should be passed in registers; that's the only
// reason why there aren't more. but it's not too bad; the six
// arguments are:
// - v  : vm instance pointer ; most functions take this as the first argument
// - ip : instruction pointer ; the current vm instruction ; function pointer pointer
// - fp : frame pointer       ; current function context
// - sp : stack pointer       ; data/call stack
// - hp : heap pointer        ; the next free heap location
// - xp : return value        ; general-purpose register

// when the interpreter isn't running, the state variables that
// would normally be in registers are stored in slots on the
// vm structure. phowever while the interpreter is running it
// uses these struct slots to pass and return extra values
// without using the stack. so the interpreter has to restore
// the current values in the vm struct before it makes any
// "external" function calls.
#define Pack() (v->ip=ip,v->sp=sp,v->hp=hp,v->fp=fp,v->xp=xp)
#define Unpack() (fp=v->fp,hp=v->hp,sp=v->sp,ip=v->ip,xp=v->xp)
#define CallOut(...) (Pack(), __VA_ARGS__, Unpack())

// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer. if a function has
// locals, this will have been initialized before they are
// referenced.

#define ApN(n, x) (xp = (x), ip += (n), ApC(G(ip), xp))
#define ApC(f, x) (f)(v, (x), ip, hp, sp, fp)
#define ApY(f, x) (ip = (mo) (f), ApC(G(ip), (x)))

#define ArityCheck(n) if (n > fp->argc) return ApC(xary, putnum(n))
#define Check(_) if (!(_)) return ApC(xdom, xp)
#define Have(n) if (sp - hp < n) return (v->xp = n, ApC(gc, xp))
// sp is at least hp so this is a safe check for 1 word
#define Have1() if (sp == hp) return (v->xp = 1, ApC(gc, xp))

static Inline two ini_two(void *_, ob a, ob b) {
  two w = _;
  w->head.disp = disp;
  w->head.mtbl = &mtbl_two;
  w->a = a, w->b = b;
  return w; }

static Inline str ini_str(void *_, size_t len) {
  str s = _;
  s->head.disp = disp;
  s->head.mtbl = &mtbl_str;
  s->len = len;
  return s; }

static Inline mo ini_mo(void *_, size_t len) {
  mo k = _;
  tag t = (tag) (k + len);
  t->null = NULL, t->head = k;
  return k; }

#define Gc(n) ob n(la v, ob x, ob *pool0, ob *top0)
#define Vm(n) ob n(la v, ob xp, mo ip, ob *hp, ob *sp, sf fp)
