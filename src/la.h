#include "lisa.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __STDC_HOSTED__

#include <stdlib.h>
#define la_malloc malloc
#define la_free free

#include <stdio.h>
typedef FILE *la_io;
#define la_stdin stdin
#define la_stdout stdout
#define la_stderr stderr
#define la_puts fputs
#define la_putc fputc
#define la_getc fgetc
#define la_ungetc ungetc

#include <time.h>
typedef clock_t la_clock_t;
#define la_clock clock

#else

typedef uintptr_t la_io, la_clock_t;
la_clock_t la_clock(void);
void
  *la_calloc(size_t, size_t),
  la_free(void*),
  la_putc(int, la_io),
  la_getc(la_io),
  la_puts(const char*, la_io),
  la_ungetc(int, la_io);

#endif

// thanks !!
typedef intptr_t ob, la_ob;
typedef struct la_carrier *la;
typedef struct la_fn *la_fn, *la_mo, *fn, *mo; // procedures

typedef struct sf { // stack frame
  ob *clos; // closure pointer FIXME
  // keep this on the stack outside the
  // frame so we don't waste space for
  // functions w/o closures.
  mo retp; // thread return address
  struct sf *subd; // stack frame of caller
  size_t argc; // argument count
  ob argv[]; } *sf;

typedef enum la_status vm(la, ob, mo, ob*, ob*, sf); // interpreter function type
struct la_fn { vm *ap; };
// every dynamically allocated thread ends
// with a footer holding a pointer to its head
typedef struct tl {
  void *null; // it's always 0
  struct la_fn *head, end[];
} *tag, *la_fn_tag;

// static method table for built-in types
typedef const struct mtbl {
  vm *does;
  bool (*equi)(la, ob, ob);
  intptr_t (*hash)(la, ob);
  void (*emit)(la, la_io, ob);
  ob (*evac)(la, ob, ob*, ob*);
//  void (*walk)(la, ob, ob*, ob*);
} *mtbl;

struct hd { vm *disp; mtbl mtbl; };

typedef struct two { struct hd h; ob a, b; } *two;

typedef struct str { // strings
  struct hd h;
  size_t len;
  char text[]; } *str;

typedef struct sym {
  struct hd h;
  str nom;
  intptr_t code;
  // symbols are interned into a binary search tree.
  // anonymous symbols (nom == 0) don't have branches.
  struct sym *l, *r; } *sym;

typedef struct tbl_e {
  ob key, val;
  struct tbl_e *next; } *tbl_e;

typedef struct tbl { // hash tables
  struct hd h;
  size_t len, cap;
  tbl_e *tab; } *tbl;

// linked list for gc protection
typedef struct keep { void **addr; struct keep *next; } *keep;

struct la_lexicon {
  sym define, cond, lambda, quote, begin, splat, eval; };

struct la_carrier {
  // registers -- in CPU registers when VM is running
  mo ip;
  sf fp;
  ob xp, *hp, *sp;

  // global variables & state
  tbl topl, macros; // global scope
  sym syms; // symbol table // TODO use a hash
  struct la_lexicon lex;
  intptr_t rand;

  // memory manager state
  size_t len;
  ob *pool;
  keep safe;
  // TODO list of finalizers
  union { la_clock_t t0; ob *cp; } run; // TODO copy pointer for cheney's algorithm
};

// FIXME develop towards public API
void
  la_tx(la, la_io, ob), // write a value
  la_reset(la), // reset interpreter state
  la_perror(la, la_status),
  la_putsn(const char*, size_t, la_io);

enum la_status
  la_call(la, la_fn, size_t),
  la_go(la),
  la_ap(la, la_fn, ob),
  la_ev_x(la, la_ob),
  la_ev_fs(la, la_io),
  la_ev_f(la, la_io),
  la_rx(la, la_io);

vm disp; // dispatch instruction for data threads; also used as a sentinel

ob
  tbl_get(la, tbl, ob, ob),
  cp(la, ob, ob*, ob*), // copy something; used by type-specific copying functions
  hnom(la_carrier, la_fn); // get function name FIXME hide this

la_fn_tag motag(la_fn); // get tag at end
mo mkmo(la_carrier, size_t), // allocate a thread
   ini_mo(void *, size_t);
sym symof(la, str);
str ini_str(void *, size_t);
two pair(la, ob, ob), // pair constructor
    ini_two(void *, ob, ob);

tbl mktbl(la), tbl_set(la, tbl, ob, ob);

bool
  please(la, size_t),
  pushs(la, ...), // push args onto stack; true on success
  eql(la, ob, ob), // object equality
  neql(la, ob, ob); // always returns false

size_t llen(ob); // length of list
intptr_t
  hash(la, ob), // hash function for tables
  lcprng(intptr_t); // linear congruential pseudorandom number generator

extern const struct mtbl mtbl_two, mtbl_str, mtbl_tbl, mtbl_sym;

// just a big random number!
#define mix ((int64_t)2708237354241864315)

#define wsizeof(_) b2w(sizeof(_))

#define getnum(_) ((ob)(_)>>1)
#define putnum(_) (((ob)(_)<<1)|1)

#define nil putnum(0)
#define T putnum(-1)

#define Avail (v->sp - v->hp)
#define mm(r) \
  ((v->safe = &((struct keep){(void**)(r), v->safe})))
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
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static Inline intptr_t ror(intptr_t x, size_t n) {
  return (x<<((8*sizeof(intptr_t))-n))|(x>>n); }

_Static_assert(-1 == -1 >> 1, "signed >>");
_Static_assert(sizeof(size_t) == sizeof(void*),
  "size_t size == data pointer size");
_Static_assert(sizeof(vm*) == sizeof(void*),
  "function pointer size == data pointer size");

// these are vm functions used by C but not lisp.
#define cfns(_) _(gc) _(xdom) _(xoom) _(xary)
#define ninl(x, ...) vm x NoInline;
cfns(ninl)
#undef cfns

// used by the compiler but not exposed as primitives
#define i_internals(_)\
 _(call) _(ret) _(rec) _(jump) _(varg)\
 _(arity) _(ary1) _(ary2) _(ary3) _(ary4)\
 _(idno) _(idmo) _(idtwo) _(idtbl)\
 _(imm) _(immn1) _(imm0) _(imm1)\
 _(immn1p) _(imm0p) _(imm1p)\
 _(argn) _(arg0) _(arg1) _(arg2) _(arg3)\
 _(arg0p) _(arg1p) _(arg2p) _(arg3p)\
 _(clon) _(clo0) _(clo1) _(clo2) _(clo3)\
 _(clo0p) _(clo1p) _(clo2p) _(clo3p)\
 _(sl1n) _(sl10) _(sl11) _(sl12) _(sl13)\
 _(sl10p) _(sl11p) _(sl12p) _(sl13p)\
 _(deftop) _(late)\
 _(setloc) _(defsl1)\
 _(take) _(encl1) _(encl0)\
 _(twop_) _(nump_) _(nilp_) _(strp_)\
 _(tblp_) _(symp_) _(homp_)\
 _(add) _(sub) _(mul) _(quot) _(rem) _(neg)\
 _(sar) _(sal) _(band) _(bor) _(bxor)\
 _(lt) _(lteq) _(eq) _(gteq) _(gt)\
 _(tget) _(tset) _(thas) _(tlen)\
 _(cons) _(car) _(cdr)\
 _(br1) _(br0) _(bre) _(brn)\
 _(brl) _(brle) _(brge) _(brg)\
 _(push)

i_internals(ninl)

// primitive functions
#define i_primitives(_) _(ev_f, "ev") _(ap_f, "ap")\
 _(hom_f, "hom") _(homp_f, "homp")\
 _(poke_f, "poke") _(peek_f, "peek")\
 _(seek_f, "seek") _(hfin_f, "hfin")\
  \
 _(nump_f, "nump") _(rand_f, "rand")\
 _(add_f, "+") _(sub_f, "-") _(mul_f, "*")\
 _(quot_f, "/") _(rem_f, "%")\
 _(sar_f, ">>") _(sal_f, "<<")\
 _(band_f, "&") _(bnot_f, "!") _(bor_f, "|") _(bxor_f, "^")\
  \
 _(twop_f, "twop") _(cons_f, "X") _(car_f, "A") _(cdr_f, "B")\
  \
 _(tbl_f, "tbl") _(tblp_f, "tblp") _(tlen_f, "tlen")\
 _(tget_f, "tget") _(thas_f, "thas") _(tset_f, "tset")\
 _(tdel_f, "tdel") _(tkeys_f, "tkeys")\
  \
 _(str_f, "str") _(strp_f, "strp") _(slen_f, "slen")\
 _(ssub_f, "ssub") _(scat_f, "scat") _(sget_f, "schr")\
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

#define Vm(n) enum la_status n(la v, ob xp, mo ip, ob *hp, ob *sp, sf fp)
#define Gc(n) ob n(la v, ob x, ob *pool0, ob *top0)

// unchecked allocator -- make sure there's enough memory!
static Inline void *bump(la v, size_t n) {
  void *x = v->hp;
  return v->hp += n, x; }

static Inline void *cells(la v, size_t n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

static Inline void *cpyw_r2l(void *dst, const void *src, size_t n) {
  while (n--) ((void**)dst)[n] = ((void**)src)[n];
  return dst; }

static Inline void *cpyw_l2r(void *dst, const void *src, size_t n) {
  for (size_t i = 0; i < n; i++)
    ((void**)dst)[i] = ((void**)src)[i];
  return dst; }

static Inline void *setw(void *dst, intptr_t w, size_t n) {
  while (n--) ((intptr_t*)dst)[n] = w;
  return dst; }
