#include "lisa.h"
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

// thanks !!
typedef void u0;
typedef bool u1;

typedef intptr_t I;
typedef uintptr_t U;
typedef I ob, la_ob;
typedef struct carrier *la, *la_carrier, *F, *A;
typedef struct M *la_fn, *la_mo, *fn, *mo; // procedures

#ifdef __STDC_HOSTED__
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#else
typedef U FILE;
uintptr_t clock(void);
void *malloc(size_t, size_t), free(void*),
  putc(int, FILE*), getc(FILE*), ungetc(int, FILE*),
  fputs(const char*, FILE*);
extern ob stdin, stdout, stderr;
#endif

typedef FILE *la_io;

typedef struct sf { // stack frame
  ob *clos; // closure pointer FIXME
  // keep this on the stack outside the
  // frame so we don't waste space for
  // functions w/o closures.
  mo retp; // thread return address
  struct sf *subd; // stack frame of caller
  size_t argc; // argument count
  ob argv[]; } *sf;

typedef enum status vm(la, ob, mo, ob*, ob*, sf); // interpreter function type
struct M { vm *ap; };
// every dynamically allocated thread ends
// with a footer holding a pointer to its head
typedef struct tl { struct M *null, *head, end[]; } *tag, *la_fn_tag;

// static method table for built-in types
typedef const struct mtbl {
  vm *does;
  u1 (*equi)(la, I, I);
  I (*hash)(la, I);
  u0 (*emit)(la, la_io, I);
  I (*evac)(la, I, I*, I*);
//  void (*walk)(la, ob, ob*, ob*);
} *mtbl;

struct hd {
  vm *disp;
  mtbl mtbl; };

typedef struct two {
  struct hd h;
  ob a, b; } *two;

typedef struct str { // strings
  struct hd h;
  size_t len;
  char text[]; } *str;

typedef struct sym {
  struct hd h;
  str nom;
  I code;
  // symbols are interned into a binary search tree.
  // anonymous symbols (nom == 0) don't have branches.
  struct sym *l, *r; } *sym;

typedef struct tbl_e {
  I key, val;
  struct tbl_e *next; } *tbl_e;

typedef struct tbl { // hash tables
  struct hd h;
  U len, cap;
  tbl_e *tab; } *tbl;

// linked list for gc protection
struct ll {
  ob *addr;
  struct ll *next; };

struct lex {
  sym define, cond, lambda, quote, begin,
      splat, eval; };

struct carrier {
  // registers -- in CPU registers when VM is running
  mo ip; sf fp; ob xp, *hp, *sp;

  // global variables & state
  tbl topl, macros; // global scope
  sym syms; // symbol table
  struct lex lex;
  intptr_t rand;

  // memory manager state
  size_t len;
  ob *pool;
  struct ll *safe;
  union {
    uintptr_t t0;
    ob *cp; // TODO copy pointer for cheney's algorithm
  } run; };

// FIXME develop towards public API
void
  *bump(struct carrier*, size_t),
  *cells(struct carrier*, size_t),
  transmit(struct carrier*, FILE*, ob), // write a value
  la_reset(struct carrier*), // reset interpreter state
  la_perror(struct carrier*, enum status),
  la_putsn(const char*, size_t, FILE*);

enum status
  la_ev_x(la, la_ob),
  receive(la, la_io);

vm disp; // dispatch instruction for data threads; also used as a sentinel

ob
  tbl_get(la, tbl, ob, ob),
  cp(la, ob, ob*, ob*), // copy something; used by type-specific copying functions
  hnom(la_carrier, la_fn); // get function name FIXME hide this

#define motag mo_tl
#define mkmo mo_n
#define ini_mo mo_ini
#define ini_two two_ini
#define ini_str str_ini
mo mo_n(struct carrier*, size_t), // allocate a thread
   mo_ini(void *, size_t);
sym symof(struct carrier*, str);
str str_ini(u0*, size_t);
two pair(la, ob, ob), // pair constructor
    two_ini(u0*, ob, ob);

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

extern const struct mtbl
  mtbl_two, mtbl_str, mtbl_tbl, mtbl_sym;

// just a big random number!
#define mix ((int64_t)2708237354241864315)

#define wsizeof(_) b2w(sizeof(_))

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

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define SI static Inline

SI struct tl *mo_tl(mo k) {
  while (G(k)) k = F(k);
  return (struct tl*) k; }

SI u1 nilp(ob _) { return _ == nil; }
SI u1 nump(ob _) { return _ & 1; }
SI u1 homp(ob _) { return !nump(_); }
SI u1 tblp(ob _) { return homp(_) && GF(_) == (vm*) &mtbl_tbl; }
SI u1 strp(ob _) { return homp(_) && GF(_) == (vm*) &mtbl_str; }
SI u1 twop(ob _) { return homp(_) && GF(_) == (vm*) &mtbl_two; }
SI u1 symp(ob _) { return homp(_) && GF(_) == (vm*) &mtbl_sym; }

SI U b2w(U b) {
  U q = b / sizeof(ob), r = b % sizeof(ob);
  return r ? q + 1 : q; }

// this can give a false positive if x is a fixnum
SI u1 livep(la v, ob x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

SI I ror(I x, U n) {
  return (x<<((8*sizeof(I))-n))|(x>>n); }

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

#define Vm(n) enum status n(la v, ob xp, mo ip, ob *hp, ob *sp, sf fp)
#define Gc(n) ob n(la v, ob x, ob *pool0, ob *top0)


SI u0 *cpyw_r2l(u0 *dst, const u0 *src, U n) {
  while (n--) ((U*)dst)[n] = ((U*)src)[n];
  return dst; }

SI u0 *cpyw_l2r(u0 *dst, const u0 *src, U n) {
  for (U i = 0; i < n; i++) ((U*)dst)[i] = ((U*)src)[i];
  return dst; }

SI u0 *setw(u0 *d, I w, U n) {
  while (n) ((I*)d)[--n] = w;
  return d; }

_Static_assert(-1 == -1 >> 1, "signed >>");
_Static_assert(sizeof(size_t) == sizeof(void*),
  "size_t size == data pointer size");
