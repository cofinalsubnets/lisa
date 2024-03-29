#ifndef _l_i_h
#define _l_i_h
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include "li.h"

_Static_assert(-1 >> 1 == -1, "signed shift");
_Static_assert(sizeof(size_t) == sizeof(void*), "size_t");

// thanks !!
//
typedef struct V *la, *li;
typedef intptr_t ob;
typedef struct mo *mo; // procedures
                       //
typedef struct frame { // stack frame
  ob *clos; // closure pointer FIXME // use stack
  mo retp; // thread return address
  struct frame *subd; // stack frame of caller
  uintptr_t argc; // argument count
  ob argv[]; } *sf, *frame;;

// interpreter type
typedef enum status vm(li, ob, mo, ob*, ob*, frame);

struct mo { vm *ap; };
struct tag { struct mo *null, *head, end[]; };

typedef void emitter(li, FILE*, ob), gc_walk(li, ob, ob*, ob*);
typedef ob gc_evac(li, ob, ob*, ob*);
typedef uintptr_t hasher(li, ob);
typedef bool equator(li, ob, ob);

typedef const struct typ {
  vm *does;
  equator *equi;
  hasher *hash;
  gc_evac *evac;
  gc_walk *walk;
  emitter *emit; } *typ;

typedef struct two {
  vm *act; const struct typ *typ;
  ob a, b; } *two;
typedef struct str {
  vm *act; const struct typ *typ;
  uintptr_t len; char text[]; } *str;
typedef struct tbl { // hash tables
  vm *act; const struct typ *typ;
  uintptr_t len, cap;
  struct tbl_e {
    ob key, val;
    struct tbl_e *next; } **tab; } *tbl;
typedef struct sym {
  vm *act; const struct typ *typ;
  str nom; uintptr_t code;
  // symbols are interned into a binary search tree.
  // anonymous symbols (nom == 0) don't have branches.
  struct sym *l, *r; } *sym;

struct V {
  mo ip; frame fp; ob xp, *hp, *sp;

  // global variables & state
  struct glob {
    tbl topl, macros; // global scope
    sym define, cond, lambda, quote,
        begin, splat, eval; } *lex;
  sym syms; // internal symbols
  uintptr_t rand;

  // memory manager state
  uintptr_t len;
  intptr_t *pool, *loop;
  struct ll { ob *addr; struct ll *next; } *safe;
  union { ob *cp; size_t t0; }; };

vm act, yield_status, gc, xok, setclo, genclo0, genclo1;

void transmit(li, FILE*, ob), // write to output
     report(li, enum status); // show error message

bool please(li, size_t),
     pushs(li, ...); // push args onto stack; true on success

enum status li_go(li), receive(li, FILE*);

uintptr_t llen(ob), hash(li, ob), liprng(li);

vm do_id, do_tbl, do_two;
emitter tx_two, tx_tbl, tx_str, tx_sym;
gc_walk wk_tbl, wk_str, wk_sym, wk_two;
gc_evac cp_str, cp_sym, cp_tbl, cp_two;
hasher hx_two, hx_sym, hx_typ, hx_str;
equator _eql, neql, eq_two, eq_str;

mo thd(li, ...), mo_n(li, size_t);
tbl tbl_new(li), tbl_set(li, tbl, ob, ob);
two pair(li, ob, ob);
str strof(li, const char*);
sym nym(li), symof(li, str), intern(li, sym*, str);
ob hnom(li, mo), *new_pool(size_t),
   cp(li, ob, ob*, ob*), tbl_get(li, tbl, ob, ob);

extern const struct typ two_typ, str_typ, tbl_typ, sym_typ;

#define Gc(n) ob n(li v, ob x, ob *pool0, ob *top0)
#define End ((intptr_t)0)
#define EndArgs End
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

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

static Inline void *bump(li v, size_t n) {
  void *x = v->hp;
  return v->hp += n, x; }

static Inline void *cells(li v, size_t n) {
  return Avail < n && !please(v, n) ? 0 : bump(v, n); }

static Inline void *cpyw_r2l(void *dst, const void *src, size_t n) {
  while (n--) ((intptr_t*)dst)[n] = ((intptr_t*)src)[n];
  return dst; }

static Inline void *cpyw_l2r(void *dst, const void *src, size_t n) {
  for (size_t i = 0; i < n; i++) ((intptr_t*)dst)[i] = ((intptr_t*)src)[i];
  return dst; }

static Inline void *setw(void *d, intptr_t w, size_t n) {
  while (n) ((intptr_t*)d)[--n] = w;
  return d; }

static Inline struct tag *mo_tag(mo k) {
  for (;; k++) if (!G(k)) return (struct tag*) k; }

static Inline bool nilp(ob _) { return _ == nil; }
static Inline bool nump(ob _) { return _ & 1; }
static Inline bool homp(ob _) { return !nump(_); }

static Inline bool tblp(ob _) { return homp(_) && (typ) GF(_) == &tbl_typ; }
static Inline bool strp(ob _) { return homp(_) && (typ) GF(_) == &str_typ; }
static Inline bool twop(ob _) { return homp(_) && (typ) GF(_) == &two_typ; }
static Inline bool symp(ob _) { return homp(_) && (typ) GF(_) == &sym_typ; }

static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(ob), r = b % sizeof(ob);
  return r ? q + 1 : q; }

// this can give a false positive if x is a fixnum
static Inline bool livep(la v, ob x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static Inline intptr_t ror(intptr_t x, uintptr_t n) {
  return (x << ((8 * sizeof(intptr_t)) - n)) | (x >> n); }

static Inline mo mo_ini(void *_, size_t len) {
  struct tag *t = (struct tag*) ((mo) _ + len);
  return t->null = NULL, t->head = _; }

static Inline two two_ini(void *_, ob a, ob b) {
  two w = _; return
    w->act = act, w->typ = &two_typ,
    w->a = a, w->b = b, w; }

static Inline str str_ini(void *_, size_t len) {
  str s = _; return
    s->act = act, s->typ = &str_typ,
    s->len = len, s; }

static Inline tbl ini_tbl(void *_, size_t len, size_t cap, struct tbl_e **tab) {
  tbl t = _; return
    t->act = act, t->typ = &tbl_typ,
    t->len = len, t->cap = cap, t->tab = tab, t; }

static Inline sym ini_anon(void *_, uintptr_t code) {
  sym y = _; return
    y->act = act, y->typ = &sym_typ,
    y->nom = 0, y->code = code, y; }

static Inline bool eql(li v, ob a, ob b) {
  return a == b || _eql(v, a, b); }

#define gettyp(x) ((typ)GF((x)))

// " the interpreter "
#define Vm(n, ...) NoInline enum status\
  n(la v, ob xp, mo ip, ob *hp, ob *sp, frame fp, ##__VA_ARGS__)
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
#define CallOut(...) ((void)(Pack(),__VA_ARGS__,Unpack()))

#define ApC(f, x) (f)(v, (x), ip, hp, sp, fp)
#define ApY(f, x) (ip = (mo) (f), ApC(G(ip), (x)))
#define ApN(n, x) (xp = (x), ip += (n), ApC(G(ip), xp))

#define Yield(s, x) (v->xp = (s), ApC(yield_status, (x)))
#define ArityCheck(n) if (n > fp->argc) return Yield(ArityError, putnum(n))
#define Check(_) if (!(_)) return Yield(DomainError, xp)
#define Have(n) if (sp - hp < n) return (v->xp = n, ApC(gc, xp))
// sp is at least hp so this is a safe check for 1 word
#define Have1() if (sp == hp) return (v->xp = 1, ApC(gc, xp))

// used by the compiler but not exposed as primitives
#define ForEachInstruction(_)\
 _(call) _(ret) _(rec) _(jump) _(varg)\
 _(arity) _(ary1) _(ary2) _(ary3) _(ary4)\
 _(idno) _(idmo) _(idtwo) _(idtbl)\
 _(imm) _(immn1) _(imm0) _(imm1)\
 _(immn1p) _(imm0p) _(imm1p) _(immp)\
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
 _(sar) _(sal) _(band) _(bor) _(bxor) _(bnot)\
 _(lt) _(lteq) _(eq) _(gteq) _(gt)\
 _(tget) _(tset) _(thas) _(tlen)\
 _(cons) _(car) _(cdr)\
 _(br1) _(br0) _(bre) _(brn)\
 _(brl) _(brle) _(brge) _(brg)\
 _(push)

// primitive functions
#define ForEachFunction(_)\
 _(ev_f, "ev") _(ap_f, "ap")\
 _(hom_f, "hom") _(homp_f, "homp")\
 _(poke_f, "poke") _(peek_f, "peek")\
 _(seek_f, "seek") _(hfin_f, "hfin")\
 _(nump_f, "nump") _(rand_f, "rand")\
 _(add_f, "+") _(sub_f, "-") _(mul_f, "*")\
 _(quot_f, "/") _(rem_f, "%")\
 _(sar_f, ">>") _(sal_f, "<<")\
 _(band_f, "&") _(bnot_f, "!") _(bor_f, "|") _(bxor_f, "^")\
 _(twop_f, "twop") _(cons_f, "X") _(car_f, "A") _(cdr_f, "B")\
 _(tbl_f, "tbl") _(tblp_f, "tblp") _(tlen_f, "tlen")\
 _(tget_f, "tget") _(thas_f, "thas") _(tset_f, "tset")\
 _(tdel_f, "tdel") _(tkeys_f, "tkeys")\
 _(str_f, "str") _(strp_f, "strp") _(slen_f, "slen")\
 _(ssub_f, "ssub") _(scat_f, "scat") _(sget_f, "schr")\
 _(sym_f, "sym") _(symp_f, "symp") _(ynom_f, "ynom")\
 _(tx_f, ".") _(txc_f, "putc") _(rxc_f, "getc")\
 _(eq_f, "=") _(lt_f, "<") _(lteq_f, "<=")\
 _(gteq_f, ">=") _(gt_f, ">") _(nilp_f, "nilp")\
 _(xdom, "nope")

#define decl(x, ...) Vm(x);
ForEachInstruction(decl)
ForEachFunction(decl)
#undef decl
#endif
