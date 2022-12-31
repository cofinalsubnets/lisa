#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>

_Static_assert(-1 == -1 >> 1, "signed >>");
_Static_assert(sizeof(size_t) == sizeof(void*),
  "size_t size == data pointer size");

struct carrier;
enum status;

enum status la_ini(struct carrier*);
void la_fin(struct carrier*);

// thanks !!
typedef void u0;
typedef bool u1;

typedef intptr_t I;
typedef uintptr_t U;
typedef I ob;
typedef struct carrier *la;
typedef struct mo *mo; // procedures
typedef struct frame *sf, *frame;
typedef enum status vm(la, ob, mo, ob*, ob*, frame); // interpreter function type

struct mo { vm *ap; };
struct tl { struct mo *null, *head, end[]; };

struct frame { // stack frame
  ob *clos; // closure pointer FIXME // use stack
  mo retp; // thread return address
  struct frame *subd; // stack frame of caller
  U argc; // argument count
  ob argv[]; };

typedef const struct typ {
  vm *does;
  u1 (*equi)(la, I, I);
  I  (*hash)(la, I);
  u0 (*emit)(la, FILE*, I);
  //  u0 (*walk)(la, ob, ob*, ob*);
  I  (*evac)(la, I, I*, I*); } *typ;

typedef struct two {
  vm *data; typ typ;
  ob a, b; } *two;

typedef struct str { // strings
  vm *data; typ typ;
  U len; char text[]; } *str;

typedef struct sym {
  vm *data; typ typ;
  str nom;
  I code;
  // symbols are interned into a binary search tree.
  // anonymous symbols (nom == 0) don't have branches.
  struct sym *l, *r; } *sym;

struct tbl_e;

typedef struct tbl { // hash tables
  vm *data; typ typ;
  U len, cap;
  struct tbl_e **tab; } *tbl;

// linked list for gc protection
struct ll { ob *addr; struct ll *next; };

struct glob {
  sym define, cond, lambda, quote, begin, splat, eval; };

struct carrier {
  // registers -- in CPU registers when VM is running
  mo ip;
  frame fp;
  ob xp, *hp, *sp;

  // global variables & state
  tbl topl, macros; // global scope
  sym syms; // symbol table
  struct glob lex;
  U rand;
  enum status (*exit)(struct carrier *, enum status);

  // memory manager state
  U len;
  ob *pool;
  struct ll *safe;
  union {
    ob *cp; // TODO copy pointer for cheney's algorithm
    U t0; } run; };


static vm data; // dataatch instruction for data threads; also used as a sentinel
static sym symof(struct carrier*, str);
static str str_ini(u0*, U);
static tbl mktbl(la), tbl_set(la, tbl, ob, ob);
static two pair(la, ob, ob), two_ini(u0*, ob, ob);
static mo mo_n(struct carrier*, U), mo_ini(u0*, U), thd(la, ...);
static ob tbl_get(la, tbl, ob, ob),
   cp(la, ob, ob*, ob*), // copy something; used by type-specific copying functions
   hnom(la, mo); // get function name FIXME hide this
static u1 please(la, U),
   pushs(la, ...), // push args onto stack; true on success
   eql(la, ob, ob), // object equality
   neql(la, ob, ob); // always returns false
static U llen(ob);
static I hash(la, ob);

static u0 unwind(la), transmit(la, FILE*, ob),
          la_perror(la, enum status);


#define Width(_) b2w(sizeof(_))

#define getnum(_) ((ob)(_)>>1)
#define putnum(_) (((ob)(_)<<1)|1)

#define nil putnum(0)

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

static Inline u0 fputsn(const char *s, U n, FILE *o) {
  while (n--) putc(*s++, o); }

static Inline I lcprng(I s) { // the constant came from a paper
  const I steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }

static Inline struct tl *mo_tl(mo k) {
  for (;; k++) if (!G(k)) return (struct tl*) k; }


static Inline u1 nilp(ob _) { return _ == nil; }
static Inline u1 nump(ob _) { return _ & 1; }
static Inline u1 homp(ob _) { return !nump(_); }

static const struct typ two_typ, str_typ, tbl_typ, sym_typ;
static Inline u1 tblp(ob _) {
  return homp(_) && (typ) GF(_) == &tbl_typ; }
static Inline u1 strp(ob _) {
  return homp(_) && (typ) GF(_) == &str_typ; }
static Inline  u1 twop(ob _) {
  return homp(_) && (typ) GF(_) == &two_typ; }
static Inline u1 symp(ob _) {
  return homp(_) && (typ) GF(_) == &sym_typ; }

static Inline U b2w(U b) { U q, r; return
  q = b / sizeof(ob),
  r = b % sizeof(ob),
  r ? q + 1 : q; }

// this can give a false positive if x is a fixnum
static Inline u1 livep(la v, ob x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static Inline  I ror(I x, U n) { return (x<<((8*sizeof(I))-n))|(x>>n); }

#define Gc(n) ob n(la v, ob x, ob *pool0, ob *top0)

static Inline u0 *cpyw_r2l(u0 *dst, const u0 *src, U n) {
  while (n--) ((U*)dst)[n] = ((U*)src)[n];
  return dst; }

static Inline u0 *cpyw_l2r(u0 *dst, const u0 *src, U n) {
  for (U i = 0; i < n; i++) ((U*)dst)[i] = ((U*)src)[i];
  return dst; }

static Inline u0 *setw(u0 *d, I w, U n) {
  while (n) ((I*)d)[--n] = w;
  return d; }

// length of list
static U llen(ob l) {
  for (U i = 0;;)
    if (twop(l)) l = B(l), i++;
    else return i; }


// these are vm functions used by C but not lisp.
#define cfns(_) _(gc) _(xdom) _(xoom) _(xary) _(xok)
#define ninl(x, ...) static NoInline vm x;
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
 _(sar) _(sal) _(band) _(bor) _(bxor) _(bnot)\
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

#define Vm(n) static NoInline enum status\
  n(la v, ob xp, mo ip, ob *hp, ob *sp, frame fp)

static U copy(la, U);
static u0 do_copy(la, U, ob*);

// FIXME the garbage collector works pretty well but it could be better:
//
// - it uses stack recursion so a process that constructs infinite
//   data will stack overflow, rather than fail gracefully with oom.
//   we could fix this with cheney's algorithm.
//
// - we allocate a new pool every cycle rather than keeping two pools
//   at all times. theoretically this means we have less memory allocated
//   most of the time, and if malloc is efficient then the overhead from
//   calling it every cycle should be negligible, but it would still be
//   better only to call out when we need to grow or shrink the pool.

#define VIT_FLOOR 32
#define VIT_CEIL 128
////
/// garbage collector
//
// please : u1 la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// governs the size of the memory pool.
u1 please(la v, U req) {
  // copy into a new pool of the same size.
  U len = v->len, vit = copy(v, len);
  // if this fails then the request fails.
  if (!vit) return 0;
  U tar = len, all = len - (Avail - req);
  // adjust size up if we're too small or slow.
  while (all > tar || vit < VIT_FLOOR) tar <<= 1, vit <<= 1;
  // adjust size down if we're big and fast enough.
  while (all < tar>>1 && vit > VIT_CEIL) tar >>= 1, vit >>= 1;
  // if we don't need to resize, return success.
  return tar == len
    // otherwise adjust the size and copy again.
    || copy(v, tar)
    // if that fails, succeed if we have enough free space.
    || all <= len; }

// copy : la_clock_t la size_t
// relocate all reachable data into a newly allocated
// memory pool of the given length. return 0 if a new
// pool can't be allocated or else a positive integer
// value u that's higher the less time we spend in GC:
//
//   u = t1 == t2 ? 1 : (t2 - t0) / (t2 - t1)
//
// where
//
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
static U copy(la v, U len) {
  U t1 = clock(), t0 = v->run.t0, t2;
  ob *pool1 = malloc(len * sizeof(ob));
  if (!pool1) return 0;

  ob *pool0 = v->pool;
  do_copy(v, len, pool1);
  free(pool0);

  t2 = v->run.t0 = clock();
  t1 = t2 - t1;
  return t1 ? (t2 - t0) / t1 : VIT_CEIL; }

static u0 do_copy(la v, U len1, ob *pool1) {
  ob len0 = v->len,
     *sp0 = v->sp,
     *pool0 = v->pool,
     *top0 = pool0 + len0,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->syms = 0;
  v->len = len1;
  v->hp = v->pool = pool1;
  v->sp = sp0 + shift;
  v->fp = (sf) ((ob*) v->fp + shift);

  v->xp = cp(v, v->xp, pool0, top0);
  v->ip = (mo) cp(v, (ob) v->ip, pool0, top0);

  // copy globals
  v->topl = (tbl) cp(v, (ob) v->topl, pool0, top0);
  v->macros = (tbl) cp(v, (ob) v->macros, pool0, top0);
  for (U i = 0; i < Width(struct glob); i++)
    ((ob*)&v->lex)[i] = cp(v, ((ob*)&v->lex)[i], pool0, top0);
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);

  // copy the stack
  ob *sp = v->sp;
  sf fp = v->fp;
  for (;;) {
    while (sp < (ob*) fp) *sp++ = cp(v, *sp0++, pool0, top0);
    if (sp0 == top0) break;
    sf fp0 = (sf) sp0;
    fp->argc = fp0->argc;
    fp->subd = (sf) ((ob*) fp0->subd + shift);
    fp->clos = (ob*) cp(v, (ob) fp0->clos, pool0, top0);
    fp->retp = (mo) cp(v, (ob) fp0->retp, pool0, top0);
    sp = fp->argv;
    sp0 = fp0->argv;
    fp = fp->subd; } }

// unchecked allocator -- make sure there's enough memory!
static Inline u0 *bump(la v, U n) {
  u0*x = v->hp;
  return v->hp += n, x; }

static NoInline ob cp_mo(la v, mo src, ob *pool0, ob *top0) {
  struct tl *fin = mo_tl(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;

  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  for (GF(d) = (vm*) dst; d-- > dst;
    G(d) = (vm*) cp(v, (ob) G(d), pool0, top0));
  return (ob) (src - ini + dst); }

#define stale(o) ((ob*)(o) >= pool0 && (ob*) o < top0)
Gc(cp) {
  if (nump(x) || !stale(x)) return x;
  ob y = (ob) G(x);
  if (!nump(y) && livep(v, y)) return y;
  if ((vm*) y == data) return
    ((typ) GF(x))->evac(v, x, pool0, top0);
  return cp_mo(v, (mo) x, pool0, top0); }

enum status {
  Eof = -1,
  Ok,
  DomainError,
  ArityError,
  NameError,
  SyntaxError,
  SystemError,
  OomError };

static sym symofs(la, const char*);
static str strof(la, const char*);
static u1
  defprim(la, vm*, const char*) NoInline,
  inst(la, const char*, vm*),
  la_ini_(la);

static enum status vm_exit(la v, enum status r) { return r; }
NoInline enum status la_ini(la v) {
  const U len = 1 << 10; // power of 2
  memset(v, 0, sizeof(struct carrier));
  ob *pool = malloc(len * sizeof(ob));
  return !pool ? OomError :
    (v->len = len,
     v->hp = v->pool = pool,
     v->fp = (sf) (v->sp = pool + len),
     v->rand = v->run.t0 = clock(),
     v->exit = vm_exit,
     la_ini_(v) ? Ok : (la_fin(v),  OomError)); }

u0 la_fin(struct carrier *v) {
  if (v) free(v->pool), v->pool = NULL; }


static Inline u0 *cells(la v, U n) { return
  Avail >= n || please(v, n) ? bump(v, n) : 0; }

static str strof(la v, const char* c) {
  U bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  return o ? (memcpy(o->text, c, bs), str_ini(o, bs)) : o; }

// initialization helpers
static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

#define dp(go, nom) && defprim(v, go, nom)
static NoInline u1 defprim(la v, vm *i, const char *n) {
  mo k; sym y; return
    (y = symofs(v, n)) &&
    (with(y, k = mo_n(v, 2)), k) &&
    (k[0].ap = i,
     k[1].ap = (vm*) y,
     tbl_set(v, v->topl, (ob) y, (ob) k)); }

#define reg_intl(a) && inst(v, "i-"#a, a)
static u1 la_ini_(la v) { sym y; ob _; return
  (y = symofs(v, "ev"), v->lex.eval = y) &&
  (y = symofs(v, ":"), v->lex.define = y) &&
  (y = symofs(v, "?"), v->lex.cond = y) &&
  (y = symofs(v, "\\"), v->lex.lambda = y) &&
  (y = symofs(v, "`"), v->lex.quote = y) &&
  (y = symofs(v, ","), v->lex.begin = y) &&
  (y = symofs(v, "."), v->lex.splat = y) &&
  (v->topl = mktbl(v)) i_internals(reg_intl) &&
  (v->macros = mktbl(v)) &&
  (_ = (ob) symofs(v, "_ns")) &&
  tbl_set(v, v->topl, _, (ob) v->topl) &&
  (_ = (ob) symofs(v, "macros")) &&
  tbl_set(v, v->topl, _, (ob) v->macros)
  i_primitives(dp); }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static u1 inst(la v, const char *a, vm *b) {
  sym z; return (z  = symofs(v, a)) &&
                tbl_set(v, v->topl, (ob) z, (ob) b); }

u1 neql(la v, ob x, ob y) { return false; }
u1 eql(la v, ob a, ob b) { return a == b ||
  (!nump(a|b) && G(a) == data &&
   ((typ) GF(a))->equi(v, a, b)); }

#define T putnum(-1)
// comparison operators
Vm(lt) { return ApN(1, *sp++ < xp ? T : nil); }
Vm(lteq) { return ApN(1, *sp++ <= xp ? T : nil); }
Vm(eq) { return ApN(1, eql(v, *sp++, xp) ? T : nil); }
Vm(gteq) { return ApN(1, *sp++ >= xp ? T : nil); }
Vm(gt) { return ApN(1, *sp++ > xp ? T : nil); }

// TODO remove macros
#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
#define EQ(a,b) eql(v,a,b)
#define cmp(op, n) Vm(n##_f) {\
  for (long i = fp->argc-1; i > 0; i--)\
    if (!op(fp->argv[i-1], fp->argv[i])) return ApC(ret, nil);\
  return ApC(ret, T); }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(EQ, eq)

// type predicates
#define Tp(t)\
  Vm(t##p_) { return ApN(1, (t##p(xp)?T:nil)); }\
  Vm(t##p_f) {\
    for (size_t i = fp->argc; i;)\
      if (!t##p(fp->argv[--i])) return ApC(ret, nil);\
    return ApC(ret, T); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// type/arity checking
Vm(idno) { return nump(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idmo) { return homp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idtbl) { return tblp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idtwo) { return twop(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(arity) { return
  fp->argc >= getnum(GF(ip)) ? ApN(2, xp) :
  ApC(xary, (ob) GF(ip)); }
Vm(ary1) { return fp->argc >= 1 ? ApN(1, xp) : ApC(xary, putnum(1)); }
Vm(ary2) { return fp->argc >= 2 ? ApN(1, xp) : ApC(xary, putnum(2)); }
Vm(ary3) { return fp->argc >= 3 ? ApN(1, xp) : ApC(xary, putnum(3)); }
Vm(ary4) { return fp->argc >= 4 ? ApN(1, xp) : ApC(xary, putnum(4)); }
////
/// Branch Instructions
//
// calling and returning
//
// return from a function
Vm(ret) { return
  ip = fp->retp,
  sp = fp->argv + fp->argc,
  fp = fp->subd,
  ApY(ip, xp); }

// normal function call
Vm(call) {
  Have(Width(struct frame));
  sf subd = fp;
  sp = (ob*) (fp = (sf) sp - 1);
  fp->argc = getnum(GF(ip));
  fp->retp = FF(ip);
  fp->subd = subd;
  fp->clos = (ob*) nil;
  return ApY(xp, nil); }

// tail calls
Vm(rec) {
  U adic = getnum(GF(ip));
  // save return address
  sf subd = fp->subd;
  mo retp = fp->retp;
  // reset fp
  fp = (sf) (fp->argv + fp->argc - adic) - 1;
  // copy the args high to low BEFORE repopulating fp.
  cpyw_r2l(fp->argv, sp, adic);
  sp = (ob*) fp;
  // populate fp
  fp->retp = retp;
  fp->subd = subd;
  fp->argc = adic;
  fp->clos = (ob*) nil;
  return ApY((mo) xp, nil); }

Vm(ap_f) {
  ArityCheck(2);
  Check(homp(fp->argv[0]));
  xp = fp->argv[1];
  U adic = llen(xp);
  Have(adic);
  ip = (mo) fp->argv[0];
  sf subd = fp->subd;
  mo retp = fp->retp;
  sp = (ob*) (fp = (sf) (fp->argv + fp->argc - adic) - 1);
  fp->retp = retp;
  fp->argc = adic;
  fp->subd = subd;
  fp->clos = (ob*) nil;
  for (ob *i = fp->argv; adic--; xp = B(xp)) *i++ = A(xp);
  return ApY(ip, nil); }

// unconditional jump
Vm(jump) { return ApY(GF(ip), xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, b) Vm(nom) { return\
  ApY((test) ? (ob) a(ip) : (ob) b(ip), xp); }
// combined test/branch instructions
Br(br1, nilp(xp), FF, GF)
Br(br0, nilp(xp), GF, FF)
Br(bre, eql(v, xp, *sp++), GF, FF)
Br(brn, eql(v, xp, *sp++), FF, GF)
Br(brl,   *sp++ <  xp, GF, FF)
Br(brle,  *sp++ <= xp, GF, FF)
Br(brg,   *sp++ >  xp, GF, FF)
Br(brge,  *sp++ >= xp, GF, FF)

////
/// Load Instructions
//

// immediate values
Vm(imm) { return ApN(2, (ob) GF(ip)); }
Vm(imm0) { return ApN(1, putnum(0)); }
Vm(imm1) { return ApN(1, putnum(1)); }
Vm(immn1) { return ApN(1, putnum(-1)); }

Vm(imm0p) { return ApC(push, putnum(0)); }
Vm(imm1p) { return ApC(push, putnum(1)); }
Vm(immn1p) { return ApC(push, putnum(-1)); }

// function arguments
Vm(argn) { return ApN(2, fp->argv[getnum(GF(ip))]); }
Vm(arg0) { return ApN(1, fp->argv[0]); }
Vm(arg1) { return ApN(1, fp->argv[1]); }
Vm(arg2) { return ApN(1, fp->argv[2]); }
Vm(arg3) { return ApN(1, fp->argv[3]); }
Vm(arg0p) { return ApC(push, fp->argv[0]); }
Vm(arg1p) { return ApC(push, fp->argv[1]); }
Vm(arg2p) { return ApC(push, fp->argv[2]); }
Vm(arg3p) { return ApC(push, fp->argv[3]); }

// the first two stack slots under the current frame
// may hold extra call data.
#define Slot1 ((ob**)fp)[-1]
#define Slot2 ((ob**)fp)[-2]

// local variables
Vm(sl1n) { return ApN(2, Slot1[getnum(GF(ip))]); }
Vm(sl10) { return ApN(1, Slot1[0]); }
Vm(sl11) { return ApN(1, Slot1[1]); }
Vm(sl12) { return ApN(1, Slot1[2]); }
Vm(sl13) { return ApN(1, Slot1[3]); }
Vm(sl10p) { return ApC(push, Slot1[0]); }
Vm(sl11p) { return ApC(push, Slot1[1]); }
Vm(sl12p) { return ApC(push, Slot1[2]); }
Vm(sl13p) { return ApC(push, Slot1[3]); }

// closure variables
Vm(clon) { return ApN(2, fp->clos[getnum(GF(ip))]); }
Vm(clo0) { return ApN(1, fp->clos[0]); }
Vm(clo1) { return ApN(1, fp->clos[1]); }
Vm(clo2) { return ApN(1, fp->clos[2]); }
Vm(clo3) { return ApN(1, fp->clos[3]); }
Vm(clo0p) { return ApC(push, fp->clos[0]); }
Vm(clo1p) { return ApC(push, fp->clos[1]); }
Vm(clo2p) { return ApC(push, fp->clos[2]); }
Vm(clo3p) { return ApC(push, fp->clos[3]); }

////
/// Store Instructions
// // stack push
Vm(push) { Have1(); return
  *--sp = xp,
  ApN(1, xp); }

// set a local variable
Vm(defsl1) { return
  Slot1[getnum(GF(ip))] = xp,
  ApN(2, xp); }

// set a module variable
Vm(deftop) { u1 _; return
  CallOut(_ = tbl_set(v, (tbl) A(GF(ip)), B(GF(ip)), xp)),
  _ ? ApN(2, xp) : ApC(xoom, xp); }

// allocate local variable array
Vm(setloc) {
  U n = getnum((ob) GF(ip));
  // + 1 for the stack slot
  Have(n + Width(struct tl) + 1);
  mo t = setw(mo_ini(hp, n), nil, n);
  return
    hp += n + Width(struct tl),
    *--sp = (ob) t,
    ApN(2, xp); }

NoInline Vm(xnom);
// late binding
// TODO dynamic type checking here
Vm(late) {
  ob w = (ob) GF(ip), d = A(w);
  xp = B(w);
  w = tbl_get(v, (tbl) d, xp, 0); // FIXME call name resolve procedure
  if (!w) return ApC(xnom, xp);
  xp = w;
  // omit the arity check if possible
  vm *n = G(FF(ip));
  if ((n == call || n == rec) && // xp will be a hom
      G(xp) == arity &&
      (ob) GF(FF(ip)) >= (ob) GF(xp))
    xp = (ob) FF(ip);
  return
    G(ip) = imm,
    GF(ip) = (vm*) xp,
    ApN(2, xp); }

// varargs
NoInline Vm(varg0) {
  Have1(); return
    fp = cpyw_l2r((ob*) fp - 1, fp, Width(struct frame) + fp->argc),
    sp = (ob*) fp,
    fp->argv[fp->argc++] = nil,
    ApN(2, xp); }

Vm(varg) {
  U reqd = getnum((ob) GF(ip));
  if (reqd == fp->argc) return ApC(varg0, xp);
  if (reqd > fp->argc) return ApC(xary, putnum(reqd));
  U vdic = fp->argc - reqd;
  // in this case we need to add another argument
  // slot to hold the nil.
  // in this case we just keep the existing slots.
  Have(Width(struct two) * vdic);
  two t = (two) hp;
  hp += Width(struct two) * vdic;
  for (U i = vdic; i--;
    two_ini(t + i, fp->argv[reqd + i], (ob) (t + i + 1)));
  t[vdic-1].b = nil;
  fp->argv[reqd] = (ob) t;
  return ApN(2, xp); }

// math functions
// frameless
Vm(add) { return ApN(1, xp + *sp++ - 1); }
Vm(sub) { return ApN(1, *sp++ - xp + 1); }
Vm(mul) { return ApN(1, putnum(getnum(*sp++) * getnum(xp))); }
Vm(neg) { return ApN(1, ~xp+3); }

Vm(quot) { return xp == putnum(0) ? ApC(xdom, xp) :
  ApN(1, putnum(getnum(*sp++) / getnum(xp))); }

Vm(rem) { return xp == putnum(0) ? ApC(xdom, xp) :
  ApN(1, putnum(getnum(*sp++) % getnum(xp))); }

Vm(sar) { return ApN(1, putnum(getnum(*sp++) >> getnum(xp))); }
Vm(sal) { return ApN(1, putnum(getnum(*sp++) << getnum(xp))); }
Vm(bor) { return ApN(1, xp | *sp++); }
Vm(band) { return ApN(1, xp & *sp++); }
Vm(bxor) { return ApN(1, (xp ^ *sp++) | 1); }
Vm(bnot) { return ApN(1, ~xp | 1); }

// framed
// FIXME do type checks
Vm(add_f) {
  xp = 0;
  for (U i = 0; i < fp->argc; xp += getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }
Vm(mul_f) {
  xp = 1;
  for (U i = 0; i < fp->argc; xp *= getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }

Vm(sub_f) {
  if (fp->argc == 0) return ApC(ret, xp);
  if (fp->argc == 1) return ApC(ret, putnum(-getnum(fp->argv[0])));
  xp = getnum(fp->argv[0]);
  U i = 1;
  do xp -= getnum(fp->argv[i++]); while (i < fp->argc);
  return ApC(ret, putnum(xp)); }

Vm(quot_f) {
  if (fp->argc == 0) return ApC(ret, putnum(1));
  xp = getnum(fp->argv[0]);
  for (U i = 1; i < fp->argc; i++) {
    I n = getnum(fp->argv[i]);
    Check(n);
    xp /= n; }
  return ApC(ret, putnum(xp)); }

Vm(rem_f) {
  if (fp->argc == 0) return ApC(ret, putnum(1));
  xp = getnum(fp->argv[0]);
  for (U i = 1; i < fp->argc; i++) {
    I n = getnum(fp->argv[i]);
    Check(n);
    xp %= n; }
  return ApC(ret, putnum(xp)); }

Vm(bor_f) {
  xp = 0;
  for (U i = 0; i < fp->argc; xp |= getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }

Vm(bxor_f) {
  xp = 0;
  for (U i = 0; i < fp->argc; xp ^= getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }

Vm(band_f) {
  xp = -1;
  for (U i = 0; i < fp->argc; xp &= getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }

Vm(bnot_f) { return
  xp = fp->argc ? *fp->argv : 0,
  ApC(ret, ~xp|1); }

Vm(sar_f) {
  if (fp->argc == 0) return ApC(ret, xp);
  if (fp->argc == 1) return ApC(ret, putnum(getnum(fp->argv[0])>>1));
  xp = getnum(fp->argv[0]);
  U i = 1;
  do xp >>= getnum(fp->argv[i++]); while (i < fp->argc);
  return ApC(ret, putnum(xp)); }

Vm(sal_f) {
  if (fp->argc == 0) return ApC(ret, xp);
  if (fp->argc == 1) return ApC(ret, putnum(getnum(fp->argv[0])<<1));
  xp = getnum(fp->argv[0]);
  U i = 1;
  do xp <<= getnum(fp->argv[i++]); while (i < fp->argc);
  return ApC(ret, putnum(xp)); }

Vm(rand_f) { return
  v->rand = lcprng(v->rand),
  ApC(ret, putnum(v->rand)); }


Vm(yield) {
  enum status s = v->xp;
  return Pack(), v->exit(v, s); }
#define Yield(s) (v->xp = (s), ApC(yield, xp))
Vm(xary) { return Yield(ArityError); }
Vm(xok) { return Yield(Ok); }
Vm(xdom) { return Yield(DomainError); }
Vm(xoom) { return Yield(OomError); }
Vm(xnom) { return Yield(NameError); }
NoInline Vm(gc) {
  U req = v->xp; return
    CallOut(req = please(v, req)),
    req ? ApY(ip, xp) : Yield(OomError); }
// Run a GC cycle from inside the VM

#undef Yield

#include <errno.h>
static NoInline u0 report(la, const char*, ...);
u0 la_perror(la v, enum status s) { switch (s) {
  // not error codes, so print nothing.
  case Ok: case Eof: return;
  case DomainError: report(v, "has no value"); break;
  case OomError: report(v, "oom at %d words", v->len); break;
  case SyntaxError: report(v, "syntax error"); break; // TODO source info
  case ArityError:
    report(v, "wrong arity : %d of %d", v->fp->argc, getnum(v->xp));
    break;
  case NameError: {
    const char *n = "#sym";
    U l = 4;
    str s = ((sym) v->xp)->nom;
    if (s) n = s->text, l = s->len;
    report(v, "free variable : %.*s", l, n); 
    break; }
  case SystemError:
    report(v, "system error : %s", strerror(errno)); } }

static NoInline u0 report_call(la v, mo ip, sf fp) {
  putc('(', stderr);
  transmit(v, stderr, (ob) ip);
  for (size_t i = 0, argc = fp->argc; i < argc;
    putc(' ', stderr),
    transmit(v, stderr, fp->argv[i++]));
  putc(')', stderr); }

// this prints a backtrace.
// TODO maybe show it upside down like python?
#define aubas (((ob*) fp) == v->pool + v->len)
static NoInline u0 report(la v, const char *msg, ...) {
  mo ip = v->ip;
  sf fp = v->fp;

  // print error
  fputs(";; ", stderr);

  // show the function if there is one
  if (!aubas)
    report_call(v, ip, fp),
    putc(' ', stderr),
    ip = fp->retp,
    fp = fp->subd;

  // show message
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  putc('\n', stderr);

  // show backtrace
  while (!aubas)
    fputs(";; in ", stderr),
    report_call(v, ip, fp),
    putc('\n', stderr),
    ip = (mo) fp->retp,
    fp = fp->subd; }

u0 unwind(la v) {
  v->sp = v->pool + v->len,
  v->fp = (sf) v->sp,
  v->ip = 0,
  v->xp = nil; }

// out

static u0 tx_nom(la, FILE*, ob);
u0 transmit(la v, FILE* o, ob x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (G(x) == data) ((typ) GF(x))->emit(v, o, x);
  else tx_nom(v, o, hnom(v, (mo) x)); }

// print a function name // this is weird
static NoInline u0 tx_nom(la v, FILE* o, ob x) {
  if (symp(x)) putc('\\', o), transmit(v, o, x);
  else if (!twop(x)) putc('\\', o);
  else {
    if (symp(A(x)) || twop(A(x))) tx_nom(v, o, A(x));
    if (symp(B(x)) || twop(B(x))) tx_nom(v, o, B(x)); } }

Vm(txc_f) { return !fp->argc ?
  ApC(xary, putnum(1)) :
  ApC(ret, putnum(putc(getnum(fp->argv[0]), stdout))); }

Vm(tx_f) {
  U i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      transmit(v, stdout, fp->argv[i++]),
      putc(' ', stdout);
    xp = fp->argv[i];
    transmit(v, stdout, xp); }
  return putc('\n', stdout), ApC(ret, xp); }

Vm(rxc_f) { return ApC(ret, putnum(getc(stdin))); }


// in
#include <ctype.h>

static str rx_atom_str(la, FILE*), rx_str(la, FILE*);
static ob rx_atom(la, str), rx_ret(la, FILE*, ob),
  rx(la, FILE*), rx_two(la, FILE*);

static ob receive_x(la v, FILE* i) { return
  pushs(v, rx_ret, NULL) ? rx(v, i) : 0; }

// FIXME doesn't distinguish between OOM and parse error
static enum status receive(la v, FILE* i) {
  ob x = receive_x(v, i);
  return x ? (v->xp = x, Ok) : feof(i) ? Eof : SyntaxError; }

////
/// " the parser "
//
// simple except it uses the managed stack for recursion.

// get the next token character from the stream
static int rx_char(FILE* i) {
  for (int c;;) switch ((c = getc(i))) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (getc(i)) {
      case '\n': case EOF: return rx_char(i); } } }

static Inline ob rx_pull(la v, FILE *i, ob x) { return
  ((ob (*)(la, FILE*, ob))(*v->sp++))(v, i, x); }

static ob rx_ret(la v, FILE* i, ob x) { return x; }

static ob rx_two_cons(la v, FILE* i, ob x) {
  ob y = *v->sp++; return
    rx_pull(v, i, x ? (ob) pair(v, y, x) : x); }

static ob rx_two_cont(la v, FILE* i, ob x) {
  return !x || !pushs(v, rx_two_cons, x, NULL) ?
    rx_pull(v, i, 0) : rx_two(v, i); }

static ob rx_q(la v, FILE* i, ob x) { return
  x = x ? (ob) pair(v, x, nil) : x,
  x = x ? (ob) pair(v, (ob) v->lex.quote, x) : x,
  rx_pull(v, i, x); }

static NoInline ob rx(la v, FILE* i) {
  int c = rx_char(i);
  switch (c) {
    case ')': case EOF: return rx_pull(v, i, 0);
    case '(': return rx_two(v, i);
    case '"': return rx_pull(v, i, (ob) rx_str(v, i));
    case '\'': return
      pushs(v, rx_q, NULL) ? rx(v, i) : rx_pull(v, i, 0); }
  ungetc(c, i);
  str a = rx_atom_str(v, i);
  ob x = a ? rx_atom(v, a) : 0;
  return rx_pull(v, i, x); }

static NoInline ob rx_two(la v, FILE* i) {
  int c = rx_char(i);
  switch (c) {
    case ')': return rx_pull(v, i, nil);
    case EOF: return rx_pull(v, i, 0);
    default: return
      ungetc(c, i),
      pushs(v, rx_two_cont, NULL) ?
        rx(v, i) : rx_pull(v, i, 0); } }

static str mkbuf(la v) { str s; return
  s = cells(v, Width(struct str) + 1),
  s ? str_ini(s, sizeof(ob)) : s; }

static str buf_grow(la v, str s) {
  U len = s->len;
  str t; return
    with(s, t = cells(v, Width(struct str) + 2 * b2w(len))),
    !t ? t : (memcpy(t->text, s->text, len),
              str_ini(t, 2 * len)); }

// read the contents of a string literal into a string
static str rx_str(la v, FILE* p) {
  str o = mkbuf(v);
  for (U n = 0, lim = sizeof(ob); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = getc(p)) {
      // backslash causes the next character
      // to be read literally // TODO more escape sequences
      case '\\': if ((x = getc(p)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, o; }
  return 0; }

// read the characters of an atom (number or symbol)
// into a string
static str rx_atom_str(la v, FILE* p) {
  str o = mkbuf(v);
  for (U n = 0, lim = sizeof(ob); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = getc(p)) {
      default: o->text[n++] = x; continue;
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': ungetc(x, p);
      case EOF: return o->len = n, o; }
  return 0; }

static NoInline ob rx_atom_n(la v, str b, U inset, int sign, int rad) {
  static const char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
  U len = b->len;
  if (inset >= len) fail: return (ob) symof(v, b);
  I out = 0;
  do {
    int dig = 0, c = tolower(b->text[inset++]);
    while (digits[dig] && digits[dig] != c) dig++;
    if (dig >= rad) goto fail;
    out = out * rad + dig;
  } while (inset < len);
  return putnum(sign * out); }

static NoInline ob rx_atom(la v, str b) {
  size_t i = 0, len = b->len;
  int sign = 1;
  while (i < len) switch (b->text[i]) {
    case '+': i += 1; continue;
    case '-': i += 1, sign *= -1; continue;
    case '0': if (i+1 < len) {
      const char *r = "b\2s\6o\10d\12z\14x\20n\44";
      for (char c = tolower(b->text[i+1]); *r; r += 2)
        if (*r == c) return rx_atom_n(v, b, i+2, sign, r[1]); }
    default: goto out; } out:
  return rx_atom_n(v, b, i, sign, 10); }


static NoInline mo thdr(la v, U n, va_list xs) {
  vm *x = va_arg(xs, vm*);
  if (!x) return mo_n(v, n);
  mo k; with(x, k = thdr(v, n + 1, xs));
  if (k) k[n].ap = x;
  return k; }

// push things onto the stack
static NoInline u1 pushsr(la v, U i, va_list xs) {
  u1 _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i+1, xs)),
     _ && (*--v->sp = x, true)); }

static u1 pushs(la v, ...) {
  u1 _; va_list xs; return
  va_start(xs, v),
  _ = pushsr(v, 0, xs),
  va_end(xs),
  _; }

static mo thd(la v, ...) {
  mo k; va_list xs; return
  va_start(xs, v),
  k = thdr(v, 0, xs),
  va_end(xs),
  k; }


static vm ap_two;
static u1 eq_two(la, I, I);
static u0 tx_two(la, FILE*, I);
static I cp_two(la, I, I*, I*),
         hx_two(la, I);

static const struct typ two_typ = {
  .does = ap_two,
  .emit = tx_two,
  .evac = cp_two,
  .hash = hx_two,
  .equi = eq_two, };

two two_ini(u0 *_, ob a, ob b) {
  two w = _; return
    w->data = data,
    w->typ = &two_typ,
    w->a = a, w->b = b,
    w; }

// pairs and lists
static NoInline two pair_gc(la v, ob a, ob b) {
  u1 ok; return
    with(a, with(b, ok = please(v, Width(struct two)))),
    ok ? pair(v, a, b) : 0; }

NoInline two pair(la v, ob a, ob b) {
  return Avail >= Width(struct two) ?
    two_ini(bump(v, Width(struct two)), a, b) :
    pair_gc(v, a, b); }

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(Width(struct two));
  xp = (ob) two_ini(hp, xp, *sp++);
  hp += Width(struct two);
  return ApN(1, xp); }

Vm(car_f) {
  if (fp->argc)
    xp = fp->argv[0],
    xp = twop(xp) ? A(xp) : xp;
  return ApC(ret, xp); }

Vm(cdr_f) {
  if (fp->argc)
    xp = fp->argv[0],
    xp = twop(xp) ? B(xp) : nil;
  return ApC(ret, xp); }

Vm(cons_f) {
  if (fp->argc) {
    U n = Width(struct two) * (fp->argc - 1);
    Have(n);
    two w = (two) hp;
    hp += n;
    xp = fp->argv[fp->argc-1];
    for (size_t i = fp->argc - 1; i--;
      xp = (ob) two_ini(w+i, fp->argv[i], xp)); }
  return ApC(ret, xp); }

Vm(ap_two) { return
  ApC(ret, fp->argc ? B(ip) : A(ip)); }

static Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, Width(struct two));
  src->data = (vm*) dst;
  return (ob) two_ini(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

static u0 tx_two(la v, FILE* o, ob x) {
  putc('(', o);
  for (;;) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) break;
    putc(' ', o); }
  putc(')', o); }

static I hx_two(la v, ob x) {
  I hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(I)); }

static u1 eq_two(la v, ob x, ob y) {
  return (typ) GF(y) == &two_typ &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }


static vm ap_tbl;
static u0 tx_tbl(la, FILE*, I);
static I hx_tbl(la, I), cp_tbl(la, I, I*, I*);

static const struct typ tbl_typ = {
  .does = ap_tbl, .emit = tx_tbl, .evac = cp_tbl,
  .hash = hx_tbl, .equi = neql, };

// FIXME this is a totally ad hoc, unproven hashing method.
//
// its performance on hash tables and anonymous functions
// is very bad (they all go to the same bucket!)
//
// strings, symbols, and numbers do better. for pairs it
// depends on what they contain.
//
// copying GC complicates the use of memory addresses for
// hashing mutable data, which is the obvious way to fix
// the bad cases. we would either need to assign each datum
// a unique identifier when it's created & hash using that,
// or use the address but rehash as part of garbage collection.
//
// TODO replace with something better, verify & benchmark

// just a big random number!
static const I mix = 2708237354241864315;

I hash(la v, ob x) {
  if (nump(x)) return ror(mix * x, sizeof(I) * 2);
  if (G(x) == data) return ((typ) GF(x))->hash(v, x);
  if (!livep(v, x)) return mix ^ (x * mix);
  return mix ^ hash(v, hnom(v, (mo) x)); }

// hash tables
// some of the worst code is here :(

static Inline U tbl_load(tbl t) { return t->len / t->cap; }
static Inline U tbl_idx(U cap, U co) { return co & (cap - 1); }
static Inline tbl ini_tbl(u0 *_, U len, U cap, struct tbl_e **tab) {
  tbl t = _; return
    t->data = data,
    t->typ = &tbl_typ,
    t->len = len,
    t->cap = cap,
    t->tab = tab,
    t; }

struct tbl_e { I key, val; struct tbl_e *next; };
static struct tbl_e *tbl_ent_hc(la v, tbl t, ob k, U hc) {
  struct tbl_e *e = t->tab[tbl_idx(t->cap, hc)];
  while (e && !eql(v, e->key, k)) e = e->next;
  return e; }

static struct tbl_e *tbl_ent(la v, tbl t, ob k) {
  return tbl_ent_hc(v, t, k, hash(v, k)); }

static u1 tblss(la, I, I);
static u0 tbl_shrink(la, tbl);
static ob tbl_del_s(la, tbl, ob, ob), tbl_keys(la);
static tbl tbl_grow(la, tbl), tbl_set_s(la, tbl, ob, ob);

tbl mktbl(la v) {
  tbl t = cells(v, Width(struct tbl) + 1);
  if (t) ini_tbl(t, 0, 1, (struct tbl_e**) (t + 1)), t->tab[0] = 0;
  return t; }

tbl tbl_set(la v, tbl t, ob k, ob x) { return
  t = tbl_set_s(v, t, k, x),
  t ? tbl_grow(v, t) : 0; }

ob tbl_get(la v, tbl t, ob k, ob d) {
  struct tbl_e *e = tbl_ent(v, t, k);
  return e ? e->val : d; }

Vm(tget_f) { return
  fp->argc < 2 ? ApC(xary, putnum(2)) :
  !tblp(fp->argv[0]) ? ApC(xdom, xp) :
  ApC(ret, tbl_get(v, (tbl) fp->argv[0], fp->argv[1], nil)); }

Vm(tdel_f) {
  ArityCheck(1);
  Check(tblp(fp->argv[0]));
  tbl t = (tbl) fp->argv[0];
  for (U i = 1, l = fp->argc; i < l; i++)
    xp = tbl_del_s(v, t, fp->argv[i], xp);
  if (!tbl_load(t)) tbl_shrink(v, t);
  return ApC(ret, xp); }

Vm(tget) { return
  xp = tbl_get(v, (tbl) xp, *sp++, nil),
  ApN(1, xp); }

Vm(thas) { return
  xp = tbl_get(v, (tbl) xp, *sp++, 0),
  ApN(1, xp ? T : nil); }

Vm(tlen) { return ApN(1, putnum(((tbl) xp)->len)); }

Vm(thas_f) { return
  fp->argc < 2 ? ApC(xary, putnum(2)) :
  !tblp(fp->argv[0]) ? ApC(xdom, xp) :
  (xp = tbl_get(v, (tbl) fp->argv[0], fp->argv[1], 0),
   ApC(ret, xp ? T : nil)); }

Vm(tset_f) { u1 _; return
  !fp->argc ? ApC(ret, xp) :
  !tblp(xp = fp->argv[0]) ? ApC(xdom, xp) :
  (CallOut(_ = tblss(v, 1, fp->argc)),
   _ ? ApC(ret, fp->argv[fp->argc-1]) :
       ApC(xoom, nil)); }

Vm(tbl_f) {
  ob x = fp->argc; return
    CallOut(x = (v->xp = (ob) mktbl(v)) && tblss(v, 0, x)),
    x ? ApC(ret, xp) : ApC(xoom, nil); }

Vm(tkeys_f) { ob x; return
  !fp->argc ? ApC(xary, putnum(1)) :
  !tblp(xp = fp->argv[0]) ? ApC(xdom, xp) :
  (CallOut(x = tbl_keys(v)), !x) ?
    ApC(xoom, xp) : ApC(ret, x); }

Vm(tlen_f) { return
  !fp->argc ? ApC(xary, putnum(1)) :
  !tblp(xp = fp->argv[0]) ? ApC(xdom, xp) :
  ApC(ret, putnum(((tbl) xp)->len)); }

Vm(tset) {
  ob x = *sp++; return
    CallOut(x = (ob) tbl_set(v, (tbl) xp, x, *sp)),
    x ? ApN(1, *sp++) : ApC(xoom, xp); }

// FIXME so bad :(
static ob tbl_del_s(la v, tbl y, ob key, ob val) {
  U b = tbl_idx(y->cap, hash(v, key));
  struct tbl_e
   *e = y->tab[b],
   prev = {0,0,e};
  for (struct tbl_e *l = &prev; l && l->next; l = l->next)
    if (eql(v, l->next->key, key)) {
      val = l->next->val;
      l->next = l->next->next;
      y->len--;
      break; }
  y->tab[b] = prev.next;
  return val; }

// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static tbl tbl_grow(la v, tbl t) {
  struct tbl_e **tab0, **tab1;
  U cap0 = t->cap,
    cap1 = cap0,
    load = tbl_load(t);
  while (load > 1) cap1 <<= 1, load >>= 1;
  if (cap0 == cap1) return t;

  with(t, tab1 = (struct tbl_e**) cells(v, cap1));
  if (!tab1) return 0;
  setw(tab1, 0, cap1);
  tab0 = t->tab;

  for (U i; cap0--;)
    for (struct tbl_e *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = tbl_idx(cap1, hash(v, e->key)),
      e->next = tab1[i],
      tab1[i] = e);

  t->cap = cap1;
  t->tab = tab1;
  return t; }

static tbl tbl_set_s(la v, tbl t, ob k, ob x) {
  U hc = hash(v, k);
  struct tbl_e *e = tbl_ent_hc(v, t, k, hc);
  if (e) return e->val = x, t;
  U i = tbl_idx(t->cap, hc);
  with(t, with(k, with(x, e = cells(v, Width(struct tbl_e)))));
  if (!e) return 0;
  e->key = k, e->val = x, e->next = t->tab[i];
  t->tab[i] = e;
  t->len++;
  return t; }

// get table keys
// XXX calling convention: table in v->xp
static ob tbl_keys(la v) {
  U len = ((tbl) v->xp)->len;
  two ks;
  ks = cells(v, Width(struct two) * len);
  if (!ks) return 0;
  ob r = nil;
  struct tbl_e **tab = ((tbl) v->xp)->tab;
  while (len) for (struct tbl_e *e = *tab++; e;
    two_ini(ks, e->key, r),
    r = (ob) ks++,
    e = e->next,
    len--);
  return r; }

// do a bunch of table assignments.
// XXX calling convention: table in v->xp
// FIXME gross!
static u1 tblss(la v, I i, I l) {
  u1 _ = true;
  while (_ && i <= l - 2)
    _ = tbl_set(v, (tbl) v->xp, v->fp->argv[i], v->fp->argv[i+1]),
    i += 2;
  return _; }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static u0 tbl_shrink(la v, tbl t) {
  struct tbl_e *e = NULL, *f, *g;
  U i = t->cap;

  // collect all entries
  while (i--) for (f = t->tab[i], t->tab[i] = 0; f;
    g = f->next, f->next = e, e = f, f = g);

  // shrink bucket array
  while (t->cap > 1 && !tbl_load(t)) t->cap >>= 1;

  // reinsert
  while (e)
    i = tbl_idx(t->cap, hash(v, e->key)),
    f = e->next,
    e->next = t->tab[i],
    t->tab[i] = e,
    e = f; }

Vm(ap_tbl) {
  u1 _; ob a = fp->argc; switch (a) {
    case 0: return ApC(ret, putnum(((tbl) ip)->len));
    case 1: return
      xp = tbl_get(v, (tbl) ip, fp->argv[0], nil),
      ApC(ret, xp);
    default: return
      xp = (ob) ip,
      CallOut(_ = tblss(v, 1, a)),
      _ ? ApC(ret, fp->argv[a-1]) : ApC(xoom, nil); } }

static u0 tx_tbl(la v, FILE* o, ob _) {
  fprintf(o, "#tbl:%ld/%ld", ((tbl)_)->len, ((tbl)_)->cap); }

static I hx_tbl(la v, ob _) {
  return ror(mix, 3 * sizeof(I) / 4); }

static struct tbl_e *cp_tbl_e(la v, struct tbl_e *src, ob *pool0, ob *top0) {
  if (!src) return src;
  struct tbl_e *dst = bump(v, Width(struct tbl_e));
  dst->next = cp_tbl_e(v, src->next, pool0, top0);
  dst->val = cp(v, src->val, pool0, top0);
  dst->key = cp(v, src->key, pool0, top0);
  return dst; }

static Gc(cp_tbl) {
  tbl src = (tbl) x;
  U i = src->cap;
  tbl dst = bump(v, Width(struct tbl) + i);
  src->data = (vm*) dst;
  ini_tbl(dst, src->len, i, (struct tbl_e**) (dst+1));
  while (i--) dst->tab[i] = cp_tbl_e(v, src->tab[i], pool0, top0);
  return (ob) dst; }


str str_ini(void *_, size_t len) {
  str s = _; return
    s->data = data, s->typ = &str_typ,
    s->len = len,
    s; }

static I hx_str(la v, ob _) {
  str s = (str) _;
  I h = 1;
  U words = s->len / sizeof(ob),
    bytes = s->len % sizeof(ob);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const I *ws = (I*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static Inline u1 escapep(char c) { return c == '\\' || c == '"'; }

static u0 tx_str(struct carrier *v, FILE *o, ob _) {
  str s = (str) _;
  U len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if (escapep(c = *text++)) putc('\\', o);
  putc('"', o); }

static Gc(cp_str) {
  str src = (str) x;
  return (ob) (src->data = (vm*)
    memcpy(bump(v, Width(struct str) + b2w(src->len)),
      src, sizeof(struct str) + src->len)); }

static u1 eq_str(struct carrier *v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len && !strncmp(a->text, b->text, a->len); }

static vm ap_str;

static const struct typ str_typ = {
  .does = ap_str,
  .emit = tx_str,
  .evac = cp_str,
  .hash = hx_str,
  .equi = eq_str, };

Vm(ap_str) {
  str s = (str) ip;
  fputsn(s->text, s->len, stdout);
  return ApC(ret, (ob) ip); }

// string instructions
Vm(slen_f) { return
  fp->argc == 0 ? ApC(xary, putnum(1)) :
  !strp(xp = fp->argv[0]) ? ApC(xdom, xp) :
  ApC(ret, putnum(((str) xp)->len)); }

Vm(sget_f) {
  if (fp->argc < 2) return ApC(xary, putnum(2));
  if (!strp(fp->argv[0])) return ApC(xdom, xp);
  str s = (str) fp->argv[0];
  I i = getnum(fp->argv[1]);
  xp = i < 0 || i >= s->len ? nil : putnum(s->text[i]);
  return ApC(ret, xp); }

Vm(scat_f) {
  U sum = 0, i = 0;
  for (U l = fp->argc; i < l;) {
    ob x = fp->argv[i++];
    Check(strp(x));
    sum += ((str)x)->len; }
  U words = Width(struct str) + b2w(sum);
  Have(words);
  str d = str_ini(hp, sum);
  hp += words;
  for (str x; i--;
    x = (str) fp->argv[i],
    sum -= x->len,
    memcpy(d->text+sum, x->text, x->len));
  return ApC(ret, (ob) d); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_f) {
  if (fp->argc < 2) return ApC(xary, putnum(2));
  if (!strp(fp->argv[0])) return ApC(xdom, xp);
  str src = (str) fp->argv[0];
  I lb = getnum(fp->argv[1]),
    ub = fp->argc > 2 ? getnum(fp->argv[2]) : INTPTR_MAX;
  lb = max(lb, 0);
  ub = min(ub, src->len);
  ub = max(ub, lb);
  U len = ub - lb,
    words = Width(struct str) + b2w(len);
  Have(words);
  str dst = str_ini(hp, len);
  hp += words;
  memcpy(dst->text, src->text + lb, len);
  return ApC(ret, (ob) dst); }

Vm(str_f) {
  U len = fp->argc,
    words = Width(struct str) + b2w(len);
  Have(words);
  str s = str_ini(hp, len);
  hp += words;
  while (len--) s->text[len] = getnum(fp->argv[len]);
  return ApC(ret, (ob) s); }


//symbols
//
static sym ini_anon(u0 *_, U code) {
  sym y = _;
  y->data = data;
  y->typ = &sym_typ;
  y->nom = 0;
  y->code = code;
  return y; }

static sym ini_sym(u0 *_, str nom, U code) {
  sym y = _;
  y->data = data;
  y->typ = &sym_typ;
  y->nom = nom;
  y->code = code;
  y->l = y->r = 0;
  return y; }

// FIXME this should probably change at some point.
// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle which seems to keep it
// from getting too bad. this is much more performant than a
// list & uses less memory than a hash table, but maybe we
// should use a table anyway.
//
// FIXME the caller must ensure Avail >= Width(struct sym)
// (because GC here would void the tree)
static sym intern(la v, sym *y, str b) {
  if (*y) {
    sym z = *y;
    str a = z->nom;
    int i = strncmp(a->text, b->text,
      a->len < b->len ? a->len : b->len);
    if (i == 0) {
      if (a->len == b->len) return z;
      i = a->len < b->len ? -1 : 1; }
    return intern(v, i < 0 ? &z->l : &z->r, b); }
  return *y = ini_sym(bump(v, Width(struct sym)), b,
    hash(v, putnum(hash(v, (ob) b)))); }

static Gc(cp_sym) {
  sym src = (sym) x;
  return (ob) (src->data = (vm*) (src->nom ?
    intern(v, &v->syms, (str) cp(v, (ob) src->nom, pool0, top0)) :
    ini_anon(bump(v, Width(struct sym) - 2), src->code))); }

static I hx_sym(la v, ob _) { return ((sym) _)->code; }

static u0 tx_sym(la v, FILE* o, ob _) {
  str s = ((sym) _)->nom;
  s ? fputsn(s->text, s->len, o) : fputs("#sym", o); }

Vm(ap_nop) { return ApC(ret, (ob) ip); }

static const struct typ sym_typ = {
  .does = ap_nop,
  .emit = tx_sym,
  .evac = cp_sym,
  .hash = hx_sym,
  .equi = neql, };

sym symof(la v, str s) {
  if (Avail < Width(struct sym)) {
    u1 _; with(s, _ = please(v, Width(struct sym)));
    if (!_) return 0; }
  return s ? intern(v, &v->syms, s) :
    ini_anon(bump(v, Width(struct sym) - 2), v->rand = lcprng(v->rand)); }

Vm(sym_f) {
  str i = fp->argc && strp(fp->argv[0]) ? (str) fp->argv[0] : 0;
  sym y; CallOut(y = symof(v, i));
  return y ? ApC(ret, (ob) y) : ApC(xoom, xp); }

Vm(ynom_f) {
  if (fp->argc && symp(fp->argv[0]))
    xp = (ob) ((sym) fp->argv[0])->nom,
    xp = xp ? xp : nil;
  return ApC(ret, xp); }

// function functions
//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|?|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
//
// two easy potential optimizations are:
// - add a tail pointer to the start of the function,
//   so GC can find the head quickly (since often we
//   won't have an internal pointer)
// - tag the tail/head pointers instead of using a null
//   sentinel (but then the C compiler would need to
//   align functions)

mo mo_ini(u0 *_, U len) {
  struct tl *t = (struct tl*) ((mo) _ + len);
  return t->null = NULL, t->head = _; }

// allocate a thread
mo mo_n(la v, U n) {
  mo k = cells(v, n + Width(struct tl));
  return k ? mo_ini(k, n) : k; }

// instructions for the internal compiler
// initialize a function
Vm(hom_f) {
  if (fp->argc && nump(fp->argv[0])) {
    U len = getnum(fp->argv[0]);
    Have(len + Width(struct tl));
    mo k = setw(mo_ini(hp, len), nil, len);
    hp += len + Width(struct tl);
    xp = (ob) (k + len); }
  return ApC(ret, xp); }

// trim a function after writing out code
Vm(hfin_f) {
  if (fp->argc) {
    ob x = fp->argv[0];
    if (homp(x) && G(x) != data)
      mo_tl((mo) x)->head = (mo) x,
      xp = x; }
  return ApC(ret, xp); }

// emit data
Vm(poke_f) {
  if (fp->argc) {
    U i = fp->argc - 1;
    if (homp(fp->argv[i])) {
      mo k = (mo) fp->argv[i];
      while (i--) G(--k) = (vm*) fp->argv[i];
      xp = (ob) k; } }
  return ApC(ret, xp); }

Vm(peek_f) {
  if (fp->argc && homp(fp->argv[0])) xp = (ob) G(fp->argv[0]);
  return ApC(ret, xp); }

// thread pointer arithmetic -- not bounds checked!
Vm(seek_f) {
  if (fp->argc >= 2 && homp(fp->argv[0]) && nump(fp->argv[1]))
    xp = (ob) ((mo) fp->argv[0] + getnum(fp->argv[1]));
  return ApC(ret, xp); }

// TODO maybe we could do this with closures instead?
Vm(data) { return ApC(((typ) GF(ip))->does, xp); }

// closure functions
//
// pop some things off the stack into an array.
Vm(take) {
  ob n = getnum((ob) GF(ip));
  Have(n + Width(struct tl));
  mo k = mo_ini(cpyw_r2l(hp, sp, n), n);
  hp += n + Width(struct tl);
  return ApC(ret, (ob) k); }

// set the closure for this frame
Vm(setclo) { return
  fp->clos = (ob*) GF(ip),
  ApY(G(FF(ip)), xp); }

// finalize function instance closure
Vm(genclo1) { return
  G(ip) = setclo,
  GF(ip) = (vm*) xp,
  ApY(ip, xp); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.

struct clo_env {
  mo cons;
  ob loc, *clo, argc, argv[]; };

Vm(genclo0) {
  struct clo_env *ec = (u0*) GF(ip);
  U adic = getnum(ec->argc);
  Have(Width(struct frame) + adic + 1);
  sf subd = fp; return
    G(ip) = genclo1,
    sp = (ob*) (fp = (sf) (sp - adic) - 1),
    cpyw_r2l(fp->argv, ec->argv, adic),
    fp->retp = ip,
    fp->subd = subd,
    fp->argc = adic,
    fp->clos = (ob*) ec->clo,
    *--sp = ec->loc,
    ApY(ec->cons, xp); }

// the next few functions create and store
// lexical environments.
Vm(enclose) {
  U thd_len = 3 + Width(struct tl),
    env_len = fp->argc + Width(struct tl) +
                         Width(struct clo_env);
  Have(env_len + thd_len);
  ob codeXcons = (ob) GF(ip); // pair of the compiled thread & closure constructor
  ob *block = hp;
  hp += env_len + thd_len;

  struct clo_env *env = (u0*)
    mo_ini(block, Width(struct clo_env) + fp->argc); // holds the closure environment & constructor
  env->cons = (mo) B(codeXcons);
     // TODO get closure out of stack frame; configure via xp
  env->loc = nilp(xp) ? xp : ((ob*)fp)[-1];
  env->clo = fp->clos;
  env->argc = putnum(fp->argc);
  cpyw_r2l(env->argv, fp->argv, fp->argc);

  mo thd = mo_ini(block + env_len, 3); // the thread that actually gets returned
  G(thd) = genclo0;
  GF(thd) = (vm*) env;
  G(FF(thd)) = (vm*) A(codeXcons);

  return ApN(2, (ob) thd); }

// these pass the locals array to encl in xp
// TODO do the same thing with the closure ptr
Vm(encl1) { return ApC(enclose, putnum(1)); }
// FIXME if there are no locals we don't need to defer closure construction!
Vm(encl0) { return ApC(enclose, putnum(0)); }

// try to get the name of a function
ob hnom(la v, mo x) {
  if (!livep(v, (ob) x)) return nil;
  vm *k = G(x);

  if (k == setclo || k == genclo0 || k == genclo1) // closure?
    return hnom(v, (mo) G(FF(x)));

  ob n = ((ob*) mo_tl(x))[-1];
  return homp(n) && livep(v, n) && G(n) == data ? n : nil; }

static mo ana(la, ob);

// bootstrap eval interpreter function
Vm(ev_f) {
  mo e = (mo) tbl_get(v, v->topl, (ob) v->lex.eval, 0);
  if (e && G(e) != ev_f) return ApY(e, xp);
  if (!fp->argc) return ApC(ret, xp);
  mo y; CallOut(y = ana(v, fp->argv[0]));
  return y ? ApY(y, xp) : ApC(xoom, xp); }


static NoInline enum status la_go(la v) {
  mo ip; sf fp; ob xp, *hp, *sp;
  return Unpack(), ApY(ip, xp); }

////
///  the thread compiler
//
// this is the most complicated part of the C code but it
// normally only gets used during initialization to bootstrap the
// self-hosted compiler.
//
// " compilation environments "
typedef struct env {
  ob arg, loc, clo, name, asig, s1, s2;
  struct env *par; } *env;
// if a function is not variadic its arity signature is
// n = number of required arguments; otherwise it is -n-1

typedef mo co(la, env*, size_t);

static mo
  p_pulbi(la, env*, size_t),
  p_pulbix(la, env*, size_t),
  p_mo_ini(la, env*, size_t),
  p_co_x(la, env*, size_t),
  p_co_def_bind(la, env*, size_t),
  co_x(la, env*, size_t, ob) NoInline,
  co_if(la, env*, size_t, ob) NoInline,
  co_ap(la, env*, size_t, ob, ob) NoInline,
  co_def(la, env*, size_t, ob) NoInline,
  co_fn(la, env*, size_t, ob) NoInline,
  mo_seq(la, env*, size_t, ob) NoInline,
  co_sym(la, env*, size_t, ob) NoInline,
  mo_mac(la, env*, size_t, ob, ob) NoInline,
  mo_l(la, env*, size_t, ob) NoInline,
  mo_i_x(la, env*, size_t, vm*, ob) NoInline;

static Inline mo pull_m(la v, env *e, size_t m) { return
  ((mo (*)(la, env*, size_t)) (*v->sp++))(v, e, m); }

static mo ana(la v, ob x) { return
  pushs(v, p_co_x, x, p_pulbi, ret, p_mo_ini, NULL) ?
    pull_m(v, 0, 0) :
    0; }

#define Co(nom,...)\
  static mo nom(la v, env *e, size_t m, ##__VA_ARGS__)

static u1 scan(la, env*, ob) NoInline;

// apply instruction pullbacks
static Inline mo pulbi(vm *i, mo k) { return k--, G(k) = i, k; }
static Inline mo pulbix(vm *i, ob x, mo k) {
  return pulbi(i, pulbi((vm*) x, k)); }

// supplemental list functions
//

// index of item in list (-1 if absent)
static NoInline I lidx(ob l, ob x) {
  for (I i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

// append to tail
static NoInline two snoc(la v, ob l, ob x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = (ob) snoc(v, B(l), x));
  return x ? pair(v, A(l), x) : 0; }

static NoInline ob rw_let_fn(la v, ob x) {
  mm(&x);
  for (two w; x && twop(A(x)); x =
    (w = snoc(v, BA(x), AB(x))) &&
    (w = pair(v, (ob) v->lex.lambda, (ob) w)) &&
    (w = pair(v, (ob) w, BB(x))) ?
      (ob) pair(v, AA(x), (ob) w) : 0);
  return um, x; }

static NoInline ob asign(la v, ob a, intptr_t i, ob *m) {
  ob x;
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == (ob) v->lex.splat)
    return *m = -i - 1, (ob) pair(v, A(a), nil);
  return with(a, x = asign(v, B(a), i + 1, m)),
    x ? (ob) pair(v, A(a), x) : 0; }

static Inline ob new_scope(la v, env *e, ob arg, ob nom) {
  env f;
  I asig = 0;
  with(nom,
    arg = asign(v, arg, 0, &asig),
    with(arg, f = (env) mo_n(v, Width(struct env))));
  if (f)
    f->arg = arg,
    f->name = nom,
    f->asig = putnum(asig),
    f->loc = f->clo = f->s1 = f->s2 = nil,
    f->par = e ? *e : (env) nil;
  return (ob) f; }

static NoInline int scan_def(la v, env *e, ob x) {
  int r;
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  with(x,
     r = scan_def(v, e, BB(x)),
     r = r != 1 ? r :
       !(x = rw_let_fn(v, x)) ||
       !((*e)->loc = (ob) pair(v, A(x), (*e)->loc)) ||
       !scan(v, e, AB(x)) ? -1 : 1);
  return r; }

static NoInline u1 scan(la v, env *e, ob x) {
  u1 _; return !twop(x) ||
    A(x) == (ob) v->lex.lambda ||
    A(x) == (ob) v->lex.quote ||
    (A(x) == (ob) v->lex.define &&
     scan_def(v, e, B(x)) != -1) ||
    (with(x, _ = scan(v, e, A(x))),
     _ && scan(v, e, B(x))); }

static NoInline ob linitp(la v, ob x, ob *d) {
  if (!twop(B(x))) return *d = x, nil;
  ob y; return
    with(x, y = linitp(v, B(x), d)),
    y ? (ob) pair(v, A(x), y) : 0; }

static Inline ob comp_body(la v, env *e, ob x) {
  I i;
  if (!pushs(v, p_co_x, x, p_pulbi, ret, p_mo_ini, NULL) ||
      !scan(v, e, v->sp[1]) ||
      !(x = (ob) pull_m(v, e, 4)))
    return 0;
  x = !(i = llen((*e)->loc)) ? x :
   (ob) pulbix(setloc, putnum(i), (mo) x);
  x = (i = getnum((*e)->asig)) > 0 ?
        (ob) pulbix(arity, putnum(i), (mo) x) :
      i < 0 ?
        (ob) pulbix(varg, putnum(-i-1), (mo) x) :
      x;
  return mo_tl((mo) x)->head = (mo) x,
    !twop((*e)->clo) ? x : (ob) pair(v, (*e)->clo, x); }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static ob co_fn_ltu(la v, env *e, ob n, ob l) {
  ob y = nil; return
    with(n, with(y, with(l,
      l = (l = twop(l) ? l : (ob) pair(v, l, nil)) &&
          (l = linitp(v, l, &y)) &&
          (n = (ob) pair(v, n, e ? (*e)->name : nil)) &&
          (n = new_scope(v, e, l, n)) ?
        comp_body(v, (env*) &n, A(y)) : 0))),
    l; }

static ob co_fn_clo(la v, env *e, ob vars, ob code) {
  U i = llen(vars);
  mm(&vars), mm(&code);

  vars = pushs(v, p_pulbix, take, putnum(i), p_mo_ini, NULL) ? vars : 0;
  while (vars && i--) vars =
    pushs(v, p_co_x, A(vars), p_pulbi, push, NULL) ? B(vars) : 0;
  vars = vars ? (ob) pull_m(v, e, 0) : vars;
  vars = vars ? (ob) pair(v, code, vars) : vars;
  return um, um, vars; }

Co(co_fn_enclose, ob x, mo k) { return
  with(k, x = co_fn_clo(v, e, A(x), B(x))),
  // FIXME no locals => no need to defer closure construction
  x ? pulbix(e && homp((*e)->loc) ? encl1 : encl0, x, k) : 0; }

Co(co_fn, ob x) {
  ob nom = *v->sp == (ob) p_co_def_bind ? v->sp[1] : nil;
  mo k; with(nom, with(x, k = pull_m(v, e, m+2)));
  if (!k) return 0;
  with(k, x = co_fn_ltu(v, e, nom, x));
  return !x ? 0 : G(x) == data ? co_fn_enclose(v, e, m, x, k) : pulbix(imm, x, k); }

Co(p_co_def_bind) {
  ob _ = *v->sp++;
  if (!e) return
    _ = (ob) pair(v, (ob) v->topl, _),
    _ ? mo_i_x(v, e, m, deftop, _) : 0;
  return mo_i_x(v, e, m, defsl1, putnum(lidx((*e)->loc, _))); }

static u1 co_def_r(la v, env *e, ob x) {
  u1 _; return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = co_def_r(v, e, BB(x))), _) &&
     pushs(v, p_co_x, AB(x), p_co_def_bind, A(x), NULL)); }

// syntactic sugar for define
static Inline u1 co_def_sugar(la v, two x) {
  ob _ = nil;
  with(_, x = (two) linitp(v, (ob) x, &_));
  return x &&
    (x = pair(v, (ob) x, _)) &&
    (x = pair(v, (ob) v->lex.begin, (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    (x = pair(v, (ob) v->lex.lambda, (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    pushs(v, p_co_x, x, NULL); }

Co(co_def, ob x) {
  if (!twop(B(x))) return mo_i_x(v, e, m, imm, nil);
  x = llen(B(x)) & 1 ?
    co_def_sugar(v, (two) x) :
    co_def_r(v, e, B(x));
  return x ? pull_m(v, e, m) : 0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
Co(co_if_pre) {
  ob x = (ob) pull_m(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s2) : x;
  return x ? (mo) A((*e)->s2 = x) : 0; }

// before generating a branch emit a jump to
// the top of stack 2
Co(co_if_pre_con) {
  mo k, x = pull_m(v, e, m + 2);
  return !x ? 0 : G(k = (mo) A((*e)->s2)) == ret ?
    pulbi(ret, x) : pulbix(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
Co(co_if_post_con) {
  ob x = (ob) pull_m(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s1) : x;
  return x ? (mo) A((*e)->s1 = x) : 0; }

// before generating an antecedent emit a branch to
// the top of stack 1
Co(co_if_pre_ant) {
  mo x = pull_m(v, e, m + 2);
  if (!x) return 0;
  x = pulbix(br1, A((*e)->s1), x);
  (*e)->s1 = B((*e)->s1);
  return x; }

static u1 co_if_loop(la v, env *e, ob x) {
  u1 _;
  x = twop(x) ? x : (ob) pair(v, nil, nil);
  if (!x) return false;
  if (!twop(B(x))) return
    pushs(v, p_co_x, A(x), co_if_pre_con, NULL);
  with(x,
    _ = pushs(v, co_if_post_con, p_co_x,
              AB(x), co_if_pre_con, NULL),
    _ = _ ? co_if_loop(v, e, BB(x)) : _);
  return _ ? pushs(v, p_co_x, A(x), co_if_pre_ant, NULL) : 0; }

Co(co_if, ob x) {
  u1 _;
  with(x, _ = pushs(v, co_if_pre, NULL));
  _ = _ && co_if_loop(v, e, x);
  if (!_) return 0;
  mo pf = pull_m(v, e, m);
  if (pf) (*e)->s2 = B((*e)->s2);
  return pf; }

Co(p_co_ap_call) {
  ob ary = *v->sp++;
  mo k = pull_m(v, e, m + 2);
  return k ? pulbix(G(k) == ret ? rec : call, ary, k) : 0; }

enum where { Arg, Loc, Clo, Here, Wait };

static NoInline ob co_sym_look(la v, env e, ob y) { return
  nilp((ob) e) ?
    (y = tbl_get(v, v->topl, y, 0)) ?
      (ob) pair(v, Here, y) :
      (ob) pair(v, Wait, (ob) v->topl) :
  lidx(e->loc, y) >= 0 ? (ob) pair(v, Loc, (ob) e) :
  lidx(e->arg, y) >= 0 ? (ob) pair(v, Arg, (ob) e) :
  lidx(e->clo, y) >= 0 ? (ob) pair(v, Clo, (ob) e) :
  co_sym_look(v, (env) e->par, y); }

Co(co_sym, ob x) {
  ob q;
  with(x, q = co_sym_look(v, e ? *e : (env) nil, x));
  if (!q) return 0;
  if (A(q) == Here) return mo_i_x(v, e, m, imm, B(q));
  if (A(q) == Wait) return
    (x = (ob) pair(v, B(q), x)) &&
    (with(x, q = (ob) pull_m(v, e, m + 2)), q) ?
      pulbix(late, x, (mo) q) : 0;

  if (B(q) == (ob) *e) {
    ob idx = putnum(lidx(((ob*)(*e))[A(q)], x));
    vm *i = A(q) == Arg ? argn : A(q) == Loc ? sl1n : clon;
    return mo_i_x(v, e, m, i, idx); }

  U y = llen((*e)->clo);
  if (!(q = (ob) snoc(v, (*e)->clo, x))) return 0;
  return (*e)->clo = q,
    mo_i_x(v, e, m, clon, putnum(y)); }

Co(co_x, ob x) { return
  symp(x) ? co_sym(v, e, m, x) :
  twop(x) ? mo_l(v, e, m, x) :
  mo_i_x(v, e, m, imm, x); }

Co(p_co_x) { return co_x(v, e, m, *v->sp++); }

Co(co_ap, ob f, ob args) {
  mm(&args);
  if (!pushs(v,
        p_co_x, f,
        p_pulbi, idmo,
        p_co_ap_call, putnum(llen(args)),
        NULL))
    return um, NULL;
  for (; twop(args); args = B(args))
    if (!pushs(v, p_co_x, A(args), p_pulbi, push, NULL))
      return um, NULL;
  return um, pull_m(v, e, m); }

static NoInline u1 seq_mo_loop(la v, env *e, ob x) {
  u1 _; return !twop(x) ||
    (with(x, _ = seq_mo_loop(v, e, B(x))),
     _ && pushs(v, p_co_x, A(x), NULL)); }

Co(mo_seq, ob x) { return
  x = twop(x) ? x : (ob) pair(v, x, nil),
  x && seq_mo_loop(v, e, x) ?
    pull_m(v, e, m) :
    0; }

static enum status la_call(la v, mo f, U n) { return
  v->ip = thd(v, imm, f, call, putnum(n), xok, NULL),
  !v->ip ? OomError : la_go(v); }

static enum status la_ap(la v, mo f, ob x) {
  mo k = thd(v, imm, x, push, imm, f, push, imm, ap_f, rec, putnum(2), ap_f, NULL);
  return !k ? OomError :
    (k[7].ap = (vm*) (k + 10),
     la_call(v, k, 0)); }

Co(mo_mac, ob mac, ob x) {
  enum status s;
  ob xp = v->xp;
  mo ip = v->ip;
  return
    with(xp, with(ip, s = la_ap(v, (mo) mac, x))),
    x = v->xp, v->xp = xp, v->ip = ip,
    la_perror(v, s),
    s == Ok ? co_x(v, e, m, x) : NULL; }

Co(mo_l, ob x) {
  ob a = A(x);
  if (symp(a)) {
    sym y = (sym) a;
    if (y == v->lex.quote) return
      mo_i_x(v, e, m, imm, twop(B(x)) ? AB(x) : B(x));
    if (y == v->lex.cond)   return co_if(v, e, m, B(x));
    if (y == v->lex.lambda) return co_fn(v, e, m, B(x));
    if (y == v->lex.define) return co_def(v, e, m, x);
    if (y == v->lex.begin)  return mo_seq(v, e, m, B(x)); }
  if ((a = tbl_get(v, v->macros, a, 0)))
    return mo_mac(v, e, m, a, B(x));
  return co_ap(v, e, m, A(x), B(x)); }

Co(p_pulbi) {
  vm *i = (vm*) *v->sp++;
  mo k = pull_m(v, e, m + 1);
  return k ? pulbi(i, k): 0; }

static mo mo_i_x(la v, env *e, U m, vm *i, ob x) {
  mo k; return
    with(x, k = pull_m(v, e, m + 2)),
    k ? pulbix(i, x, k) : 0; }

Co(p_pulbix) {
  vm *i = (vm*) *v->sp++;
  ob x = *v->sp++;
  return mo_i_x(v, e, m, i, x); }

Co(p_mo_ini) {
  mo k = mo_n(v, m + 1);
  if (k) setw(k, nil, m),
         G(k += m) = (vm*) (e ? (*e)->name : nil);
  return k; }

#include <getopt.h>
#include <unistd.h>

static u0 interact(la);
static FILE *boot_src(void);
static enum status
  main_process(u1, u1, char**),
  source(struct carrier*, FILE*);

int main(int ac, char **av) {
  static const char *help =
    "usage: %s [options] [scripts]\n"
    "with no arguments, interact\n"
    "options:\n"
    "  -h show this message\n"
    "  -i interact\n"
    "  -_ don't bootstrap\n";
  for (u1 boot = true, repl = ac == 1 && isatty(STDIN_FILENO);;)
    switch (getopt(ac, av, "hi_")) {
      default: return EXIT_FAILURE;
      case 'i': repl = true; continue;
      case '_': boot = false; continue;
      case 'h': fprintf(stdout, help, *av); continue;
      case -1: return ac == optind && !repl ? EXIT_SUCCESS :
        main_process(boot, repl, av + optind); } }

static int main_process(u1 boot, u1 repl, char **av) {
  struct carrier V;
  enum status s = la_ini(&V);

  if (s == Ok && boot) s = source(&V, boot_src());
  while (s == Ok && *av) s = source(&V, fopen(*av++, "r"));
  if (s == Ok && repl) interact(&V);

  return la_perror(&V, s),
         la_fin(&V),
         s == Eof ? Ok : s; }

static NoInline enum status la_ev_x(la v, ob x) {
  mo k = thd(v, imm, x, push, imm, ev_f, rec, putnum(1), ev_f, NULL);
  if (!k) return OomError;
  k[4].ap = (vm*) (k + 7);
  return la_call(v, k, 0); }

static enum status source(struct carrier *v, FILE *in) {
  if (!in) return SystemError;
  for (enum status r;;)
    if ((r = receive(v, in)) != Ok ||
        (r = la_ev_x(v, v->xp)) != Ok)
      return fclose(in), r == Eof ? Ok : r; }

#define NOM "sen"
#define SUFF "la"
static FILE *boot_src(void) {
  FILE *b; char *home, buf[256]; return
  (home = getenv("HOME")) &&
  snprintf(buf, sizeof(buf), "%s/.local/lib/" NOM "/boot." SUFF, home) < sizeof(buf) &&
  ((b = fopen(buf, "r")) ||
   (b = fopen("/usr/local/lib/" NOM "/boot." SUFF, "r")) ||
   (b = fopen("/usr/lib/" NOM "/boot." SUFF, "r"))) ? b :
   fopen("/lib/" NOM "/boot." SUFF, "r"); }

static u0 interact(la v) {
  enum status s;
  while ((s = receive(v, stdin)) == Ok &&
         (s = la_ev_x(v, v->xp)) != Eof)
    if (s == Ok) transmit(v, stdout, v->xp),
                 putc('\n', stdout);
    else la_perror(v, s),
         unwind(v); }
