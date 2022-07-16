#ifndef _em_h
#define _em_h
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h> // FIXME
#include <stdlib.h>

// thanks !!

typedef void u0;
typedef bool u1;
typedef uintptr_t N;
typedef intptr_t ob, Z;
typedef struct dt *mo, *dt;
typedef struct pt *la, *ph, *ps, *pt;
typedef struct fr *fr, *co, *ar;
#define Dt(n, ...)\
  ob n(pt v, ob xp, dt ip, ob *hp, ob *sp, fr fp)
typedef Dt(host);
#define Vm Ll
#define Ll Dt
typedef host vm, ll, go;

// FIXME 2bit
#define TagBits 3
#define TagMask ((1<<TagBits)-1)
enum class { Hom, Num, Two, Str, Sym, Tbl, };
#define NomHom "hom"
#define NomNum "num"
#define NomTwo "two"
// FIXME ext
#define NomTbl "tbl"
// FIXME principled reason to separate sym & str?
#define NomStr "str"
#define NomSym "sym"

typedef FILE *fd;

typedef struct str { ob ext; intptr_t len; char text[]; } *str;
typedef struct sym { ob nom, code, l, r; } *sym;
typedef struct two { ob a, b; } *two;
typedef struct mm { ob *it; struct mm *et; } *mm;
typedef struct tbl { ob *tab, len, cap; } *tbl;

struct fr { ob clos, retp, subd, argc, argv[]; };
struct dt { host *ll; };

// language symbols
enum lex {
  Def, Cond, Lamb, Quote, Seq, Splat,
  Eval, Apply, LexN };

struct pt {
  // vm state -- kept in CPU registers most of the time
  mo ip; // current thread
  ar fp; // top of control stack
  ob xp, // free register
     *hp, // top of heap
     *sp; // top of data stack

  // memory state
  mm keep; // list of C stack addresses to copy on gc
  Z t0, // gc timestamp, governs len
    len, // memory pool size
    *pool; // memory pool

  // other runtime state
  ob wns, // working namespace -- a stack of dicts
     sns, // system namespace -- a dict of dicts
     syms, // internal symbols
     rand, // random seed
     lex[LexN]; }; // grammar symbols

void la_fin(pt), tx(pt, FILE*, ob);
bool please(pt, uintptr_t), eql(ob, ob);
uintptr_t llen(ob), hash(pt, ob);
intptr_t lidx(ob, ob), lcprng(intptr_t);

pt la_ini(void);
mo ana(pt, ob, ob);
ob string(pt, const char*),
   intern(pt, ob),
   table(pt),
   refer(pt, ob),
   tbl_set(pt, ob, ob, ob),
   tbl_get(pt, ob, ob),
   pair(pt, ob, ob),
   rxq(pt, FILE*),
   rx(pt, FILE*),
   hnom(pt, ob),
   sskc(pt, ob*, ob),
   err(pt, ob, const char*, ...);

#define N0 putnum(0)
#define nil N0
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define A(o) get2(o)->a
#define B(o) get2(o)->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define Avail (v->sp-v->hp)
#define mm(r) ((v->keep=&((struct mm){(r),v->keep})))
#define um (v->keep=v->keep->et)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Width(t) b2w(sizeof(struct t))

#define nilp(_) ((_)==nil)

#define F(_) ((dt)(_)+1)
#define G(_) (((dt)(_))->ll)

#define putstr(_) ((ob)(_)+Str)
#define getnum getZ
#define putnum putZ
#define getZ(_) ((ob)(_)>>TagBits)
#define putZ(_) (((ob)(_)<<TagBits)+Num)
#define getstr(_) ((str)((_)-Str))
#define puthom(_) ((ob)(_))
#define gethom(_) ((mo)(_))
#define getsym getY
#define putsym putY
#define getY(_) ((sym)((_)-Sym))
#define putY(_) ((ob)(_)+Sym)
#define gettbl(_) ((tbl)((_)-Tbl))
#define puttbl(_) ((ob)(_)+Tbl)
#define get2(_) ((two)((_)-Two))
#define put2(_) ((ob)(_)+Two)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

host
  gc NoInline,
  dom_err NoInline,
  oom_err NoInline,
  ary_err NoInline,
  clos NoInline,
  clos0 NoInline,
  clos1 NoInline;

static Inline enum class Q(ob _) { return _ & TagMask; }

static Inline bool nump(ob _) { return Q(_) == Num; }
static Inline bool strp(ob _) { return Q(_) == Str; }
static Inline bool symp(ob _) { return Q(_) == Sym; }
static Inline bool twop(ob _) { return Q(_) == Two; }
static Inline bool tblp(ob _) { return Q(_) == Tbl; }
static Inline bool homp(ob _) { return Q(_) == Hom; }

static Inline mo button(mo k) {
  while (G(k)) k = F(k);
  return k; }

static Inline uintptr_t b2w(uintptr_t b) {
  uintptr_t quot = b / sizeof(ob),
            rem = b % sizeof(ob);
  return rem ? quot + 1 : quot; }

static Inline void setw(void *x, intptr_t i, uintptr_t l) {
  for (intptr_t *d = x; l--; *d++ = i); }

static Inline void cpyw(void *x, const void *y, uintptr_t l) {
  intptr_t *d = x;
  const intptr_t *s = y;
  while (l--) *d++ = *s++; }

static Inline void rcpyw(void *x, const void *y, uintptr_t l) {
  intptr_t *d = (ob*) x + (l - 1);
  const intptr_t *s = (const intptr_t*) y + (l - 1);
  while (l--) *d-- = *s--; }

// unchecked allocator -- make sure there's enough memory!
static Inline void *bump(pt v, intptr_t n) {
  void *x = v->hp;
  v->hp += n;
  return x; }

static Inline void *cells(pt v, uintptr_t n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

#define insts(_)\
 _(tget, 0) _(tset, 0) _(thas, 0) _(tlen, 0) _(arity, 0)\
 _(idZ, 0) _(idH, 0) _(id2, 0) _(idT, 0) _(imm, 0)\
 _(arg, 0) _(clo, 0) _(loc, 0) _(take, 0) _(locals, 0)\
 _(loc_, 0) _(encll, 0) _(encln, 0) _(ret, 0)\
 _(jump, 0) _(branch, 0) _(barnch, 0) _(call, 0) _(rec, 0)\
 _(rslv, 0) _(sar, 0) _(sal, 0) _(band, 0) _(bor, 0)\
 _(bxor, 0) _(tbind, 0) _(push, 0) _(add, 0) _(sub, 0)\
 _(mul, 0) _(dqv, 0) _(mod, 0) _(neg, 0) _(lt, 0)\
 _(lteq, 0) _(eq, 0) _(gteq, 0) _(gt, 0) _(twopp, 0)\
 _(numpp, 0) _(nilpp, 0) _(strpp, 0) _(tblpp, 0) _(sympp, 0)\
 _(hompp, 0) _(car, 0) _(cdr, 0) _(cons, 0)\
 _(one, 0) _(zero, 0) _(arg0, 0) _(arg1, 0)\
 _(loc0, 0) _(loc1, 0) _(clo0, 0) _(clo1, 0)\
 _(brlt, 0) _(brlteq, 0) _(breq, 0) _(brgteq, 0) _(brlt2, 0)\
 _(brlteq2, 0) _(brgt2, 0) _(brgt, 0) _(brne, 0)\
 _(dupl, 0) _(emi, 0) _(emx, 0) _(vararg, 0)\
 _(sym_u, NomSym) _(cwm_u, "cwm")\
 _(sar_u, ">>") _(sal_u, "<<") _(band_u, "&") _(bnot_u, "!")\
 _(bor_u, "|") _(bxor_u, "^") _(add_u, "+") _(hom_u, NomHom)\
 _(sub_u, "-") _(mul_u, "*") _(div_u, "/") _(mod_u, "%")\
 _(lt_u, "<") _(lteq_u, "<=") _(eq_u, "=") _(gteq_u, ">=")\
 _(gt_u, ">") _(car_u, "A") _(cdr_u, "B") _(cons_u, "X")\
 _(strg, "sget") _(strmk, NomStr)\
 _(strl, "slen") _(strs, "ssub")   _(strconc, "scat")\
 _(tbll, "tlen") _(tblmk, "tbl") _(tblg, "tget")\
 _(tblc, "thas") _(tbls, "tset") _(tbld, "tdel")\
 _(tblks, "tkeys") _(seek_u, "seek") _(dom_err, "fail")\
 _(putc_u, "putc") _(ystr_u, "ystr") _(hnom_u, "hnom")\
 _(emx_u, "emx") _(emi_u, "emi") _(show_u, ".") _(ev_u, "ev")\
 _(ap_u, "ap") _(peeki_u, "peeki")\
 _(hfin_u, "hfin") _(peekx_u, "peekx") _(twop_u, "twop")\
 _(nump_u, "nump") _(homp_u, "homp") _(tblp_u, "tblp")\
 _(symp_u, "symp") _(strp_u, "strp")\
 _(nilp_u, "nilp") _(rnd_u, "rand")

#define ninl(x, _) host x NoInline;
insts(ninl)
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
#define Out(...) (Pack(),(__VA_ARGS__),Unpack())

// FIXME confusing premature optimization
#define Locs ((ob**)fp)[-1]
#define Clos ((ob*)fp->clos)
// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer. if a function has
// locals, this will have been initialized before they are
// referenced.

#define ApN(n, x) ApY(ip+(n), (x))
#define ApC(f, x) (f)(v, (x), ip, hp, sp, fp)
#define ApY(f, x) (ip = (dt) (f), ApC(ip->ll, (x)))

#define HasArgs(n) (putnum(n) <= fp->argc)
#define ArityCheck(n) if (!HasArgs(n)) return ApC(ary_err, putZ(n))
#define Ary ArityCheck
#define IsA(t, x) (t==Q((x)))
#define TypeCheck(x,t) if (!IsA((t),(x))) return ApC(dom_err, xp)
#define Pray(n) ApC((v->xp=n, gc), xp)
#define Hope (sp - hp)
#define Slack Hope
#define Have1() if (!Hope) return Pray(1)
#define Have(n) if (Hope < n) return Pray(n)

#define ptr(x) ((ob*)(x))
#define R ptr
#define T putZ(-1)

#define LeftParen '('
#define RightParen ')'
#define SingleQuote '\''
#define Backslash '\\'
#define DoubleQuote '"'
#define Newline '\n'
#define Space ' '

// XXX FIXME XXX
_Static_assert(sizeof(intptr_t) == 8, "64bit");
_Static_assert(-1 == -1 >> 1, "signed >>");

#define Op(n, nom, x) Ll(nom) { xp = (x); return ApN(n, xp); }
#define OP1(nom, x) Op(1, nom, x)
#define BINOP(nom, xpn) Vm(nom) { xp = (xpn); return ApN(1, xp); }
#endif
