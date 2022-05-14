#ifndef _em_h
#define _em_h
#include <stdint.h>
#include <stdbool.h>
// FIXME don't use stdio
#include <stdio.h>

// thanks !!

// XXX FIXME XXX
_Static_assert(sizeof(intptr_t) == 8, "64bit");
_Static_assert(-1 == -1 >> 1, "signed >>");

typedef intptr_t ob, Z;
typedef struct mo *mo, *yo;
typedef struct em *em;
typedef struct fr *fr;
#define Ll(n, ...)\
  ob n(em v, ob xp, mo ip, ob *hp, ob *sp, fr fp)
typedef Ll(ll);
typedef ll vm;

// FIXME 2bit
#define TagBits 3
#define TagMask ((1<<TagBits)-1)
enum class { Hom, Num, Two, Str, Sym, Tbl, };
#define NomHom "hom"
#define NomNum "num"
#define NomTwo "two"
#define NomStr "str"
#define NomSym "sym"
#define NomTbl "tbl"

#define Gc(n) ob n(em v, ob x, intptr_t len0, ob*pool0)
#define Hash(n) uintptr_t n(em v, ob x)
#define Show(n) void n(em v, ob x, FILE *o)
typedef Hash(hasher);
typedef Gc(copier);
typedef Show(writer);

typedef struct ext {
  ob name;
  copier *copy;
  hasher *hash;
  writer *show; } *ext;

typedef struct str { ext ext; Z len; char text[]; } *str;
typedef struct sym { ob nom, code, l, r; } *sym;
typedef struct two { ob a, b; } *two;
typedef struct mm { ob *it; struct mm *et; } *mm;
typedef struct tbl { ob *tab, len, cap; } *tbl;

struct fr { ob clos, retp, subd, argc, argv[]; };
struct mo { ll *ll; };

// FIXME indices into a thread-global table of constants
enum { Def, Cond, Lamb, Quote, Seq, Splat,
       Topl, Eval, Apply, NGlobs };

struct em {
  mo ip; ob xp, *hp, *sp; fr fp;
  mm mm; ob syms, glob[NGlobs];
  intptr_t rand, t0, len, *pool; };

hasher hash;
void *cells(em, uintptr_t), emit(em, ob, FILE*);
bool eql(ob, ob), please(em, uintptr_t), pushs(em, ...);
mo ana(em, ob);
ob lookup(em, ob), cwm(em);
ob string(em, const char*),
   intern(em, ob),
   table(em),
   tbl_set(em, ob, ob, ob),
   tbl_get(em, ob, ob),
   pair(em, ob, ob),
   parse(em, FILE*),
   homnom(em, ob),
   err(em, ob, const char*, ...);

const char *tnom(enum class);

#define N0 putnum(0)
#define nil N0
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define A(o) gettwo(o)->a
#define B(o) gettwo(o)->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define Avail (v->sp-v->hp)
#define mm(r) ((v->mm=&((struct mm){(r),v->mm})))
#define um (v->mm=v->mm->et)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Width(t) b2w(sizeof(struct t))
#define Push(...) pushs(v, __VA_ARGS__, (ob) 0)

#define Q(_) ((enum class)((_)&TagMask))

#define nilp(_) ((_)==nil)

#define F(_) ((mo)(_)+1)
#define G(_) (((mo)(_))->ll)

#define nump(_) (Q(_)==Num)
#define strp(_) (Q(_)==Str)
#define homp(_) (Q(_)==Hom)
#define symp(_) (Q(_)==Sym)
#define tblp(_) (Q(_)==Tbl)
#define twop(_) (Q(_)==Two)
#define putstr(_) ((ob)(_)+Str)
#define getnum(_) ((ob)(_)>>3)
#define putnum(_) (((ob)(_)<<3)+Num)
#define getstr(_) ((str)((_)-Str))
#define puthom(_) ((ob)(_))
#define gethom(_) ((mo)(_))
#define getsym(_) ((sym)((_)-Sym))
#define putsym(_) ((ob)(_)+Sym)
#define gettbl(_) ((tbl)((_)-Tbl))
#define puttbl(_) ((ob)(_)+Tbl)
#define gettwo(_) ((two)((_)-Two))
#define puttwo(_) ((ob)(_)+Two)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

static Inline mo button(mo k) {
  while (G(k)) k = F(k);
  return k; }

static Inline uintptr_t llen(ob l) {
  uintptr_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

static Inline ob interns(em v, const char *s) {
  ob _ = string(v, s);
  return !_ ? 0 : intern(v, _); }

static Inline uintptr_t b2w(uintptr_t b) {
  return b / sizeof(ob) + (b % sizeof(ob) && 1); }

static Inline ext extt(ob _) {
  return (ext) (_ & ~ TagMask); }

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
 _(gsym_u, "sym") _(cwm_u, "cwm")\
 _(par_u, "read") _(sar_u, ">>") _(sal_u, "<<") _(band_u, "&")\
 _(bor_u, "|") _(bxor_u, "^")\
 _(add_u, "+") _(hom_u, "hom")\
 _(sub_u, "-") _(mul_u, "*") _(div_u, "/") _(mod_u, "%")\
 _(lt_u, "<") _(lteq_u, "<=") _(eq_u, "=") _(gteq_u, ">=")\
 _(gt_u, ">") _(car_u, "A") _(cdr_u, "B") _(cons_u, "X")\
 _(strg, "sget") _(gsym_u, "ssym") _(strmk, "str")\
 _(strl, "slen") _(strs, "ssub")   _(strconc, "scat")\
 _(tbll, "tlen") _(tblmk, "tbl") _(tblg, "tget")\
 _(tblc, "thas") _(tbls, "tset") _(tbld, "tdel")\
 _(tblks, "tkeys") _(hseek_u, "hseek") _(fail, "fail")\
 _(putc_u, "putc") _(ystr_u, "ystr")\
 _(hnom_u, "hnom")\
 _(emx_u, "emx") _(emi_u, "emi") _(show_u, ".") _(ev_u, "ev")\
 _(ap_u, "ap") _(hgeti_u, "hgeti")\
 _(hfin_u, "hfin") _(hgetx_u, "hgetx") _(twop_u, "twop")\
 _(nump_u, "nump") _(homp_u, "homp") _(tblp_u, "tblp")\
 _(symp_u, "symp") _(strp_u, "strp")\
 _(nilp_u, "nilp") _(rnd_u, "rand")

#define ninl(x, _) vm x;
insts(ninl)
#undef ninl

vm gc, type_error, ary_error, div_error;

// " the interpreter "
#define Vm Ll
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
#define CallC(...) (Pack(), (__VA_ARGS__), Unpack())

// FIXME confusing premature optimization
#define Locs ((ob*)fp)[-1]
// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer. if a function has
// locals, this will have been initialized before they are
// referenced.

#define ApN(n, x) ApY(ip+(n), (x))
#define ApC(f, x) (f)(v, (x), ip, hp, sp, fp)
#define ApY(f, x) (ip = (mo) (f), ApC(ip->ll, (x)))

#define HasArgs(n) (putnum(n) <= fp->argc)
#define ArityError(n) ApC(ary_error, putnum(n))
#define Arity(n) if (!HasArgs(n)) return ArityError(n)
#define IsA(x, t) (Q((x))==t)
#define TypeError(x,t) ApC(type_error, x)
#define TypeCheck(x,t) if (!IsA((x), (t))) return TypeError((x), (t))
#define Get(n) ApC((v->xp=n, gc), xp)
#define Have1() if (hp == sp) return Get(1)
#define Have(n) if (sp - hp < n) return Get(n)

#define Tc TypeCheck
#define CheckType TypeCheck

static Inline void setw(void *x, intptr_t i, uintptr_t l) {
  for (intptr_t *d = x; l--; *d++ = i); }

static Inline void cpyw(void *x, const void *y, uintptr_t l) {
  intptr_t *d = x;
  const intptr_t *s = y;
  while (l--) *d++ = *s++; }

static Inline void rcpyw(void *x, const void *y, uintptr_t l) {
  intptr_t *d = (ob*) x + (l - 1);
  const intptr_t *s = (const ob*) y + (l - 1);
  while (l--) *d-- = *s--; }

static Inline intptr_t lcprng(intptr_t s) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }
#define R(x) ((ob*)(x))
#endif
