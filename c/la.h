#ifndef _em_h
#define _em_h
#include <stdint.h>
#include <stdbool.h>
// FIXME don't use stdio
#include <stdio.h>

// thanks !!

typedef void u0;
typedef bool u1;
typedef uintptr_t N;
typedef intptr_t ob, Z;
typedef struct mo *mo;
typedef struct ps *em, *la, *ph, *ps;
typedef struct fr *fr, *co, *ar;
#define Ll(n, ...)\
  ob n(ps v, ob xp, mo ip, ob *hp, ob *sp, ar fp)
typedef Ll(host);
#define Vm Ll
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

#define S static
#define K const
#define I Inline

typedef FILE *fd;
typedef char ch;
#define Gc(n) ob n(ph v, ob x, Z len0, ob *pool0)
#define Hash(n) N n(la v, ob x)
#define Show(n) void n(la v, ob x, fd o)
typedef Hash(hasher);
typedef Gc(copier);
typedef Show(writer);
typedef void finalizer(la, ob);

typedef struct ext {
  ob name;
  ob (*copy)(la, ob, Z, ob*);
  N (*hash)(la, ob);
  void (*show)(la, ob, fd);
  bool (*equal)(ob, ob);
} *ext;

typedef struct str { ext ext; Z len; char text[]; } *str;
typedef struct sym { ob nom, code, l, r; } *sym;
typedef struct two { ob a, b; } *two;
typedef struct mm { ob *it; struct mm *et; } *mm;
typedef struct tbl { ob *tab, len, cap; } *tbl;

struct fr { ob clos, retp, subd, argc, argv[]; };
struct mo { host *ll; };

// language symbols
enum lex {
  Def, Cond, Lamb, Quote, Seq, Splat,
  Eval, Apply, LexN };

struct ps {
  // vm state -- kept in CPU registers most of the time
  mo ip; // current thread
  ar fp; // top of control stack
  ob xp, // free register
     *hp, // top of heap
     *sp; // top of data stack

  // memory state
  mm keep; // list of C stack addresses to copy on gc
  ob fins; // finalizers
  Z t0, // gc timestamp, governs len
    len, // memory pool size
    *pool; // memory pool

  // other runtime state
  ob wns, // working namespace -- a stack of dicts
     sns, // system namespace -- a dict of dicts
     syms, // internal symbols
     rand, // random seed
     lex[LexN]; }; // grammar symbols

u0 emit(la, ob, fd),
   la1(la);

u1 eql(ob, ob),
   please(la, N);

N llen(ob),
  hash(la, ob);

Z lcprng(Z),
  lidx(ob, ob);

la la0(void);

mo ana(la, ob, ob);

ob
   refer(la, ob),
   string(la, K char*),
   intern(la, ob),
   interns(la, K char*),
   table(la),
   tbl_set(la, ob, ob, ob),
   tbl_get(la, ob, ob),
   pair(la, ob, ob),
   parq(la, fd),
   parse(la, fd),
   hnom(la, ob),
   linitp(la, ob, ob*),
   snoc(la, ob, ob),
   sskc(la, ob*, ob),
   err(la, ob, K char*, ...);

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
#define mm(r) ((v->keep=&((struct mm){(r),v->keep})))
#define um (v->keep=v->keep->et)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Width(t) b2w(sizeof(struct t))

#define nilp(_) ((_)==nil)

#define F(_) ((mo)(_)+1)
#define G(_) (((mo)(_))->ll)

#define putstr(_) ((ob)(_)+Str)
#define getnum getZ
#define putnum putZ
#define puthom putM
#define gethom getM
#define getZ(_) ((ob)(_)>>TagBits)
#define putZ(_) (((ob)(_)<<TagBits)+Num)
#define getstr(_) ((str)((_)-Str))
#define putM(_) ((ob)(_))
#define getM(_) ((mo)(_))
#define getsym getY
#define putsym putY
#define getY(_) ((sym)((_)-Sym))
#define putY(_) ((ob)(_)+Sym)
#define gettbl(_) ((tbl)((_)-Tbl))
#define puttbl(_) ((ob)(_)+Tbl)
#define gettwo getW
#define puttwo putW
#define getW(_) ((two)((_)-Two))
#define putW(_) ((ob)(_)+Two)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

host gc NoInline,
     dom_err NoInline,
     oom_err NoInline,
     ary_err NoInline;

S I enum class Q(ob _) { return _ & TagMask; }

S I u1 nump(ob _) { return Q(_) == Num; }
S I u1 strp(ob _) { return Q(_) == Str; }
S I u1 symp(ob _) { return Q(_) == Sym; }
S I u1 twop(ob _) { return Q(_) == Two; }
S I u1 tblp(ob _) { return Q(_) == Tbl; }
S I u1 homp(ob _) { return Q(_) == Hom; }

S I mo button(mo k) {
  while (G(k)) k = F(k);
  return k; }

S I N b2w(N b) {
  return b / sizeof(ob) + (b % sizeof(ob) && 1); }

S I ext extt(ob _) { return (ext) (_ & ~ TagMask); }

S I u0 setw(u0 *x, Z i, N l) {
  for (Z *d = x; l--; *d++ = i); }

S I u0 cpyw(u0 *x, K u0 *y, N l) {
  Z *d = x;
  K Z *s = y;
  while (l--) *d++ = *s++; }

S I u0 rcpyw(u0 *x, K u0 *y, N l) {
  Z *d = (ob*) x + (l - 1);
  K Z *s = (K ob*) y + (l - 1);
  while (l--) *d-- = *s--; }

// unchecked allocator -- make sure there's enough memory!
S I u0 *bump(la v, Z n) {
  u0 *x = v->hp;
  return v->hp += n, x; }

S I u0 *cells(la v, N n) {
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

#define ninl(x, _) ll x NoInline;
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
#define Ary(n) if (!HasArgs(n)) return ApC(ary_err, putZ(n))
#define IsA(x, t) (Q((x))==t)
#define Typ(x,t) if (!IsA((x), (t))) return ApC(dom_err, xp)
#define Get(n) ApC((v->xp=n, gc), xp)
#define Have1() if (hp == sp) return Get(1)
#define Have(n) if (sp - hp < n) return Get(n)
#define TypeCheck Typ
#define Arity Ary

#define Tc TypeCheck
#define CheckType TypeCheck


#define R(x) ((ob*)(x))
#define T putnum(-1)

#define LeftParen '('
#define RightParen ')'
#define EndOfFile  EOF
#define SingleQuote '\''
#define DoubleQuote '"'
#define NumeralSign '#'
#define Semicolon ';'
#define Space ' '
#define Tab '\t'
#define Newline '\n'
#define Backslash '\\'
#define Plus '+'
#define Minus '-'
#define Zero '0'
#define Radix2 'b'
#define Radix8 'o'
#define Radix10 'd'
#define Radix12 'z'
#define Radix16 'x'

// XXX FIXME XXX
_Static_assert(sizeof(intptr_t) == 8, "64bit");
_Static_assert(-1 == -1 >> 1, "signed >>");

#define Op(n, nom, x) Ll(nom) { xp = (x); return ApN(n, xp); }
#define OP1(nom, x) Op(1, nom, x)
#define BINOP(nom, xpn) Vm(nom) { xp = (xpn); return ApN(1, xp); }
#endif
