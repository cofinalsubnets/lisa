#ifndef _la_vm_h
#define _la_vm_h

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

// these are vm functions used by C but not lisp.
#define cfns(_) _(gc) _(xdom) _(xoom) _(xary)
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
 _(sl1n) _(sl10) _(sl11) _(sl12) _(sl13)\
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
 _(poke)\
 _(br1) _(br0) _(bre) _(brn)\
 _(brl) _(brle) _(brge) _(brl2)\
 _(brle2) _(brg2) _(brg)\
 _(push)

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

#define Vm(n) enum la_status n(la v, ob xp, mo ip, ob *hp, ob *sp, sf fp)

#endif
