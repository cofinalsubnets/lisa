#define ninl(x, ...) vm x NoInline;

// these are vm functions used by C but not lisp.
#define cfns(_)\
  _(gc) _(dom_err) _(oom_err) _(ary_err)\
  _(clos) _(clos0) _(clos1) _(do_id) _(yield)
cfns(ninl)
#undef cfns

// used by the compiler but not exposed as primitives
#define i_internals(_)\
 _(tget) _(tset) _(thas) _(tlen) _(arity) _(idZ) _(idH)\
 _(id2) _(idT) _(imm) _(arg) _(clo) _(loc) _(take) _(locals)\
 _(loc_) _(encll) _(encln) _(ret) _(jump) _(branch) _(barnch)\
 _(call) _(rec) _(late) _(sar) _(sal) _(band) _(bor)\
 _(bxor) _(tbind) _(push) _(add) _(sub) _(mul) _(dqv) _(mod)\
 _(neg) _(lt) _(lteq) _(eq) _(gteq) _(gt) _(twopp) _(numpp)\
 _(nilpp) _(strpp) _(tblpp) _(sympp) _(hompp) _(car) _(cdr)\
 _(cons) _(one) _(zero) _(arg0) _(arg1) _(loc0) _(loc1) _(disp)\
 _(clo0) _(clo1) _(brlt) _(brlteq) _(breq) _(brgteq) _(brlt2)\
 _(brlteq2) _(brgt2) _(brgt) _(brne) _(dupl) _(emi) _(emx) _(varg)

i_internals(ninl)

// primitive functions
// ev must be the first item in this list!
#define i_primitives(_) _(ev_u, "ev")\
 _(rx_u, "rx") _(nilp_u, "nilp") _(rnd_u, "rand")\
 _(sym_u, "sym") _(sar_u, ">>") _(sal_u, "<<")\
 _(band_u, "&") _(bnot_u, "!") _(bor_u, "|") _(bxor_u, "^")\
 _(add_u, "+") _(hom_u, "hom") _(sub_u, "-") _(mul_u, "*")\
 _(div_u, "/") _(mod_u, "%") _(lt_u, "<") _(lteq_u, "<=")\
 _(eq_u, "=") _(gteq_u, ">=") _(gt_u, ">") _(car_u, "A")\
 _(cdr_u, "B") _(cons_u, "X") _(sget_u, "sget") _(str_u, "str")\
 _(slen_u, "slen") _(ssub_u, "ssub")   _(scat_u, "scat")\
 _(tlen_u, "tlen") _(tbl_u, "tbl") _(tget_u, "tget")\
 _(thas_u, "thas") _(tset_u, "tset") _(tdel_u, "tdel")\
 _(tkeys_u, "tkeys") _(seek_u, "seek") _(dom_err, "fail")\
 _(putc_u, "putc") _(ystr_u, "ystr") _(emx_u, "emx")\
 _(emi_u, "emi") _(show_u, ".") _(ap_u, "ap")\
 _(peeki_u, "peeki") _(hfin_u, "hfin") _(peekx_u, "peekx")\
 _(twop_u, "twop") _(nump_u, "nump") _(homp_u, "homp")\
 _(tblp_u, "tblp") _(symp_u, "symp") _(strp_u, "strp")\

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

// FIXME confusing premature optimization
#define Locs ((ob**)fp)[-1]
// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer. if a function has
// locals, this will have been initialized before they are
// referenced.

#define ApN(n, x) (xp = (x), ip += (n), ApC(G(ip), xp))
#define ApC(f, x) (f)(v, (x), ip, hp, sp, fp)
#define ApY(f, x) (ip = (mo) (f), ApC(G(ip), (x)))

#define ARITY getnum(fp->argc)
#define ArityCheck(n) if (n > ARITY) return ApC(ary_err, putnum(n))
#define Check(_) if (!(_)) return ApC(dom_err, xp)
#define Have1() if (sp == hp) return (v->xp = 1, ApC(gc, xp))
#define Have(n) if (sp - hp < n) return (v->xp = n, ApC(gc, xp))
