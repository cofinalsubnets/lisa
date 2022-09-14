// these are vm functions used by C but not lisp.
vm
  gc NoInline,
  dom_err NoInline,
  nom_err NoInline,
  oom_err NoInline,
  ary_err NoInline,
  clos NoInline,
  clos0 NoInline,
  clos1 NoInline;

// this is a big table of public vm instructions.
//
// the first item in each pair is the vm function name.
// the second is an optional internal name.
//
// vm functions named in this table receive C prototypes and
// are exposed internally, either under the internal name if
// present, or under a name generated from the function name.
#define insts(_)\
 _(tget, 0) _(tset, 0) _(thas, 0) _(tlen, 0) _(arity, 0)\
 _(idZ, 0) _(idH, 0) _(id2, 0) _(idT, 0) _(imm, 0)\
 _(arg, 0) _(clo, 0) _(loc, 0) _(take, 0) _(locals, 0)\
 _(loc_, 0) _(encll, 0) _(encln, 0) _(ret, 0)\
 _(jump, 0) _(branch, 0) _(barnch, 0) _(call, 0) _(rec, 0)\
 _(latebind, 0) _(sar, 0) _(sal, 0) _(band, 0) _(bor, 0)\
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
 _(sym_u, "sym") _(cwm_u, "cwm")\
 _(sar_u, ">>") _(sal_u, "<<") _(band_u, "&") _(bnot_u, "!")\
 _(bor_u, "|") _(bxor_u, "^") _(add_u, "+") _(hom_u, "hom")\
 _(sub_u, "-") _(mul_u, "*") _(div_u, "/") _(mod_u, "%")\
 _(lt_u, "<") _(lteq_u, "<=") _(eq_u, "=") _(gteq_u, ">=")\
 _(gt_u, ">") _(car_u, "A") _(cdr_u, "B") _(cons_u, "X")\
 _(sget_u, "sget") _(str_u, "str")\
 _(slen_u, "slen") _(ssub_u, "ssub")   _(scat_u, "scat")\
 _(tlen_u, "tlen") _(tbl_u, "tbl") _(tget_u, "tget")\
 _(thas_u, "thas") _(tset_u, "tset") _(tdel_u, "tdel")\
 _(tkeys_u, "tkeys") _(seek_u, "seek") _(dom_err, "fail")\
 _(putc_u, "putc") _(ystr_u, "ystr")\
 _(emx_u, "emx") _(emi_u, "emi") _(show_u, ".") _(ev_u, "ev")\
 _(ap_u, "ap") _(peeki_u, "peeki")\
 _(hfin_u, "hfin") _(peekx_u, "peekx") _(twop_u, "twop")\
 _(nump_u, "nump") _(homp_u, "homp") _(tblp_u, "tblp")\
 _(symp_u, "symp") _(strp_u, "strp")\
 _(nilp_u, "nilp") _(rnd_u, "rand")

#define ninl(x, _) vm x NoInline;
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
#define Locs ((ob**)fp)[-1]
#define Clos ((ob*)fp->clos)
// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer. if a function has
// locals, this will have been initialized before they are
// referenced.

#define ApN(n, x) ApY(ip+(n), (x))
#define ApC(f, x) (f)(v, (x), ip, hp, sp, fp)
#define ApY(f, x) (ip = (mo) (f), ApC(ip->ll, (x)))

#define Argv fp->argv
#define Argc fp->argc
#define ArityCheck(n) if (putnum(n) > Argc) return ApC(ary_err, putnum(n))
#define TypeCheck(x,t) if (Q(x) != t) return ApC(dom_err, xp)
#define Collect(n) ApC((v->xp=n, gc), xp)
#define Free (sp - hp)
#define Have1() if (!Free) return Collect(1)
#define Have(n) if (Free < n) return Collect(n)
#define ArityError(n) ApC(ary_err, putnum(n))
#define Undefined() ApC(dom_err, xp)
