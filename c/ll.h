#ifndef _terp_h
#define _terp_h
ll gc, type_error, oob_error, ary_error, div_error, yield;
ob err(mo, const char*, ...) NoInline;

#define insts(_)\
 _(tget, 0) _(tset, 0) _(thas, 0) _(tlen, 0) _(arity, 0)\
 _(idZ, 0) _(idH, 0) _(id2, 0) _(idT, 0) _(imm, 0)\
 _(arg, 0) _(clo, 0) _(loc, 0) _(take, 0) _(locals, 0)\
 _(loc_, 0) _(encll, 0) _(encln, 0) _(ret, 0)\
 _(jump, 0) _(branch, 0) _(barnch, 0) _(call, 0) _(rec, 0)\
 _(lbind, 0) _(sar, 0) _(sal, 0) _(band, 0) _(bor, 0)\
 _(bxor, 0) _(tbind, 0) _(push, 0) _(add, 0) _(sub, 0)\
 _(mul, 0) _(dqv, 0) _(mod, 0) _(neg, 0) _(lt, 0)\
 _(lteq, 0) _(eq, 0) _(gteq, 0) _(gt, 0) _(twopp, 0)\
 _(numpp, 0) _(nilpp, 0) _(strpp, 0) _(tblpp, 0) _(sympp, 0)\
 _(hompp, 0) _(car, 0) _(cdr, 0) _(cons, 0)\
 _(unit, 0) _(one, 0) _(zero, 0) _(arg0, 0) _(arg1, 0)\
 _(loc0, 0) _(loc1, 0) _(clo0, 0) _(clo1, 0)\
 _(brlt, 0) _(brlteq, 0) _(breq, 0) _(brgteq, 0) _(brlt2, 0)\
 _(brlteq2, 0) _(brgt2, 0) _(brgt, 0) _(brne, 0)\
 _(cont, 0) _(dupl, 0) _(emi, 0) _(emx, 0) _(vararg, 0)\
 _(gsym_u, tnom(Sym)) _(exit_u, "exit") _(sys_u, "sys")\
 _(par_u, "read") _(sar_u, ">>") _(sal_u, "<<") _(band_u, "&")\
 _(bor_u, "|") _(bxor_u, "^") _(clock_u, "clock")\
 _(sleep_u, "sleep") _(add_u, "+") _(hom_u, tnom(Hom))\
 _(sub_u, "-") _(mul_u, "*") _(div_u, "/") _(mod_u, "%")\
 _(lt_u, "<") _(lteq_u, "<=") _(eq_u, "=") _(gteq_u, ">=")\
 _(gt_u, ">") _(car_u, "A") _(cdr_u, "B") _(cons_u, "X")\
 _(strg, "sget") _(gsym_u, "ssym") _(strmk, tnom(Str))\
 _(strl, "slen") _(strs, "ssub")   _(strconc, "scat")\
 _(tbll, "tlen") _(tblmk, tnom(Tbl)) _(tblg, "tget")\
 _(tblc, "thas") _(tbls, "tset") _(tbld, "tdel")\
 _(bootstrap, "boot")\
 _(tblks, "tkeys") _(hseek_u, "hseek") _(fail, "fail")\
 _(ccc_u, "ccc") _(putc_u, "putc") _(ystr_u, "ystr")\
 _(slurp, "slurp") _(dump, "dump") _(hnom_u, "hnom")\
 _(emx_u, "emx") _(emi_u, "emi") _(em_u, ".") _(ev_u, "ev")\
 _(ap_u, "ap") _(hgeti_u, "hgeti")\
 _(hfin_u, "hfin") _(hgetx_u, "hgetx") _(twop_u, "twop")\
 _(nump_u, "nump") _(homp_u, "homp") _(tblp_u, "tblp")\
 _(symp_u, "symp") _(strp_u, "strp")\
 _(nilp_u, "nilp") _(rnd_u, "rand")

#define ninl(x, _) ll x NoInline;
insts(ninl)
#undef ninl

// " the interpreter "
#define Ll(n,...) NoInline ob n(mo v, ob ip, ob*fp, ob*sp, ob*hp, ob xp, ##__VA_ARGS__)
#define Vm Ll
// the arguments to a terp function collectively represent the
// runtime state, and the  return value is the result of the
// program. there are six arguments because that's the number
// that the prevalent unix calling convention on AMD64 (System
// V ABI) says should be passed in registers; that's the only
// reason why there aren't more. but it's not too bad; the six
// arguments are:
// - v  : vm instance pointer ; most lips functions take this as the first argument
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
#define Pack() (v->ip=(yo)ip,v->sp=sp,v->hp=hp,v->fp=(fr)fp,v->xp=xp)
#define Unpack() (fp=(ob*)v->fp,hp=v->hp,sp=v->sp,ip=(ob)v->ip,xp=v->xp)
#define CallC(...) (Pack(), (__VA_ARGS__), Unpack())

#define Clos ((fr)fp)->clos
#define Retp ((fr)fp)->retp
#define Subr ((fr)fp)->subd
#define Argc ((fr)fp)->argc
#define Argv ((fr)fp)->argv
#define Locs ((ob*)fp)[-1]
// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer.
// if a function has locals, this will have been initialized by the
// by the time they are referred to. the wrinkle in the representation
// gives a small but significant benefit to general function call
// performance and should be extended to the closure pointer, which is often
// nil.

// the return value of a terp function is usually a call
// to another terp function.
#define ApY(f, x) (ip = (f), ((yo)(ip))->ll(v, ip, fp, sp, hp, (x)))
#define ApC(f, x) (f)(v, ip, fp, sp, hp, (x))
#define ApN(n, x) ApY((ob)((yo)ip+(n)), (x))
#define CheckType(x,t) if(Q((x))-(t)){xp=x,v->xp=t;return ApC(type_error, xp);}
#define Arity(n) if(putnum(n)>Argc){ return ApC((v->xp=n,ary_error), xp); }
#define Tc CheckType
#define TypeCheck Tc
#define N0 putnum(0)

#define Have(n) if (sp - hp < n) return ApC((v->xp=n,gc), xp)
#define Have1() if (hp == sp) return ApC((v->xp=1,gc), xp) // common case, faster comparison

#define BINOP(nom, xpn) Vm(nom) { xp = (xpn); return ApN(1, xp); }

#define If v->glob[Cond]
#define De v->glob[Def]
#define La v->glob[Lamb]
#define Qt v->glob[Quote]
#define Se v->glob[Seq]
#define Va v->glob[Splat]
#define Top v->glob[Topl]
#define Mac v->glob[Macs]
#define Eva v->glob[Eval]
#define App v->glob[Apply]
#define IP gethom(ip)
#endif
