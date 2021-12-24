#include <setjmp.h>

struct frame { obj clos, retp, subd, argc, argv[]; };

// this is a cool way to do "static data", i got it from luajit :)
#define insts(_)\
 _(tget, 0)       _(tset, 0)     _(thas, 0)\
 _(tlen, 0)       _(arity, 0)    _(gsym_u, tnom(Sym))\
 _(idZ, 0)        _(idH, 0)      _(id2, 0)\
 _(idT, 0)        _(imm, 0)      _(arg, 0)\
 _(clo, 0)        _(loc, 0)      _(take, 0)\
 _(locals, 0)     _(loc_, 0)     _(exit_u, "exit")\
 _(encll, 0)      _(par_u, "read")\
 _(encln, 0)      _(yield, 0)    _(ret, 0)\
 _(jump, 0)       _(branch, 0)   _(barnch, 0)\
 _(call, 0)       _(rec, 0)      _(lbind, 0)\
 _(sar_u, ">>")   _(sal_u, "<<")    _(band_u, "&")\
 _(bor_u, "|")    _(bxor_u, "^")    _(sar, 0)\
 _(sal, 0)        _(band, 0)     _(bor, 0)\
 _(bxor, 0)       _(tbind, 0)    _(push, 0)\
 _(add, 0)        _(sub, 0)      _(mul, 0)\
 _(dqv, 0)        _(mod, 0)      _(neg, 0)\
 _(lt, 0)         _(lteq, 0)     _(eq, 0)\
 _(gteq, 0)       _(gt, 0)       _(twopp, 0)\
 _(numpp, 0)      _(nilpp, 0)    _(strpp, 0)\
 _(tblpp, 0)      _(sympp, 0)    _(hompp, 0)\
 _(vecpp, 0)      \
 _(car, 0)        _(cdr, 0)      _(cons, 0)\
_(add_u, "+")     _(hom_u, tnom(Hom))\
 _(sub_u, "-")       _(mul_u, "*")     _(div_u, "/")\
 _(mod_u, "%")       _(lt_u, "<")      _(lteq_u, "<=")\
 _(eq_u, "=")        _(gteq_u, ">=")   _(gt_u, ">")\
 _(car_u, "A")       _(cdr_u, "B")     _(cons_u, "X")\
 _(strg, "sget")     _(gsym_u, "ssym") _(strmk, tnom(Str))\
 _(strl, "slen")     _(strs, "ssub")   _(strconc, "scat")\
 _(unit, 0)       _(one, 0)      _(zero, 0)\
 _(arg0, 0)       _(arg1, 0)     _(loc0, 0)\
 _(loc1, 0)       _(clo0, 0)     _(clo1, 0)\
 _(brlt, 0)       _(brlteq, 0)   _(breq, 0)\
 _(brgteq, 0)     _(brlt2, 0)    _(brlteq2, 0)\
 _(brgt2, 0)      _(brgteq2, 0)  _(brgt, 0)\
 _(brne, 0)       _(tbll, "tlen")   _(tblmk, tnom(Tbl))\
 _(tblg, "tget")     _(tblc, "thas")   _(tbls, "tset")\
 _(tbld, "tdel")     _(tblks, "tkeys") _(hseek_u, "hseek")\
 _(fail, "fail")     _(ccc_u, "ccc")   _(putc_u, "putc")\
 _(cont, 0)       _(ystr_u, "ystr") _(dupl, 0)\
 _(slurp, "slurp")   _(dump, "dump") _(hnom_u, "hnom")\
 _(emi, 0)        _(emx_u, "emx")   _(emi_u, "emi")\
 _(emx, 0)        _(em_u, ".")      _(ev_u, "ev")\
 _(ap_u, "ap")       _(vararg, 0)   _(getc_u, "getc")\
 _(vget_u, "vget")   _(vset_u, "vset") _(vec_u, tnom(Vec))\
 _(hgeti_u, "hgeti") _(hfin_u, "hfin") _(hgetx_u, "hgetx")\
 _(twop_u, "twop")   _(nump_u, "nump") _(homp_u, "homp")\
 _(tblp_u, "tblp")   _(vecp_u, "vecp") _(symp_u, "symp")\
 _(strp_u, "strp")   _(nilp_u, "nilp") _(rnd_u, "rand")

#define ninl(x, _) terp x NoInline;
insts(ninl)
#undef ninl

// " the interpreter "
// is all the functions of type terp:
#define VM(n,...) NoInline obj \
 n(lips v, obj ip, mem fp, mem sp, mem hp, obj xp, ##__VA_ARGS__)
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
// vm structure. however while the interpreter is running it
// uses these struct slots to pass and return extra values
// without using the stack. so the interpreter has to restore
// the current values in the vm struct before it makes any
// "external" function calls.
#define PACK() (v->ip=ip,v->sp=sp,v->hp=hp,v->fp=fp,v->xp=xp)
#define UNPACK() (fp=v->fp,hp=v->hp,sp=v->sp,ip=v->ip,xp=v->xp)
#define CALLC(...)(PACK(),(__VA_ARGS__),UNPACK())
#define RETC(...){CALLC(__VA_ARGS__);Jump(ret);}

#define CLOS ((frame)fp)->clos
#define RETP ((frame)fp)->retp
#define SUBR ((frame)fp)->subd
#define ARGC ((frame)fp)->argc
#define ARGV ((frame)fp)->argv
#define LOCS fp[-1]
#define Frame ((frame)fp)
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
#define STATE v,ip,fp,sp,hp,xp
#define Jump(f,...) return (f)(STATE,##__VA_ARGS__)
#define AP(f,x) return (ip=f,xp=x,G(ip)(STATE))
#define GO(f,x) return (xp=x,f(STATE))
#define NEXT(n) AP(ip+w2b(n),xp)
#define ok _N(1)
// type check
#define TC(x,t) if(kind((x))-(t)){v->xp=t;Jump(type_error);}
// arity check
#define arity_err_msg "wrong arity : %d of %d"
#define ARY(n) if(_N(n)>ARGC){\
 v->xp = n;\
 Jump(ary_error); }

#define OP(nom, x, n) VM(nom) { xp = (x); NEXT(n); }
#define OP0(nom, x) OP(nom, x, 0)
#define OP1(nom, x) OP(nom, x, 1)
#define OP2(nom, x) OP(nom, x, 2)

#define avail (sp-hp)
#define Have(n) if (avail < n) Jump((v->xp=n,gc))
#define Have1() if (hp == sp) Jump((v->xp=1,gc)) // common case, faster comparison

#define TERP(n, m, ...) VM(n) m(__VA_ARGS__)
#define BINOP(nom, xpn) VM(nom) { xp = (xpn); NEXT(1); }

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
#define Re  v->glob[Restart]

VM(nope, const char *, ...);
VM(gc);
VM(type_error);
VM(oob_error);
VM(ary_error);
obj restart(lips), err(lips, char*, ...);
u0 errp(lips, char*, ...);
