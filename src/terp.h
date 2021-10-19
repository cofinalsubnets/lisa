// this is a cool way to do "static data", i got it from luajit :)
#define insts(_)\
 _(tget, NULL)       _(tset, NULL)     _(thas, NULL)\
 _(tlen, NULL)       _(arity, NULL)    _(gsym_u, tnom(Sym))\
 _(idZ, NULL)        _(idH, NULL)      _(id2, NULL)\
 _(idT, NULL)        _(imm, NULL)      _(arg, NULL)\
 _(clo, NULL)        _(loc, NULL)      _(take, NULL)\
 _(locals, NULL)     _(loc_, NULL)     _(pc0, NULL)\
 _(pc1, NULL)        _(clos, NULL)     _(encll, NULL)\
 _(encln, NULL)      _(yield, NULL)    _(ret, NULL)\
 _(jump, NULL)       _(branch, NULL)   _(barnch, NULL)\
 _(call, NULL)       _(rec, NULL)      _(lbind, NULL)\
 _(sar_u, ">>")      _(sal_u, "<<")    _(band_u, "&")\
 _(bor_u, "|")       _(bxor_u, "^")    _(sar, NULL)\
 _(sal, NULL)        _(band, NULL)     _(bor, NULL)\
 _(bxor, NULL)       _(tbind, NULL)    _(push, NULL)\
 _(add, NULL)        _(sub, NULL)      _(mul, NULL)\
 _(dqv, NULL)        _(mod, NULL)      _(neg, NULL)\
 _(lt, NULL)         _(lteq, NULL)     _(eq, NULL)\
 _(gteq, NULL)       _(gt, NULL)       _(twopp, NULL)\
 _(numpp, NULL)      _(nilpp, NULL)    _(strpp, NULL)\
 _(tblpp, NULL)      _(sympp, NULL)    _(hompp, NULL)\
 _(car, NULL)        _(cdr, NULL)      _(cons, NULL)\
 _(vecpp, NULL)      _(add_u, "+")     _(hom_u, tnom(Hom))\
 _(sub_u, "-")       _(mul_u, "*")     _(div_u, "/")\
 _(mod_u, "%")       _(lt_u, "<")      _(lteq_u, "<=")\
 _(eq_u, "=")        _(gteq_u, ">=")   _(gt_u, ">")\
 _(car_u, "A")       _(cdr_u, "B")     _(cons_u, "X")\
 _(strg, "sget")     _(gsym_u, "ssym") _(strmk, tnom(Str))\
 _(strl, "slen")     _(strs, "ssub")   _(strconc, "scat")\
 _(unit, NULL)       _(one, NULL)      _(zero, NULL)\
 _(arg0, NULL)       _(arg1, NULL)     _(loc0, NULL)\
 _(loc1, NULL)       _(clo0, NULL)     _(clo1, NULL)\
 _(brlt, NULL)       _(brlteq, NULL)   _(breq, NULL)\
 _(brgteq, NULL)     _(brlt2, NULL)    _(brlteq2, NULL)\
 _(brgt2, NULL)      _(brgteq2, NULL)  _(brgt, NULL)\
 _(brne, NULL)       _(tbll, "tlen")   _(tblmk, tnom(Tbl))\
 _(tblg, "tget")     _(tblc, "thas")   _(tbls, "tset")\
 _(tbld, "tdel")     _(tblks, "tkeys") _(hseek_u, "hseek")\
 _(fail, "fail")     _(ccc_u, "ccc")   _(putc_u, "putc")\
 _(cont, NULL)       _(ystr_u, "ystr") _(dupl, NULL)\
 _(slurp, "slurp")   _(dump, "dump")\
 _(emi, NULL)        _(emx_u, "emx")   _(emi_u, "emi")\
 _(emx, NULL)        _(em_u, ".")      _(ev_u, "ev")\
 _(ap_u, "ap")       _(vararg, NULL)   _(getc_u, "getc")\
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
#define PACK() (v->ip=ip,Sp=sp,Hp=hp,Fp=fp,v->xp=xp)
#define UNPACK() (fp=Fp,hp=Hp,sp=Sp,ip=v->ip,xp=v->xp)
#define CALLC(...)(PACK(),(__VA_ARGS__),UNPACK())
#define RETC(...){CALLC(__VA_ARGS__);Jump(ret);}

// the frame structure holds the current function context.
typedef struct frame { obj clos, retp, subd, argc, argv[]; } *frame;
#define CLOS ((frame)fp)->clos
#define RETP ((frame)fp)->retp
#define SUBR ((frame)fp)->subd
#define ARGC ((frame)fp)->argc
#define ARGV ((frame)fp)->argv
#define LOCS fp[-1]
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
