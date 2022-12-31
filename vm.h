// " the interpreter "
#define Vm(n) NoInline enum status\
  n(la v, ob xp, mo ip, ob *hp, ob *sp, frame fp)
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

// these are vm functions used by C but not lisp.
#define VM0(_)\
  _(gc) _(xok) _(yield)\
  _(setclo) _(genclo0) _(genclo1)\
  _(ap_str) _(ap_nop) _(ap_two) _(ap_tbl)
#define decl(x, ...) Vm(x);
VM0(decl)

// used by the compiler but not exposed as primitives
#define VM1(_)\
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

VM1(decl)

// primitive functions
#define VM2(_) _(ev_f, "ev") _(ap_f, "ap")\
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

VM2(decl)
#undef decl

