#include <stdarg.h>
#include <string.h>
#include "em.h"

#define N1 putnum(1)

// " the virtual machine "
// It's a stack machine with one free register that runs on
// top of the C compiler's calling convention. This lets us
// keep the main state variables in CPU registers without
// needing to write any platform specific code, as long as
// the compiler passes enough arguments in registers and
// optimizes tail calls.

// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function

// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.

Ll(ap_u) {
  Arity(2);
  ip = (mo) fp->argv[0];
  TypeCheck((ob)ip, Hom);
  ob x = fp->argv[1];
  uintptr_t adic = llen(x);
  Have(adic);
  ob off = fp->subd, rp = fp->retp;
  sp = fp->argv + getnum(fp->argc) - adic;
  for (uintptr_t j = 0; j < adic; sp[j++] = A(x), x = B(x));
  return
    fp = (fr) sp - 1,
    sp = (ob*) fp,
    fp->retp = rp,
    fp->argc = putnum(adic),
    fp->subd = off,
    fp->clos = nil,
    ApY(ip, nil); }

Ll(vararg) {
  intptr_t reqd = getnum((ob) ip[1].ll),
           vdic = getnum(fp->argc) - reqd;
  Arity(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) {
    Have1(); return
      cpyw((ob*)fp - 1, fp, Width(fr) + getnum(fp->argc)),
      fp = (fr) ((ob*) fp - 1),
      sp = (ob*) fp,
      fp->argc += sizeof(ob),
      fp->argv[reqd] = nil,
      ApN(2, xp); }
  // in this case we just keep the existing slots.
  // the path is knowable at compile time in many cases
  // so maybe vararg should be two or more different
  // functions.
  Have(2 * vdic);
  two t = (two) hp;
  hp += 2 * vdic;
  for (uintptr_t i = vdic; i--;)
    t[i].a = fp->argv[reqd + i],
    t[i].b = puttwo(t+i+1);
  return
    t[vdic-1].b = nil,
    fp->argv[reqd] = puttwo(t),
    ApN(2, xp); }

// type predicates
#define Tp(t)\
  Ll(t##pp) { return ApN(1, (t##p(xp)?N1:nil)); }\
  Ll(t##p_u) {\
    for (ob *xs = fp->argv, *l = xs + getnum(fp->argc); xs < l;)\
      if (!t##p(*xs++)) return ApC(ret, nil);\
    return ApC(ret, N1); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// stack manipulation
Ll(dupl) { Have1(); --sp; sp[0] = sp[1]; return ApN(1, xp); }

static ll recne;

////
/// Branch Instructions
//
// unconditional jump
Ll(jump) { return ApY((ob) ip[1].ll, xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Ll(nom) {\
  return ApY(test ? (ob) a(ip) : (ob) b(ip), x); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq, eql(*sp++, xp), GF, N1, FF, nil)
Br(brne, eql(*sp++, xp), FF, N1, GF, nil)

Br(brlt,    *sp++ < xp,  GF, xp, FF, nil)
Br(brlt2,   *sp++ < xp,  FF, xp, GF, nil)
Br(brlteq,  *sp++ <= xp, GF, xp, FF, nil)
Br(brlteq2, *sp++ <= xp, FF, xp, GF, nil)
Br(brgt,    *sp++ > xp,  GF, xp, FF, nil)
Br(brgt2,   *sp++ > xp,  FF, xp, GF, nil)
Br(brgteq,  *sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt
#undef Br

// return from a function
Ll(ret) { return
  ip = (yo) fp->retp,
  sp = (ob*) ((ob) fp->argv + fp->argc - Num),
  fp = (fr) ((ob) sp + fp->subd - Num),
  ApY(ip, xp); }

// "inner" function call
Ll(call) {
  Have(Width(fr));
  intptr_t adic = getnum((ob) ip[1].ll),
           off = (ob*) fp - (sp + adic);
  return
    fp = (fr) sp - 1,
    sp = (ob*) fp,
    fp->retp = (ob) (ip + 2),
    fp->subd = putnum(off),
    fp->clos = nil,
    fp->argc = putnum(adic),
    ApY(xp, nil); }

// tail call
Ll(rec) { return
  ip = (yo) ip[1].ll,
  fp->argc != (ob) ip ? ApC(recne, xp) :
  (cpyw(fp->argv, sp, getnum((ob) ip)),
   sp = (ob*) fp,
   ApY(xp, nil)); }

// tail call with different arity
static Ll(recne) { return
  v->xp = fp->subd,
  v->ip = (yo) fp->retp, // save return info
  fp = (fr) (fp->argv + getnum(fp->argc - (ob) ip)),
  rcpyw(fp, sp, getnum((ob) ip)), // copy from high to low
  sp = (ob*) (((fr) fp) - 1),
  fp = (fr) sp,
  fp->retp = (ob) v->ip,
  fp->argc = (ob) ip,
  fp->subd = v->xp,
  ApY(xp, fp->clos = nil); }

// errors
Ll(fail) { return Pack(), err(v, xp, 0); }

Ll(domain_error) {
  return Pack(), err(v, xp, "is undefined at point"); }

#define arity_err_msg "has %d of %d required arguments"
Ll(ary_error) { return Pack(),
  err(v, 0, arity_err_msg, getnum(fp->argc), getnum(xp)); }

// type/arity checking
#define DTc(n, t) Ll(n) { TypeCheck(xp, t); return ApN(1, xp); }
DTc(idZ, Num) DTc(idH, Hom)
DTc(idT, Tbl) DTc(id2, Two)
Ll(arity) {
  ob reqd = (ob) ip[1].ll;
  return reqd <= fp->argc ?  ApN(2, xp) : ApC(ary_error, reqd); }

static void show_call(em v, yo ip, fr fp) {
  fputc('(', stderr), emit(v, (ob) ip, stderr);
  for (uintptr_t i = 0, argc = getnum(fp->argc); i < argc;)
    fputc(' ', stderr), emit(v, fp->argv[i++], stderr);
  fputc(')', stderr); }

#define atop (fp->argc==putnum(-Width(fr)))
NoInline ob err(em v, ob x, const char *msg, ...) {
  if (x || msg) {
    yo ip = v->ip;
    fr fp = v->fp;
    // error line
    fputs("; ", stderr);
    if (!atop) show_call(v, ip, fp), fputs(" ", stderr);
    if (msg) {
      va_list xs;
      va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
      if (x) fputs(" ", stderr); }
    if (x) emit(v, x, stderr);
    fputc('\n', stderr);

    // backtrace
    if (!atop) for (;;) {
      ip = (yo) fp->retp, fp = (fr)
        ((ob*) (fp + 1) + getnum(fp->argc)
                        + getnum(fp->subd));
      if (atop) break; else
        fputs("; in ", stderr),
        show_call(v, ip, fp),
        fputc('\n', stderr); } }
#undef atop

  // reset and yield
  return v->fp = (fr) (v->pool + v->len) - 1,
         v->sp = (ob*) v->fp,
         v->xp = nil,
         v->ip = (mo) v->fp->retp,
         0; }



#define mm_u(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);}\
  return ApC(ret, putnum(xp));}

#define mm_void(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);\
    if (x == N0) return ApC(domain_error, x);}\
  return ApC(ret, putnum(xp));}

Ll(sub_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  if (xp == 1) return ApC(ret, putnum(-getnum(*fp->argv)));
  mm_u(xp-1,fp->argv+1,getnum(*fp->argv),-); }

Ll(sar_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  mm_u(getnum(fp->argc)-1, fp->argv+1, getnum(*fp->argv), >>); }

Ll(sal_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  mm_u(getnum(fp->argc)-1, fp->argv+1, getnum(*fp->argv), <<); }

Ll(dqv) {
  return xp == N0 ? ApC(domain_error, xp) :
    (xp = putnum(getnum(*sp++) / getnum(xp)), ApN(1, xp)); }

Ll(div_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, N1);
  TypeCheck(*fp->argv, Num);
  mm_void(xp-1, fp->argv+1, getnum(*fp->argv), /); }

Ll(mod) {
  return xp == N0 ? ApC(domain_error, xp) :
    (xp = putnum(getnum(*sp++) % getnum(xp)), ApN(1, xp)); }

Ll(mod_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, N1);
  TypeCheck(*fp->argv, Num);
  mm_void(xp-1, fp->argv+1, getnum(*fp->argv), %); }

Ll(rnd_u) { return
  xp = putnum(v->rand = lcprng(v->rand)), ApC(ret, xp); }

#define OP(nom, x, n) Ll(nom) { xp = (x); return ApN(n, xp); }
#define OP1(nom, x) OP(nom, x, 1)
#define BINOP(nom, xpn) Vm(nom) { xp = (xpn); return ApN(1, xp); }
OP1(neg, putnum(-getnum(xp)))
BINOP(add,  xp + *sp++ - Num)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  putnum(getnum(*sp++) * getnum(xp)))
BINOP(band, xp & *sp++)
BINOP(sub,  *sp++ - xp + Num)
BINOP(sar,  putnum(getnum(*sp++) >> getnum(xp)))
BINOP(sal,  putnum(getnum(*sp++) << getnum(xp)))

#define UBINOP(nom, dflt, op) Ll(nom##_u) {\
  mm_u(getnum(fp->argc), fp->argv, dflt, op); }

UBINOP(add, 0, +)
UBINOP(bor, 0, |)
UBINOP(bxor, 0, ^)
UBINOP(mul, 1, *)
UBINOP(band, -1, &)

bool eql(ob a, ob b) {
  return a == b || (Q(a) == Q(b) &&
    ((twop(a) && eql(A(a), A(b)) && eql(B(a), B(b))) ||
     (strp(a) && getstr(a)->len == getstr(b)->len &&
      0 == strcmp(getstr(a)->text, getstr(b)->text)))); }

#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
BINOP(eq, eql(xp, *sp++) ? N1 : nil)
#define cmp(n, op) BINOP(n, op(*sp++, xp) ? xp : nil)
cmp(lt, LT) cmp(lteq, LE) cmp(gteq, GE) cmp(gt, GT)
#undef cmp
#define cmp(op, n) Ll(n##_u) {\
  ob n = getnum(fp->argc), *xs = fp->argv, m, *l;\
  switch (n) {\
    case 0: return ApC(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) return ApC(ret, nil);\
    case 1: return ApC(ret, N1); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(eql, eq)
#define OP2(nom, x) OP(nom, x, 2)
////
/// Load Instructions
//
// constants
OP1(one, N1)
OP1(zero, N0)
// immediate value from thread
OP2(imm, (ob) ip[1].ll)

// indexed references
#define Ref(b) (*(ob*)((ob)(b)+(ob)ip[1].ll-Num))
// pointer arithmetic works because fixnums are premultiplied by W

// function arguments
OP2(arg, Ref(fp->argv))
OP1(arg0, fp->argv[0])
OP1(arg1, fp->argv[1])

// local variables
OP2(loc, Ref(((ob*)Locs)))
OP1(loc0, ((ob*)Locs)[0])
OP1(loc1, ((ob*)Locs)[1])

// closure variables
OP2(clo, Ref(((ob*)fp->clos)))
OP1(clo0, ((ob*)fp->clos)[0])
OP1(clo1, ((ob*)fp->clos)[1])

////
/// Store Instructions
//
// stack push
Vm(push) { Have1(); return *--sp = xp, ApN(1, xp); }

// set a local variable
Vm(loc_) { return
  Ref(((ob*)Locs)) = xp,
  ApN(2, xp); }

// set a global variable
Vm(tbind) { return
  CallC(tbl_set(v, cwm(v), (ob) ip[1].ll, xp)),
  ApN(2, xp); }

// allocate local variable array
Vm(locals) {
  ob *t = hp;
  uintptr_t n = getnum((ob) ip[1].ll);
  Have(n + 3); return
    hp += n + 2,
    setw(t, nil, n),
    t[n] = 0,
    *--sp = t[n+1] = (ob) t,
    ApN(2, xp); }

// late binding
// long b/c it does the "static" type and arity checks
// that would have been done by the compiler if the function
// had been bound early.
Vm(rslv) {
  ob w = (ob) ip[1].ll, d = AB(w), typ = A(w) >> TagBits;
  if (!(w = tbl_get(v, d, xp = BB(w))))
    return Pack(), err(v, xp, "referenced undefined variable");
  xp = w;
  if (typ != sizeof(ob)) TypeCheck(xp, typ);
  // omit the arity check if possible
  if ((ip[2].ll == call || ip[2].ll == rec) && // xp will be a hom
      R(xp)[0] == (ob) arity &&
      R(ip)[3] >= R(xp)[1])
    xp = (ob) (R(xp) + 2);
  return ip[0].ll = imm,
         ip[1].ll = (ll*) xp,
         ApN(2, xp); }

// hash tables
Vm(tblg) {
  Arity(2);
  TypeCheck(fp->argv[0], Tbl);
  return xp = tbl_get(v, fp->argv[0], fp->argv[1]),
         ApC(ret, xp ? xp : nil); }

OP1(tget, (xp = tbl_get(v, xp, *sp++)) ? xp : nil)
OP1(thas, tbl_get(v, xp, *sp++) ? N1 : nil)
OP1(tlen, putnum(gettbl(xp)->len))

static ob tbl_keys_j(em v, ob e, ob l) {
  ob x; return e == nil ? l :
    (x = R(e)[0],
     with(x, l = tbl_keys_j(v, R(e)[2], l)),
     l ? pair(v, x, l) : 0); }

static ob tbl_keys_i(em v, ob t, intptr_t i) {
  ob k; return i == 1 << gettbl(t)->cap ? nil :
    (with(t, k = tbl_keys_i(v, t, i+1)),
     k ? tbl_keys_j(v, gettbl(t)->tab[i], k) : 0); }

static Inline ob tbl_keys(em v, ob t) {
  return tbl_keys_i(v, t, 0); }

Vm(tkeys) { return
  CallC(v->xp = tbl_keys(v, xp)),
  xp ? ApN(1, xp) : 0; }

Vm(tblc) {
  Arity(2);
  TypeCheck(fp->argv[0], Tbl);
  return xp = tbl_get(v, fp->argv[0], fp->argv[1]),
         ApC(ret, xp ? N1 : nil); }

static ob tblss(em v, intptr_t i, intptr_t l) {
  fr fp = (fr) v->fp;
  return
    i > l - 2 ? fp->argv[i - 1] :
    !tbl_set(v, v->xp, fp->argv[i], fp->argv[i + 1]) ? 0 :
    tblss(v, i + 2, l); }

Vm(tbls) {
  Arity(1);
  TypeCheck(*fp->argv, Tbl);
  return
    xp = *fp->argv,
    CallC(v->xp = tblss(v, 1, fp->argc >> TagBits)),
    xp ? ApC(ret, xp) : 0; }

Vm(tblmk) {
  return Pack(),
    (v->xp = table(v)) &&
    (xp = tblss(v, 0, fp->argc >> TagBits)) ?
      (Unpack(), ApC(ret, xp)) :
      0; }

Vm(tblks) {
  Arity(1);
  TypeCheck(*fp->argv, Tbl);
  return CallC(v->xp = tbl_keys(v, *fp->argv)),
         xp ? ApC(ret, xp) : 0; }

Vm(tbll) {
  Arity(1);
  TypeCheck(*fp->argv, Tbl);
  return ApC(ret, putnum(gettbl(*fp->argv)->len)); }

Vm(tset) {
  ob x = *sp++, y = *sp++;
  return
    CallC(v->xp = tbl_set(v, xp, x, y)),
    !xp ? 0 : ApN(1, xp); }

// pairs
OP1(car, A(xp)) OP1(cdr, B(xp))
Vm(cons) { Have1(); return
  hp[0] = xp,
  hp[1] = *sp++,
  xp = puttwo(hp),
  hp += 2,
  ApN(1, xp); }

Vm(car_u) {
  Arity(1);
  TypeCheck(*fp->argv, Two);
  return ApC(ret, A(*fp->argv)); }

Vm(cdr_u) {
  Arity(1);
  TypeCheck(*fp->argv, Two);
  return ApC(ret, B(*fp->argv)); }

Ll(cons_u) {
  Arity(2); Have(2); two w; return
    w = (two) hp,
    hp += 2,
    w->a = fp->argv[0],
    w->b = fp->argv[1],
    ApC(ret, puttwo(w)); }

// this is used to create closures.
Ll(take) {
  uintptr_t n = getnum((ob) ip[1].ll);
  Have(n + 2);
  ob *t = hp; return
    hp += n + 2,
    cpyw(t, sp, n),
    sp += n,
    t[n] = 0,
    t[n+1] = (ob) t,
    ApC(ret, (ob) t); }

static Ll(clos) { return
  fp->clos = (ob) ip[1].ll,
  ApY((ob) ip[2].ll, xp); }

// finalize function instance closure
static Ll(clos1) { return
  ip->ll = clos,
  ip[1].ll = (ll*) xp,
  ApY(ip, xp); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
static Vm(clos0) {
  ob *ec  = (ob*) ip[1].ll,
     arg = ec[0],
     loc = ec[1];
  uintptr_t adic = nilp(arg) ? 0 : getnum(*(ob*)arg);
  Have(Width(fr) + adic + 1);
  intptr_t off = (ob*) fp - sp;
  ip->ll = clos1;
  sp -= adic;
  cpyw(sp, (ob*) arg + 1, adic);
  ec = (ob*) ip[1].ll;
  sp = (ob*) (fp = (fr) sp - 1);
  fp->retp = (ob) ip;
  fp->subd = putnum(off);
  fp->argc = putnum(adic);
  fp->clos = ec[2];
  if (!nilp(loc)) *--sp = loc;
  return ApY(ec[3], xp); }

// the next few functions create and store
// lexical environments.
// FIXME magic numbers
static Vm(encl) {
  Z m = getnum(fp->argc),
    n = m + (m ? 14 : 11);
  Have(n);
  ob x = (ob) ip[1].ll, arg = nil, *block = hp;
  hp += n;
  if (m) {
    n -= 11;
    ob *t = block;
    block += n;
    for (t[n-2] = 0,
         t[n-1] = (ob) t,
         t[0] = putnum(n-=3);
         n--;
         t[n+1] = fp->argv[n]);
    arg = (ob) t; }

  yo t = (yo) block, // compiler thread closure array (1 length 5 elements)
     at = t + 6; // compiler thread (1 instruction 2 data 2 tag)

  return
    t[0].ll = (ll*) arg,
    t[1].ll = (ll*) xp, // Locs or nil
    t[2].ll = (ll*) fp->clos,
    t[3].ll = (ll*) B(x),
    t[4].ll = NULL,
    t[5].ll = (ll*) t,

    at[0].ll = clos0,
    at[1].ll = (ll*) t,
    at[2].ll = (ll*) A(x),
    at[3].ll = NULL,
    at[4].ll = (ll*) at,

    ApN(2, (ob) at); }

Ll(encll) { return ApC(encl, Locs); }
Ll(encln) { return ApC(encl, nil); }
Ll(cwm_u) { return ApC(ret, cwm(v)); }

ob homnom(em v, ob x) {
  ll *k = (ll*) gethom(x)->ll;
  if (k == clos || k == clos0 || k == clos1)
    return homnom(v, (ob) gethom(x)[2].ll);
  ob* h = (ob*) gethom(x);
  while (*h) h++;
  x = h[-1];
  int inb = (ob*) x >= v->pool && (ob*) x < v->pool+v->len;
  return inb ? x : nil; }
