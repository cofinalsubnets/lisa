#include "i.h"
#include "vm.h"

Vm(ap_nop) { return ApC(ret, (ob) ip); }
// comparison operators
Vm(lt) { return ApN(1, *sp++ < xp ? T : nil); }
Vm(lteq) { return ApN(1, *sp++ <= xp ? T : nil); }
Vm(eq) { return ApN(1, eql(v, *sp++, xp) ? T : nil); }
Vm(gteq) { return ApN(1, *sp++ >= xp ? T : nil); }
Vm(gt) { return ApN(1, *sp++ > xp ? T : nil); }

// TODO remove macros
#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
#define EQ(a,b) eql(v,a,b)
#define cmp(op, n) Vm(n##_f) {\
  for (I i = fp->argc-1; i > 0; i--)\
    if (!op(fp->argv[i-1], fp->argv[i])) return ApC(ret, nil);\
  return ApC(ret, T); }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(EQ, eq)

// type predicates
#define Tp(t)\
  Vm(t##p_) { return ApN(1, (t##p(xp)?T:nil)); }\
  Vm(t##p_f) {\
    for (size_t i = fp->argc; i;)\
      if (!t##p(fp->argv[--i])) return ApC(ret, nil);\
    return ApC(ret, T); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// type/arity checking
Vm(idno) { return nump(xp) ? ApN(1, xp) :
  Yield(DomainError, xp); }
Vm(idmo) { return homp(xp) ? ApN(1, xp) :
  Yield(DomainError, xp); }
Vm(idtbl) { return tblp(xp) ? ApN(1, xp) :
  Yield(DomainError, xp); }
Vm(idtwo) { return twop(xp) ? ApN(1, xp) :
  Yield(DomainError, xp); }
Vm(arity) { return fp->argc >= getnum(GF(ip)) ? ApN(2, xp) :
    Yield(ArityError, (ob) GF(ip)); }
Vm(ary1) { return fp->argc >= 1 ? ApN(1, xp) :
  Yield(ArityError, putnum(1)); }
Vm(ary2) { return fp->argc >= 2 ? ApN(1, xp) :
  Yield(ArityError, putnum(2)); }
Vm(ary3) { return fp->argc >= 3 ? ApN(1, xp) :
  Yield(ArityError, putnum(3)); }
Vm(ary4) { return fp->argc >= 4 ? ApN(1, xp) :
  Yield(ArityError, putnum(4)); }
////
/// Branch Instructions
//
// calling and returning
//
// return from a function
Vm(ret) { return
  ip = fp->retp,
  sp = fp->argv + fp->argc,
  fp = fp->subd,
  ApY(ip, xp); }

// normal function call
Vm(call) {
  Have(Width(struct frame));
  sf subd = fp;
  return
    sp = (ob*) (fp = (sf) sp - 1),
    fp->argc = getnum(GF(ip)),
    fp->retp = FF(ip),
    fp->subd = subd,
    fp->clos = (ob*) nil,
    ApY(xp, nil); }

// tail calls
Vm(rec) {
  U adic = getnum(GF(ip));
  // save return address
  sf subd = fp->subd;
  mo retp = fp->retp;
  return
    // reset fp
    fp = (sf) (fp->argv + fp->argc - adic) - 1,
    // copy the args high to low BEFORE repopulating fp.
    cpyw_r2l(fp->argv, sp, adic),
    sp = (ob*) fp,
    // populate fp
    fp->retp = retp,
    fp->subd = subd,
    fp->argc = adic,
    fp->clos = (ob*) nil,
    ApY((mo) xp, nil); }

Vm(ap_f) {
  if (fp->argc < 2) return Yield(ArityError, putnum(2));
  if (!homp(fp->argv[0])) return Yield(DomainError, xp);
  xp = fp->argv[1];
  size_t adic = llen(xp);
  Have(adic);
  ip = (mo) fp->argv[0];
  sf subd = fp->subd;
  mo retp = fp->retp;
  sp = (ob*) (fp = (frame) (fp->argv + fp->argc - adic) - 1);
  fp->retp = retp;
  fp->argc = adic;
  fp->subd = subd;
  fp->clos = (ob*) nil;
  for (ob *i = fp->argv; adic--; xp = B(xp)) *i++ = A(xp);
  return ApY(ip, nil); }

// unconditional jump
Vm(jump) { return ApY(GF(ip), xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, b) Vm(nom) { return\
  ApY((test) ? (ob) a(ip) : (ob) b(ip), xp); }
// combined test/branch instructions
Br(br1, nilp(xp), FF, GF)
Br(br0, nilp(xp), GF, FF)
Br(bre, eql(v, xp, *sp++), GF, FF)
Br(brn, eql(v, xp, *sp++), FF, GF)
Br(brl, *sp++ < xp, GF, FF)
Br(brg, *sp++ > xp, GF, FF)
Br(brle, *sp++ <= xp, GF, FF)
Br(brge, *sp++ >= xp, GF, FF)

////
/// Load Instructions
//

// immediate values
Vm(imm) { return ApN(2, (ob) GF(ip)); }
Vm(imm0) { return ApN(1, putnum(0)); }
Vm(imm1) { return ApN(1, putnum(1)); }
Vm(immn1) { return ApN(1, putnum(-1)); }

Vm(imm0p) { return ApC(push, putnum(0)); }
Vm(imm1p) { return ApC(push, putnum(1)); }
Vm(immn1p) { return ApC(push, putnum(-1)); }

// function arguments
Vm(argn) { return ApN(2, fp->argv[getnum(GF(ip))]); }
Vm(arg0) { return ApN(1, fp->argv[0]); }
Vm(arg1) { return ApN(1, fp->argv[1]); }
Vm(arg2) { return ApN(1, fp->argv[2]); }
Vm(arg3) { return ApN(1, fp->argv[3]); }
Vm(arg0p) { return ApC(push, fp->argv[0]); }
Vm(arg1p) { return ApC(push, fp->argv[1]); }
Vm(arg2p) { return ApC(push, fp->argv[2]); }
Vm(arg3p) { return ApC(push, fp->argv[3]); }

// the first two stack slots under the current frame
// may hold extra call data.
#define Slot1 ((ob**)fp)[-1]
#define Slot2 ((ob**)fp)[-2]

// local variables
Vm(sl1n) { return ApN(2, Slot1[getnum(GF(ip))]); }
Vm(sl10) { return ApN(1, Slot1[0]); }
Vm(sl11) { return ApN(1, Slot1[1]); }
Vm(sl12) { return ApN(1, Slot1[2]); }
Vm(sl13) { return ApN(1, Slot1[3]); }
Vm(sl10p) { return ApC(push, Slot1[0]); }
Vm(sl11p) { return ApC(push, Slot1[1]); }
Vm(sl12p) { return ApC(push, Slot1[2]); }
Vm(sl13p) { return ApC(push, Slot1[3]); }

// closure variables
Vm(clon) { return ApN(2, fp->clos[getnum(GF(ip))]); }
Vm(clo0) { return ApN(1, fp->clos[0]); }
Vm(clo1) { return ApN(1, fp->clos[1]); }
Vm(clo2) { return ApN(1, fp->clos[2]); }
Vm(clo3) { return ApN(1, fp->clos[3]); }
Vm(clo0p) { return ApC(push, fp->clos[0]); }
Vm(clo1p) { return ApC(push, fp->clos[1]); }
Vm(clo2p) { return ApC(push, fp->clos[2]); }
Vm(clo3p) { return ApC(push, fp->clos[3]); }

////
/// Store Instructions
// // stack push
Vm(push) { Have1(); return
  *--sp = xp,
  ApN(1, xp); }

// set a local variable
Vm(defsl1) { return
  Slot1[getnum(GF(ip))] = xp,
  ApN(2, xp); }

// set a module variable
Vm(deftop) { bool _; return
  CallOut(_ = tbl_set(v, (tbl) A(GF(ip)), B(GF(ip)), xp)),
  _ ? ApN(2, xp) : Yield(OomError, xp); }

// allocate local variable array
Vm(setloc) {
  U n = getnum((ob) GF(ip));
  // + 1 for the stack slot
  Have(n + Width(struct tag) + 1);
  mo t = setw(mo_ini(hp, n), nil, n);
  return
    hp += n + Width(struct tag),
    *--sp = (ob) t,
    ApN(2, xp); }

// late binding
// TODO dynamic type checking here
Vm(late) {
  ob w = (ob) GF(ip), d = A(w);
  xp = B(w);
  w = tbl_get(v, (tbl) d, xp, 0); // FIXME call name resolve procedure
  if (!w) return Yield(NameError, xp);
  xp = w;
  // omit the arity check if possible
  vm *n = G(FF(ip));
  if ((n == call || n == rec) && // xp will be a hom
      G(xp) == arity &&
      (ob) GF(FF(ip)) >= (ob) GF(xp))
    xp = (ob) FF(ip);
  return
    G(ip) = imm,
    GF(ip) = (vm*) xp,
    ApN(2, xp); }

// varargs
Vm(varg0) {
  Have1();
  return
    fp = cpyw_l2r((ob*) fp - 1, fp, Width(struct frame) + fp->argc),
    sp = (ob*) fp,
    fp->argv[fp->argc++] = nil,
    ApN(2, xp); }

Vm(varg) {
  U reqd = getnum((ob) GF(ip));
  if (reqd == fp->argc) return ApC(varg0, xp);
  if (reqd > fp->argc) return Yield(ArityError, putnum(reqd));
  U vdic = fp->argc - reqd;
  // in this case we need to add another argument
  // slot to hold the nil.
  // in this case we just keep the existing slots.
  Have(Width(struct two) * vdic);
  two t = (two) hp;
  hp += Width(struct two) * vdic;
  for (U i = vdic; i--;
    two_ini(t + i, fp->argv[reqd + i], (ob) (t + i + 1)));
  t[vdic-1].b = nil;
  fp->argv[reqd] = (ob) t;
  return ApN(2, xp); }

// math functions
// frameless
Vm(add) { return ApN(1, xp + *sp++ - 1); }
Vm(sub) { return ApN(1, *sp++ - xp + 1); }
Vm(mul) { return ApN(1, putnum(getnum(*sp++) * getnum(xp))); }
Vm(neg) { return ApN(1, ~xp+3); }

Vm(quot) { return xp == putnum(0) ? Yield(DomainError, xp) :
  ApN(1, putnum(getnum(*sp++) / getnum(xp))); }

Vm(rem) { return xp == putnum(0) ? Yield(DomainError, xp) :
  ApN(1, putnum(getnum(*sp++) % getnum(xp))); }

Vm(sar) { return ApN(1, putnum(getnum(*sp++) >> getnum(xp))); }
Vm(sal) { return ApN(1, putnum(getnum(*sp++) << getnum(xp))); }
Vm(bor) { return ApN(1, xp | *sp++); }
Vm(band) { return ApN(1, xp & *sp++); }
Vm(bxor) { return ApN(1, (xp ^ *sp++) | 1); }
Vm(bnot) { return ApN(1, ~xp | 1); }

// framed
// FIXME do type checks
Vm(add_f) {
  xp = 0;
  for (U i = 0; i < fp->argc; xp += getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }
Vm(mul_f) {
  xp = 1;
  for (U i = 0; i < fp->argc; xp *= getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }

Vm(sub_f) {
  if (fp->argc == 0) return ApC(ret, xp);
  if (fp->argc == 1) return ApC(ret, putnum(-getnum(fp->argv[0])));
  xp = getnum(fp->argv[0]);
  U i = 1;
  do xp -= getnum(fp->argv[i++]); while (i < fp->argc);
  return ApC(ret, putnum(xp)); }

Vm(quot_f) {
  if (fp->argc == 0) return ApC(ret, putnum(1));
  xp = getnum(fp->argv[0]);
  for (U i = 1; i < fp->argc; i++) {
    I n = getnum(fp->argv[i]);
    Check(n);
    xp /= n; }
  return ApC(ret, putnum(xp)); }

Vm(rem_f) {
  if (fp->argc == 0) return ApC(ret, putnum(1));
  xp = getnum(fp->argv[0]);
  for (U i = 1; i < fp->argc; i++) {
    I n = getnum(fp->argv[i]);
    Check(n);
    xp %= n; }
  return ApC(ret, putnum(xp)); }

Vm(bor_f) {
  xp = 0;
  for (U i = 0; i < fp->argc; xp |= getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }

Vm(bxor_f) {
  xp = 0;
  for (U i = 0; i < fp->argc; xp ^= getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }

Vm(band_f) {
  xp = -1;
  for (U i = 0; i < fp->argc; xp &= getnum(fp->argv[i++]));
  return ApC(ret, putnum(xp)); }

Vm(bnot_f) { return
  xp = fp->argc ? *fp->argv : 0,
  ApC(ret, ~xp|1); }

Vm(sar_f) {
  if (fp->argc == 0)
    return ApC(ret, xp);
  if (fp->argc == 1)
    return ApC(ret, putnum(getnum(fp->argv[0])>>1));
  xp = getnum(fp->argv[0]);
  U i = 1;
  do xp >>= getnum(fp->argv[i++]);
  while (i < fp->argc);
  return ApC(ret, putnum(xp)); }

Vm(sal_f) {
  if (fp->argc == 0) return ApC(ret, xp);
  if (fp->argc == 1) return ApC(ret, putnum(getnum(fp->argv[0])<<1));
  xp = getnum(fp->argv[0]);
  U i = 1;
  do xp <<= getnum(fp->argv[i++]); while (i < fp->argc);
  return ApC(ret, putnum(xp)); }

Vm(rand_f) { return
  v->rand = lcprng(v->rand),
  ApC(ret, putnum(v->rand)); }

Vm(yield) { return Pack(),
  v->yield ? v->yield(v, v->status) : v->status; }

Vm(xok) { return Yield(Ok, xp); }
Vm(xdom) { return Yield(DomainError, xp); }
Vm(gc) {
  size_t req = v->xp;
  CallOut(req = please(v, req));
  return req ? ApY(ip, xp) : Yield(OomError, xp); }
