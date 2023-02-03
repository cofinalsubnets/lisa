#include "i.h"


Vm(txc_f) { return
  !fp->argc ?  Yield(ArityError, putnum(1)) :
  ApC(ret, putnum(putc(getnum(fp->argv[0]), stdout))); }

Vm(tx_f) {
  size_t i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      transmit(v, stdout, fp->argv[i++]),
      putc(' ', stdout);
    xp = fp->argv[i];
    transmit(v, stdout, xp); }
  return
    putc('\n', stdout),
    ApC(ret, xp); }

Vm(rxc_f) { return ApC(ret, putnum(getc(stdin))); }

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

Vm(vm_yield) {
  enum status r = v->xp;
  return Pack(), r; }
// type/arity checking
Vm(idno) { return nump(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idmo) { return homp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idtbl) { return tblp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idtwo) { return twop(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(arity) { return
  fp->argc >= getnum(GF(ip)) ? ApN(2, xp) :
    Yield(ArityError, (ob) GF(ip)); }
Vm(ary1) { return
  fp->argc ? ApN(1, xp) :
    Yield(ArityError, putnum(1)); }
Vm(ary2) { return
  fp->argc >= 2 ? ApN(1, xp) :
    Yield(ArityError, putnum(2)); }
Vm(ary3) { return
  fp->argc >= 3 ? ApN(1, xp) :
    Yield(ArityError, putnum(3)); }
Vm(ary4) { return
  fp->argc >= 4 ? ApN(1, xp) :
    Yield(ArityError, putnum(4)); }

Vm(rand_f) { return ApC(ret, putnum(liprng(v))); }

// fixnum arithmetic
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

Vm(gc) { size_t req = v->xp; return
  CallOut(req = please(v, req)),
  req ? ApY(ip, xp) : Yield(OomError, xp); }

// instructions for the internal compiler
//
// vm interface to mo_n
Vm(hom_f) {
  if (fp->argc && nump(fp->argv[0])) {
    size_t len = getnum(fp->argv[0]);
    Have(len + Width(struct tag));
    mo k = setw(mo_ini(hp, len), nil, len);
    hp += len + Width(struct tag);
    xp = (ob) (k + len); }
  return ApC(ret, xp); }

// trim a function after writing out code
Vm(hfin_f) {
  if (fp->argc) {
    ob x = fp->argv[0];
    if (homp(x) && G(x) != act)
      mo_tag((mo) x)->head = (mo) x,
      xp = x; }
  return ApC(ret, xp); }

// emit data
Vm(poke_f) {
  if (fp->argc) {
    U i = fp->argc - 1;
    if (homp(fp->argv[i])) {
      mo k = (mo) fp->argv[i];
      while (i--) G(--k) = (vm*) fp->argv[i];
      xp = (ob) k; } }
  return ApC(ret, xp); }

Vm(peek_f) {
  if (fp->argc && homp(fp->argv[0])) xp = (ob) G(fp->argv[0]);
  return ApC(ret, xp); }

// thread pointer arithmetic. not bounds checked.
Vm(seek_f) {
  if (fp->argc >= 2 && homp(fp->argv[0]) && nump(fp->argv[1]))
    xp = (ob) ((mo) fp->argv[0] + getnum(fp->argv[1]));
  return ApC(ret, xp); }

Vm(act) { return ApC(((typ) GF(ip))->actn, xp); }

// closure functions
//
// pop some things off the stack into an array.
Vm(take) {
  ob n = getnum((ob) GF(ip));
  Have(n + Width(struct tag));
  mo k = mo_ini(cpyw_r2l(hp, sp, n), n);
  hp += n + Width(struct tag);
  return ApC(ret, (ob) k); }

// set the closure for this frame
Vm(setclo) { return
  fp->clos = (ob*) GF(ip),
  ApY(G(FF(ip)), xp); }

// finalize function instance closure
Vm(genclo1) { return
  G(ip) = setclo,
  GF(ip) = (vm*) xp,
  ApY(ip, xp); }

struct clo_env { mo cons; ob loc, *clo, argc, argv[]; };

Vm(genclo0) {
  struct clo_env *ec = (void*) GF(ip);
  size_t adic = getnum(ec->argc);
  Have(Width(struct frame) + adic + 1);
  frame subd = fp;
  return
    G(ip) = genclo1,
    sp = (ob*) (fp = (sf) (sp - adic) - 1),
    cpyw_r2l(fp->argv, ec->argv, adic),
    fp->retp = ip,
    fp->subd = subd,
    fp->argc = adic,
    fp->clos = (ob*) ec->clo,
    *--sp = ec->loc,
    ApY(ec->cons, xp); }

// the next few functions create and store
// lexical environments.
Vm(enclose) {
  size_t thd_len = 3 + Width(struct tag),
         env_len = fp->argc + Width(struct tag) +
                              Width(struct clo_env);
  Have(env_len + thd_len);
  ob codeXcons = (ob) GF(ip), // pair of the compiled thread & closure constructor
     *block = hp;
  hp += env_len + thd_len;

  struct clo_env *env = (void*)
    mo_ini(block, Width(struct clo_env) + fp->argc); // holds the closure environment & constructor
  env->cons = (mo) B(codeXcons);
     // TODO get closure out of stack frame; configure via xp
  env->loc = nilp(xp) ? xp : ((ob*)fp)[-1];
  env->clo = fp->clos;
  env->argc = putnum(fp->argc);
  cpyw_r2l(env->argv, fp->argv, fp->argc);

  mo thd = mo_ini(block + env_len, 3); // the thread that actually gets returned
  G(thd) = genclo0;
  GF(thd) = (vm*) env;
  G(FF(thd)) = (vm*) A(codeXcons);

  return ApN(2, (ob) thd); }

// these pass the locals array to encl in xp
// TODO do the same thing with the closure ptr
Vm(encl1) { return ApC(enclose, putnum(1)); }
// FIXME if there are no locals we don't need to defer closure construction!
Vm(encl0) { return ApC(enclose, putnum(0)); }
