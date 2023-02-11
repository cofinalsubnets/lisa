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

Vm(yield_status) {
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

Vm(act) { return ApC(gettyp(ip)->does, xp); }

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

Vm(ev_f) {
  mo k = (mo) tbl_get(v, v->lex->topl, (ob) v->lex->eval, 0);
  return
    k && G(k) != ev_f ? ApY(k, xp) :
    !fp->argc ? ApC(ret, xp) :
    (CallOut(k = ana(v, fp->argv[0])), !k) ? Yield(OomError, xp) :
    ApY(k, xp); }

////
/// Load Instructions
//

// immediate values

Vm(do_id) { return ApC(ret, (ob) ip); }

Vm(imm) { return ApN(2, (ob) GF(ip)); }
Vm(imm0) { return ApN(1, putnum(0)); }
Vm(imm1) { return ApN(1, putnum(1)); }
Vm(immn1) { return ApN(1, putnum(-1)); }
Vm(immp) { Have1(); return
  xp = *--sp = (ob) GF(ip),
  ApN(2, xp); }

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
  CallOut(_ = !!tbl_set(v, (tbl) A(GF(ip)), B(GF(ip)), xp)),
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

Vm(xok) { return Pack(), Ok; }
Vm(xdom) { return Pack(), DomainError; }

////
/// Branch Instructions
//
//
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
  frame subd = fp;
  return
    fp = (sf) sp - 1,
    sp = (ob*) fp,
    fp->argc = getnum(ip[1].ap),
    fp->retp = ip + 2,
    fp->subd = subd,
    fp->clos = (ob*) nil,
    ApY(xp, nil); }

// tail calls
Vm(rec) {
  size_t adic = getnum(GF(ip));
  // save return address
  sf subd = fp->subd;
  mo retp = fp->retp;
  return
    fp = (frame) (fp->argv + fp->argc - adic) - 1,
    cpyw_r2l(fp->argv, sp, adic),
    sp = (ob*) fp,
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
  frame subd = fp->subd;
  mo retp = fp->retp;
  fp = (frame) (fp->argv + fp->argc - adic) - 1;
  sp = (ob*) fp;
  fp->retp = retp;
  fp->argc = adic;
  fp->subd = subd;
  fp->clos = (ob*) nil;
  for (ob *i = fp->argv; adic--; *i++ = A(xp), xp = B(xp));
  return ApY(ip, nil); }

  // pairs
Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(Width(struct two));
  xp = (ob) two_ini(hp, xp, *sp++);
  hp += Width(struct two);
  return ApN(1, xp); }

Vm(car_f) {
  if (fp->argc)
    xp = fp->argv[0],
    xp = twop(xp) ? A(xp) : xp;
  return ApC(ret, xp); }

Vm(cdr_f) {
  if (fp->argc)
    xp = fp->argv[0],
    xp = twop(xp) ? B(xp) : nil;
  return ApC(ret, xp); }

Vm(cons_f) {
  if (fp->argc) {
    size_t n = Width(struct two) * (fp->argc - 1);
    Have(n);
    two w = (two) hp;
    hp += n;
    xp = fp->argv[fp->argc-1];
    for (size_t i = fp->argc - 1; i--;
      xp = (ob) two_ini(w+i, fp->argv[i], xp)); }
  return ApC(ret, xp); }

// string instructions
Vm(slen_f) { return
  fp->argc == 0 ? Yield(ArityError, putnum(1)) :
  !strp(xp = fp->argv[0]) ? Yield(DomainError, xp) :
  ApC(ret, putnum(((str) xp)->len)); }

Vm(sget_f) {
  if (fp->argc < 2) return Yield(ArityError, putnum(2));
  if (!strp(fp->argv[0])) return Yield(DomainError, xp);
  str s = (str) fp->argv[0];
  I i = getnum(fp->argv[1]);
  xp = i < 0 || i >= s->len ? nil : putnum(s->text[i]);
  return ApC(ret, xp); }

Vm(scat_f) {
  size_t sum = 0, i = 0;
  for (size_t l = fp->argc; i < l;) {
    ob x = fp->argv[i++];
    Check(strp(x));
    sum += ((str)x)->len; }
  size_t words = Width(struct str) + b2w(sum);
  Have(words);
  str d = str_ini(hp, sum);
  hp += words;
  for (str x; i--;
    x = (str) fp->argv[i],
    sum -= x->len,
    memcpy(d->text+sum, x->text, x->len));
  return ApC(ret, (ob) d); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_f) {
  if (fp->argc < 2) return Yield(ArityError, putnum(2));
  if (!strp(fp->argv[0])) return Yield(DomainError, xp);
  str src = (str) fp->argv[0];
  I lb = getnum(fp->argv[1]),
    ub = fp->argc > 2 ? getnum(fp->argv[2]) : INTPTR_MAX;
  lb = max(lb, 0);
  ub = min(ub, src->len);
  ub = max(ub, lb);
  size_t len = ub - lb,
         words = Width(struct str) + b2w(len);
  Have(words);
  str dst = str_ini(hp, len);
  hp += words;
  memcpy(dst->text, src->text + lb, len);
  return ApC(ret, (ob) dst); }

Vm(str_f) {
  size_t len = fp->argc,
         words = Width(struct str) + b2w(len);
  Have(words);
  str s = str_ini(hp, len);
  hp += words;
  while (len--) s->text[len] = getnum(fp->argv[len]);
  return ApC(ret, (ob) s); }
