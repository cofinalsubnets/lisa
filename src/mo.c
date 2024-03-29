#include "i.h"

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
    size_t i = fp->argc - 1;
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
