#include "la.h"

// bootstrap eval interpreter function
Ll(ev_u) {
  Ary(1);
  mo y; return
    // check to see if ev has been overridden in the
    // toplevel namespace and if so call that. this way
    // ev calls compiled pre-bootstrap will use the
    // bootstrapped compiler, which is what we want?
    // seems kind of strange to need this ...
    xp = refer(v, v->lex[Eval]),
    xp && homp(xp) && gethom(xp)->ll != ev_u ?
      ApY((mo) xp, nil) :
      // otherwise use the bootstrap compiler.
      !(Pack(), y = ana(v, *fp->argv, putnum(ret))) ? 0 :
        (Unpack(), ApY(y, xp)); }

// instructions used by the compiler
Ll(hom_u) {
  Ary(1);
  xp = fp->argv[0];
  Typ(xp, Num);
  Z len = getZ(xp) + 2;
  Have(len);
  return
    xp = (ob) hp,
    hp += len,
    setw((ob*) xp, nil, len),
    R(xp)[len-2] = 0,
    R(xp)[len-1] = xp,
    ApC(ret, (ob) (R(xp)+len-2)); }

Ll(hfin_u) {
  Arity(1);
  mo k = (mo) *fp->argv;
  TypeCheck((ob) k, Hom);
  return button(k)[1].ll = (ll*) k,
         ApC(ret, (ob) k); }

Ll(emx) {
  mo k = (mo) *sp++ - 1;
  return k->ll = (ll*) xp,
         ApN(1, (ob) k); }

Ll(emi) {
  mo k = (mo) *sp++ - 1;
  return k->ll = (ll*) getnum(xp),
         ApN(1, (ob) k); }

Ll(emx_u) {
  Arity(2);
  ob x = fp->argv[0];
  mo k = (mo) fp->argv[1];
  TypeCheck((ob) k, Hom);
  return (--k)->ll = (ll*) x,
         ApC(ret, (ob) k); }

Ll(emi_u) {
  Arity(2);
  ob n = fp->argv[0];
  mo k = (mo) fp->argv[1];
  TypeCheck(n, Num);
  TypeCheck((ob) k, Hom);
  return (--k)->ll = (ll*) getnum(n),
         ApC(ret, (ob) k); }

Ll(peeki_u) {
  Arity(1);
  TypeCheck(*fp->argv, Hom);
  return ApC(ret, putnum(gethom(*fp->argv)->ll)); }

Ll(peekx_u) {
  Arity(1);
  TypeCheck(*fp->argv, Hom);
  return ApC(ret, (ob) gethom(*fp->argv)->ll); }

Ll(seek_u) {
  Arity(2);
  TypeCheck(fp->argv[0], Hom);
  TypeCheck(fp->argv[1], Num);
  return ApC(ret, fp->argv[0] + fp->argv[1] - Num); }

Ll(hnom_u) {
  Arity(1);
  TypeCheck(*fp->argv, Hom);
  return ApC(ret, hnom(v, *fp->argv)); }
