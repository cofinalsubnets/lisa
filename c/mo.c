#include "la.h"

// instructions used by the compiler
Ll(hom_u) {
  Arity(1);
  ob x = *fp->argv;
  TypeCheck(x, Num);
  intptr_t len = getnum(x) + 2;
  Have(len);
  mo k = (mo) hp;
  return hp += len,
         setw((ob*) k, nil, len),
         k[len-1].ll = (ll*) k,
         k[len-2].ll = NULL,
         ApC(ret, (ob) (k+len-2)); }

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
  return ApC(ret, homnom(v, *fp->argv)); }
