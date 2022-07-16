#include "la.h"

// instructions used by the compiler
Ll(hom_u) {
  Ary(1);
  xp = fp->argv[0];
  TypeCheck(xp, Num);
  uintptr_t len = getZ(xp) + 2;
  if (Slack < len) return Pray(len);
  xp = (ob) hp;
  hp += len;
  setw((ob*) xp, nil, len);
  ptr(xp)[len-2] = 0;
  ptr(xp)[len-1] = xp;
  return ApC(ret, (ob) (ptr(xp) + len - 2)); }

Ll(hfin_u) {
  ArityCheck(1);
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
  ArityCheck(2);
  ob x = fp->argv[0];
  mo k = (mo) fp->argv[1];
  TypeCheck((ob) k, Hom);
  return (--k)->ll = (ll*) x,
         ApC(ret, (ob) k); }

Ll(emi_u) {
  ArityCheck(2);
  ob n = fp->argv[0];
  mo k = (mo) fp->argv[1];
  TypeCheck(n, Num);
  TypeCheck((ob) k, Hom);
  return (--k)->ll = (ll*) getnum(n),
         ApC(ret, (ob) k); }

Ll(peeki_u) {
  ArityCheck(1);
  TypeCheck(*fp->argv, Hom);
  return ApC(ret, putnum(gethom(*fp->argv)->ll)); }

Ll(peekx_u) {
  ArityCheck(1);
  TypeCheck(*fp->argv, Hom);
  return ApC(ret, (ob) gethom(*fp->argv)->ll); }

Ll(seek_u) {
  ArityCheck(2);
  TypeCheck(fp->argv[0], Hom);
  TypeCheck(fp->argv[1], Num);
  return ApC(ret, fp->argv[0] + fp->argv[1] - Num); }

Ll(hnom_u) {
  ArityCheck(1);
  TypeCheck(*fp->argv, Hom);
  return ApC(ret, hnom(v, *fp->argv)); }

ob hnom(pt v, ob x) {
  host *k = gethom(x)->ll;
  if (k == clos || k == clos0 || k == clos1)
    return hnom(v, (ob) gethom(x)[2].ll);
  ob* h = (ob*) gethom(x);
  while (*h) h++;
  x = h[-1];
  int inb = (ob*) x >= v->pool && (ob*) x < v->pool+v->len;
  return inb ? x : nil; }
