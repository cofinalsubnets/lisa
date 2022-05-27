#include "la.h"

// hash tables
Vm(tblg) {
  Arity(2);
  TypeCheck(fp->argv[0], Tbl);
  return xp = tbl_get(v, fp->argv[0], fp->argv[1]),
         ApC(ret, xp ? xp : nil); }

OP1(tget, (xp = tbl_get(v, xp, *sp++)) ? xp : nil)
OP1(thas, tbl_get(v, xp, *sp++) ? T : nil)
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
         ApC(ret, xp ? T : nil); }

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

