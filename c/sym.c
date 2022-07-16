#include "la.h"
#include <string.h>
//symbols

// FIXME this is bad
// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult. the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection.
ob sskc(pt v, ob *y, ob x) {
  int i;
  sym z;
  if (!nilp(*y)) {
    z = getsym(*y),
    i = strcmp(getstr(z->nom)->text, getstr(x)->text);
    return i == 0 ? *y : sskc(v, i < 0 ? &z->r : &z->l, x); }
  // FIXME the caller must ensure Avail >= Width(sym)
  // (because GC here would void the tree)
  z = cells(v, Width(sym)),
  z->code = hash(v, putZ(hash(v, z->nom = x))),
  z->l = z->r = nil;
  return *y = putsym(z); }

ob intern(pt v, ob x) {
  bool _; return
    Avail >= Width(sym) ||
    (with(x, _ = please(v, Width(sym))), _) ?
      sskc(v, &v->syms, x) : 0; }

Vm(sym_u) {
  sym y;

  if (fp->argc > N0 && strp(*fp->argv)) {
    Pack(),
    v->xp = intern(v, *fp->argv);
    Unpack();
    return ApC(xp ? ret : oom_err, xp); }

  if (sp - hp < Width(sym)) {
    v->xp = Width(sym);
    return ApC(gc, xp); }

  y = (sym) hp,
  hp += Width(sym),
  y->nom = y->l = y->r = nil,
  y->code = v->rand = lcprng(v->rand);
  return ApC(ret, putsym(y)); }

Dt(ystr_u) {
  ArityCheck(1);
  xp = *fp->argv;
  TypeCheck(xp, Sym);
  return ApC(ret, getsym(xp)->nom); }

