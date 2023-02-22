#include "i.h"

//symbols

// FIXME this should probably change at some point.
// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle which seems to keep it
// from getting too bad. this is much more performant than a
// list & uses less memory than a hash table, but maybe we
// should use a table anyway.
//

// FIXME the caller must ensure Avail >= Width(struct sym)
// (because GC here would void the tree)
static sym intern(li v, sym *y, str b) {
  if (*y) {
    sym z = *y;
    str a = z->nom;
    int i = strncmp(a->text, b->text,
      a->len < b->len ? a->len : b->len);
    if (i == 0) {
      if (a->len == b->len) return z;
      i = a->len < b->len ? -1 : 1; }
    return intern(v, i < 0 ? &z->l : &z->r, b); }
  return *y = ini_sym(bump(v, Width(struct sym)), b,
    hash(v, putnum(hash(v, (ob) b)))); }

NoInline sym symof(li v, str s) {
  if (Avail < Width(struct sym)) {
    bool _; with(s, _ = please(v, Width(struct sym)));
    if (!_) return 0; }
  return intern(v, &v->syms, s); }

NoInline sym nym(li v) { return
  Avail < Width(struct sym) - 2 &&
  !please(v, Width(struct sym) - 2) ? 0 :
    ini_anon(bump(v, Width(struct sym) - 2),
             v->rand = liprng(v)); }

Vm(ynom_f) {
  if (fp->argc && symp(fp->argv[0]))
    xp = (ob) ((sym) fp->argv[0])->nom,
    xp = xp ? xp : nil;
  return ApC(ret, xp); }

Vm(sym_f) {
  Have(Width(struct sym));
  str i = fp->argc && strp(fp->argv[0]) ? (str) fp->argv[0] : 0;
  sym y; return
    CallOut(y = i ?
      intern(v, &v->syms, i) :
      nym(v)),
    ApC(ret, (ob) y); }
