#include "la.h"
#include <string.h>

//symbols
static Inline sym ini_sym(void *_, ob nom, size_t code) {
  sym y = _;
  y->disp = disp, y->mtbl = mtbl_sym, y->nom = nom;
  y->code = code, y->l = y->r = 0;
  return y; }

// FIXME this is bad
// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult. the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection.
//
// FIXME the caller must ensure Avail >= Width(sym)
// (because GC here would void the tree)
static ob sskc(la v, ob *y, ob x) {
  if (*y) {
    sym z = (sym) *y;
    str a = (str) z->nom, b = (str) x;
    size_t n = a->len < b->len ? a->len : b->len;
    int i = strncmp(a->text, b->text, n);
    return i == 0 ? (ob) z :
      sskc(v, i < 0 ? &z->r : &z->l, x); }
  return *y = (ob) ini_sym(bump(v, Width(sym)), x,
    hash(v, putnum(hash(v, x)))); }

ob intern(la v, ob x) {
  if (Avail < Width(sym)) {
    bool _;
    with(x, _ = please(v, Width(sym)));
    if (!_) return 0; }
  return sskc(v, &v->syms, x); }

Vm(sym_u) {
  Have(Width(sym));
  if (fp->argc && strp(fp->argv[0]))
    return ApC(ret, sskc(v, &v->syms, fp->argv[0]));
  xp = (ob) ini_sym(hp, nil, v->rand = lcprng(v->rand));
  hp += Width(sym);
  return ApC(ret, xp); }

Vm(ystr_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(symp(xp));
  return ApC(ret, ((sym) xp)->nom); }

ob interns(la v, const char *s) {
  ob _ = string(v, s);
  return _ ? intern(v, _) : 0; }

static Gc(cp_sym) {
  sym src = (sym) x,
      dst = nilp(src->nom) ?
        cpyw(bump(v, Width(sym)), src, Width(sym)) :
        (sym) sskc(v, &v->syms, cp(v, src->nom, pool0, top0));
  return (ob) (src->disp = (vm*) dst); }

static size_t hash_sym(la v, ob x) { return ((sym) x)->code; }

static int em_sym(la v, FILE *o, ob x) {
  sym y = (sym) x;
  x = y->nom;
  if (!strp(x)) return fprintf(o, "#sym@%lx", (long) y);
  str s = (str) x;
  return femit(o, s->len - 1, 0, s->text, 0, 0); }

Vm(do_id) { return ApC(ret, (ob) ip); }

const struct mtbl s_mtbl_sym = {
  .does = do_id,
  .emit = em_sym,
  .copy = cp_sym,
  .hash = hash_sym,
  .equi = eq_no, };
