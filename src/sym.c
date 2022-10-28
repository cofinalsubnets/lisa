#include "lisa.h"
#include "vm.h"
#include <string.h>

struct mtbl s_mtbl_sym = { do_id, em_sym, cp_sym, hash_sym };
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
//
// FIXME the caller must ensure Avail >= Width(sym)
// (because GC here would void the tree)
static ob sskc(la v, ob *y, ob x) {
  if (!nilp(*y)) {
    sym z = (sym) *y;
    int i = scmp(((str) z->nom)->text, ((str) x)->text);
    return i == 0 ? *y : sskc(v, i < 0 ? &z->r : &z->l, x); }
  // sym allocated here
  sym z = bump(v, Width(sym));
  z->code = hash(v, putnum(hash(v, z->nom = x)));
  z->disp = disp; z->mtbl = mtbl_sym;
  z->l = z->r = nil;
  return *y = (ob) z; }

ob intern(la v, ob x) {
  bool _; return
    Avail >= Width(sym) ||
    (with(x, _ = please(v, Width(sym))), _) ?
      sskc(v, &v->syms, x) :
      0; }

Vm(sym_u) {
  Have(Width(sym));
  if (fp->argc > putnum(0) && strp(fp->argv[0]))
    return ApC(ret, sskc(v, &v->syms, fp->argv[0]));
  // sym allocated here
  sym y = (sym) hp;
  hp += Width(sym);
  y->nom = y->l = y->r = nil;
  y->disp = disp;
  y->mtbl = mtbl_sym;
  y->code = v->rand = lcprng(v->rand);
  return ApC(ret, (ob) y); }

Vm(ystr_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(symp(xp));
  return ApC(ret, ((sym) xp)->nom); }

ob interns(la v, const char *s) {
  ob _ = string(v, s);
  return _ ? intern(v, _) : 0; }

Gc(cp_sym) {
  sym src = (sym) x, dst;
  ob nom = src->nom;
  if (nilp(nom))
    dst = bump(v, Width(sym)),
    cpyw(dst, src, Width(sym));
  else 
    x = cp(v, nom, pool0, top0),
    dst = (sym) sskc(v, &v->syms, x);
  src->disp = (vm*) dst;
  return (ob) dst; }

size_t hash_sym(la v, ob x) { return ((sym) x)->code; }

void em_sym(la v, FILE *o, ob x) {
  sym y = (sym) x;
  x = y->nom;
  strp(x) ? fputs(((str)x)->text, o) :
            fprintf(o, "#sym@%lx", (long) y); }

Vm(do_id) { return ApC(ret, (ob) ip); }

bool symp(ob _) { return homp(_) && GF(_) == (vm*) mtbl_sym; }
