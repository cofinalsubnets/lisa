#include "la.h"
#include "alloc.h"
#include "gc.h"
#include "sym.h"
#include "hash.h"
#include "cmp.h"
#include "num.h"
#include "str.h"
#include <string.h>

//symbols
static Inline sym ini_sym(void *_, str nom, size_t code) {
  sym y = _;
  y->head.disp = disp;
  y->head.mtbl = &mtbl_sym;
  y->nom = nom;
  y->code = code;
  y->l = y->r = 0;
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
// FIXME the caller must ensure Avail >= wsizeof(struct sym)
// (because GC here would void the tree)
static sym sskc(la v, sym *y, str b) {
  if (*y) {
    sym z = *y;
    str a = z->nom;
    int i = strncmp(a->text, b->text,
      a->len < b->len ? a->len : b->len);
    if (i == 0) {
      if (a->len == b->len) return z;
      i = a->len < b->len ? -1 : 1; }
    return sskc(v, i < 0 ? &z->l : &z->r, b); }
  return *y = ini_sym(bump(v, wsizeof(struct sym)), b,
    hash(v, putnum(hash(v, (ob) b)))); }

sym symof(la v, str s) {
  if (Avail < wsizeof(struct sym)) {
    bool _;
    with(s, _ = please(v, wsizeof(struct sym)));
    if (!_) return 0; }
  return s ? sskc(v, &v->syms, s) :
    ini_sym(bump(v, wsizeof(struct sym)), s,
      v->rand = lcprng(v->rand)); }

#include "vm.h"
Vm(sym_f) {
  str i = fp->argc && strp(fp->argv[0]) ? (str) fp->argv[0] : 0;
  sym y;
  CallOut(y = symof(v, i));
  return y ? ApC(ret, (ob) y) : ApC(xoom, xp); }

Vm(ynom_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(symp(xp));
  str s = ((sym) xp)->nom;
  return ApC(ret, s ? (ob) s : nil); }

static Gc(cp_sym) {
  sym src = (sym) x,
      dst = src->nom ?
        sskc(v, &v->syms, (str) cp(v, (ob) src->nom, pool0, top0)) :
        ini_sym(bump(v, wsizeof(struct sym)), 0, src->code);
  src->head.disp = (vm*) dst;
  return (ob) dst; }

static intptr_t hx_sym(la v, ob _) { return ((sym) _)->code; }

#include "tx.h"
static long tx_sym(la v, FILE *o, ob _) {
  str s = ((sym) _)->nom;
  return s ? fputstr(o, s) : fprintf(o, "#sym"); }

static Vm(ap_nop) { return ApC(ret, (ob) ip); }

const struct mtbl mtbl_sym = {
  .does = ap_nop,
  .emit = tx_sym,
  .evac = cp_sym,
  .hash = hx_sym,
  .equi = neql, };
