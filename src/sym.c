#include "la.h"
#include <string.h>

//symbols
//
static sym ini_anon(void *_, size_t code) {
  sym y = _;
  y->h.disp = disp;
  y->h.mtbl = &mtbl_sym;
  y->nom = 0;
  y->code = code;
  return y; }

static sym ini_sym(void *_, str nom, size_t code) {
  sym y = _;
  y->h.disp = disp;
  y->h.mtbl = &mtbl_sym;
  y->nom = nom;
  y->code = code;
  y->l = y->r = 0;
  return y; }

// FIXME this should probably change at some point.
// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle which seems to keep it
// from getting too bad. this is much more performant than a
// list & uses less memory than a hash table, but maybe we
// should use a table anyway.
//
// FIXME the caller must ensure Avail >= wsizeof(struct sym)
// (because GC here would void the tree)
static sym intern(la v, sym *y, str b) {
  if (*y) {
    sym z = *y;
    str a = z->nom;
    int i = strncmp(a->text, b->text,
      a->len < b->len ? a->len : b->len);
    if (i == 0) {
      if (a->len == b->len) return z;
      i = a->len < b->len ? -1 : 1; }
    return intern(v, i < 0 ? &z->l : &z->r, b); }
  return *y = ini_sym(bump(v, wsizeof(struct sym)), b,
    hash(v, putnum(hash(v, (ob) b)))); }

sym symof(la v, str s) {
  if (Avail < wsizeof(struct sym)) {
    bool _;
    with(s, _ = please(v, wsizeof(struct sym)));
    if (!_) return 0; }
  return s ? intern(v, &v->syms, s) :
    ini_anon(bump(v, wsizeof(struct sym) - 2), v->rand = lcprng(v->rand)); }

Vm(sym_f) {
  str i = fp->argc && strp(fp->argv[0]) ? (str) fp->argv[0] : 0;
  sym y;
  CallOut(y = symof(v, i));
  return y ? ApC(ret, (ob) y) : ApC(xoom, xp); }

Vm(ynom_f) {
  if (fp->argc && symp(fp->argv[0])) {
    xp = (ob) ((sym) fp->argv[0])->nom;
    xp = xp ? xp : nil; }
  return ApC(ret, xp); }

static Gc(cp_sym) {
  sym src = (sym) x;
  return (ob) (src->h.disp = (vm*) (src->nom ?
    intern(v, &v->syms, (str) cp(v, (ob) src->nom, pool0, top0)) :
    ini_anon(bump(v, wsizeof(struct sym) - 2), src->code))); }

static intptr_t hx_sym(la v, ob _) { return ((sym) _)->code; }

static void tx_sym(la v, la_io o, ob _) {
  str s = ((sym) _)->nom;
  s ? la_putsn(s->text, s->len, o) : la_puts("#sym", o); }

static Vm(ap_nop) { return ApC(ret, (ob) ip); }

const struct mtbl mtbl_sym = {
  .does = ap_nop,
  .emit = tx_sym,
  .evac = cp_sym,
  .hash = hx_sym,
  .equi = neql, };
