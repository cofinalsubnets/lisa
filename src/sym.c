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
static sym sym_ini(void *_, str nom, U code) {
  sym y = _; return
    y->act = act,
    y->typ = &sym_typ,
    y->nom = nom,
    y->code = code,
    y->l = y->r = 0,
    y; }

static sym ini_anon(void *_, U code) {
  sym y = _;
  y->act = act;
  y->typ = &sym_typ;
  y->nom = 0;
  y->code = code;
  return y; }

// FIXME the caller must ensure Avail >= Width(struct sym)
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
  return *y = sym_ini(bump(v, Width(struct sym)), b,
    hash(v, putnum(hash(v, (ob) b)))); }

sym symof(la v, str s) {
  if (Avail < Width(struct sym)) {
    bool _; with(s, _ = please(v, Width(struct sym)));
    if (!_) return 0; }
  return intern(v, &v->syms, s); }

static Gc(cp_sym) {
  sym src = (sym) x;
  return (ob) (src->act = (vm*) (src->nom ?
    intern(v, &v->syms, (str) cp(v, (ob) src->nom, pool0, top0)) :
    ini_anon(bump(v, Width(struct sym) - 2), src->code))); }

Vm(ynom_f) {
  if (fp->argc && symp(fp->argv[0]))
    xp = (ob) ((sym) fp->argv[0])->nom,
    xp = xp ? xp : nil;
  return ApC(ret, xp); }

Vm(sym_f) {
  Have(Width(struct sym));
  str i = fp->argc && strp(fp->argv[0]) ? (str) fp->argv[0] : 0;
  sym y;
  CallOut(y = i ?
    intern(v, &v->syms, i) :
    ini_anon(bump(v, Width(struct sym) - 2),
      v->rand = liprng(v)));
  return ApC(ret, (ob) y); }

static void fputsn(const char *s, U n, FILE *o) {
  while (n--) putc(*s++, o); }
static void tx_sym(la v, FILE* o, ob _) {
  str s = ((sym) _)->nom;
  s ? fputsn(s->text, s->len, o) : fputs("#sym", o); }
static uintptr_t hx_sym(la v, ob _) {
  return ((sym) _)->code; }
const struct typ sym_typ = {
  .actn = immk,
  .emit = tx_sym,
  .evac = cp_sym,
  .hash = hx_sym,
  .equi = neql, };
