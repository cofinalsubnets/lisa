#include "la.h"

// hash tables
Vm(tblg) {
  ArityCheck(2);
  TypeCheck(fp->argv[0], Tbl);
  return xp = tbl_get(v, fp->argv[0], fp->argv[1]),
         ApC(ret, xp ? xp : nil); }

Op(1, tget, (xp = tbl_get(v, xp, *sp++)) ? xp : nil)
Op(1, thas, tbl_get(v, xp, *sp++) ? T : nil)
Op(1, tlen, putnum(gettbl(xp)->len))

static ob tks_j(pt v, ob e, ob l) {
  ob x; return e == nil ? l :
    (x = R(e)[0],
     with(x, l = tks_j(v, R(e)[2], l)),
     l ? pair(v, x, l) : 0); }

static ob tks_i(pt v, ob t, Z i) {
  ob k; return i == 1 << gettbl(t)->cap ? nil :
    (with(t, k = tks_i(v, t, i+1)),
     k ? tks_j(v, gettbl(t)->tab[i], k) : 0); }

Vm(tkeys) { return
  Pack(),
  v->xp = tks_i(v, xp, 0),
  Unpack(),
  xp ? ApN(1, xp) : ApC(oom_err, xp); }

Vm(tblc) {
  ArityCheck(2);
  TypeCheck(fp->argv[0], Tbl);
  return xp = tbl_get(v, fp->argv[0], fp->argv[1]),
         ApC(ret, xp ? T : nil); }

static ob tblss(pt v, Z i, Z l) {
  fr fp = (fr) v->fp;
  return
    i > l - 2 ? fp->argv[i - 1] :
    !tbl_set(v, v->xp, fp->argv[i], fp->argv[i + 1]) ? 0 :
    tblss(v, i + 2, l); }

Vm(tbls) {
  ArityCheck(1);
  TypeCheck(*fp->argv, Tbl);
  return
    xp = *fp->argv,
    Pack(),
    v->xp = tblss(v, 1, getZ(fp->argc)),
    Unpack(),
    ApC(xp ? ret : oom_err, xp); }

Vm(tblmk) {
  Pack();
  bool _ = (v->xp = table(v)) && tblss(v, 0, getZ(fp->argc));
  Unpack();
  return
    ApC(_ ? ret : oom_err, xp); }

Vm(tblks) {
  ArityCheck(1);
  TypeCheck(*fp->argv, Tbl);
  Pack();
  v->xp = tks_i(v, *fp->argv, 0);
  Unpack();
  return ApC(xp ? ret : oom_err, xp); }

Vm(tbll) {
  ArityCheck(1);
  TypeCheck(*fp->argv, Tbl);
  return ApC(ret, putnum(gettbl(*fp->argv)->len)); }

Vm(tset) {
  ob x = *sp++, y = *sp++;
  Pack();
  v->xp = tbl_set(v, xp, x, y);
  Unpack();
  return xp ? ApN(1, xp) : ApC(oom_err, xp); }


static Inline N tbl_idx(N cap, N co) {
  return co & ((1 << cap) - 1); }

static Inline uintptr_t tbl_load(ob t) {
  return gettbl(t)->len >> gettbl(t)->cap; }

static ob tbl_ent_(la v, ob e, ob k) {
  return e == nil || eql(R(e)[0], k) ? e :
    tbl_ent_(v, R(e)[2], k); }

static ob tbl_ent(la v, ob u, ob k) {
  tbl t = gettbl(u);
  u = t->tab[tbl_idx(t->cap, hash(v, k))];
  return tbl_ent_(v, u, k); }

static const uintptr_t mix = 2708237354241864315;

static Inline uintptr_t ror(uintptr_t x, uintptr_t n) {
  return (x<<((8*sizeof(uintptr_t))-n))|(x>>n); }

uintptr_t hash(pt v, ob x) {
  switch (Q(x)) {
    case Sym:
      return getsym(x)->code;
    case Two:
      return ror(hash(v, A(x)) * hash(v, B(x)), 32);
    case Hom:
      return hash(v, hnom(v, x)) ^ mix;
    case Tbl:
      return ror(mix * Tbl, 48);
    case Str: {
      str s = getstr(x);
      uintptr_t len = s->len;
      char *us = s->text;
      for (uintptr_t h = 1;; h ^= *us++, h *= mix)
        if (!len--) return h; }
    default:
      return ror(mix * x, 16); } }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static void tbl_fit(la v, ob t) {
  if (tbl_load(t)) return;

  ob e = nil, f, g;
  tbl u = gettbl(t);

  // collect all entries
  for (uintptr_t i = 1 << u->cap; i--;)
    for (f = u->tab[i], u->tab[i] = nil; f != nil;
      g = R(f)[2], R(f)[2] = e,
      e = f, f = g);

  // shrink bucket array
  while (u->cap && tbl_load(t) < 1) u->cap--;

  // reinsert
  while (e != nil) {
    uintptr_t i = tbl_idx(u->cap, hash(v, R(e)[0]));
    f = R(e)[2],
    R(e)[2] = u->tab[i],
    u->tab[i] = e,
    e = f; } }

static ob tbl_del(la v, ob t, ob key) {
  tbl y = gettbl(t);
  ob val = nil;
  intptr_t b = tbl_idx(y->cap, hash(v, key));
  ob e = y->tab[b],
     prev[] = {0,0,e};
  for (ob l = (ob) &prev; l != nil && R(l)[2] != nil; l = R(l)[2])
    if (R(R(l)[2])[0] == key) {
      val = R(R(l)[2])[1];
      R(l)[2] = R(R(l)[2])[2];
      y->len--;
      break; }
  return y->tab[b] = prev[2], tbl_fit(v, t), val; }


// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static ob tbl_grow(la v, ob t) {
  ob *tab0, *tab1;
  N cap0 = gettbl(t)->cap, cap1 = cap0 + 1,
    len = 1<<cap1;
  with(t, tab1 = cells(v, len + 2));
  if (!tab1) return 0;
  tab1[len] = 0, tab1[len+1] = (ob) tab1;
  setw(tab1, nil, 1<<cap1);
  tab0 = gettbl(t)->tab;

  for (N i, cap = 1 << cap0; cap--;)
    for (ob e, es = tab0[cap]; es != nil;)
      e = es,
      es = R(es)[2],
      i = tbl_idx(cap1, hash(v, R(e)[0])),
      R(e)[2] = tab1[i],
      tab1[i] = e;

  return gettbl(t)->cap = cap1, gettbl(t)->tab = tab1, t; }

static ob tbl_set_s(la v, ob t, ob k, ob x) {
  tbl y;
  ob *e = (ob*) tbl_ent(v, t, k);
  N i = tbl_idx(gettbl(t)->cap, hash(v, k));
  return (ob) e != nil ? e[1] = x :
    (with(t, with(k, with(x, e = cells(v, 5)))),
     !e ? 0 : (y = gettbl(t),
               e[0] = k,
               e[1] = x,
               e[2] = (ob) y->tab[i],
               e[3] = 0,
               e[4] = (ob) e,
               y->tab[i] = (ob) e,
               y->len += 1,
               x)); }

ob tbl_set(la v, ob t, ob k, ob x) { return
  (with(t, x = tbl_set_s(v, t, k, x)), !x) ? 0 :
  tbl_load(t) <= 1 ? x :
  (with(x, t = tbl_grow(v, t)), t ? x : 0); }

ob tbl_get(pt v, ob t, ob k) {
  ob e = tbl_ent(v, t, k);
  return e == nil ? 0 : R(e)[1]; }

ob table(pt v) {
  tbl t = cells(v, Width(tbl) + 3);
  ob *b = (ob*)(t+1);
  if (!t) return 0;
  t->len = t->cap = 0;
  t->tab = b;
  b[0] = nil;
  b[1] = 0;
  b[2] = (ob) b;
  return puttbl(t); }

Vm(tbld) {
  Ary(2);
  TypeCheck(fp->argv[0], Tbl);
  Pack(),
  v->xp = tbl_del(v, fp->argv[0], fp->argv[1]),
  Unpack();
  return ApC(ret, xp); }
