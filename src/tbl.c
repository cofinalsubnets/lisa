#include "la.h"

// hash tables
// some of the worst code is here :(

#define KEY(e) ((ob*)(e))[0]
#define VAL(e) ((ob*)(e))[1]
#define NEXT(e) ((ob*)(e))[2]

static Inline tbl ini_tbl(void *_, size_t len, size_t cap, ob *tab) {
  tbl t = _;
  t->disp = disp, t->mtbl = &mtbl_tbl;
  t->len = len, t->cap = cap, t->tab = tab;
  return t; }

static Inline size_t tbl_idx(size_t cap, size_t co) {
  return co & ((1 << cap) - 1); }

static Inline size_t tbl_load(tbl t) {
  return t->len >> t->cap; }

static Inline ob tbl_bkt_hc(la v, tbl t, size_t hc) {
  return t->tab[tbl_idx(t->cap, hc)]; }

static ob tbl_ent_hc(la v, tbl t, ob k, size_t hc) {
  ob e = tbl_bkt_hc(v, t, hc);
  while (!nilp(e) && !eql(v, KEY(e), k)) e = NEXT(e);
  return e; }

static ob tbl_ent(la v, tbl t, ob k) {
  return tbl_ent_hc(v, t, k, hash(v, k)); }

static tbl
  tbl_grow(la, tbl) NoInline;
static ob
  tbl_del(la, tbl, ob),
  tblss(la, intptr_t, intptr_t),
  tks(la) NoInline,
  tbl_set_s(la, tbl, ob, ob) NoInline;

static void
  tbl_shrink(la, tbl) NoInline;

tbl table(la v) {
  tbl t = cells(v, Width(tbl) + 1 + Width(tag));
  if (t) ini_tbl(t, 0, 0, (ob*) ini_mo(t+1, 1)),
         t->tab[0] = nil;
  return t; }

ob tbl_set(la v, tbl t, ob k, ob x) {
  with(t, x = tbl_set_s(v, t, k, x));
  if (x && tbl_load(t) > 1)
    with(x, t = tbl_grow(v, t)),
    x = t ? x : 0;
  return x; }

ob tbl_get(la v, tbl t, ob k) { return
  k = tbl_ent(v, t, k),
  nilp(k) ? 0 : VAL(k); }

Vm(tget_u) {
  ArityCheck(2);
  xp = fp->argv[0];
  Check(tblp(xp));
  xp = tbl_get(v, (tbl) xp, fp->argv[1]);
  return ApC(ret, xp ? xp : nil); }

Vm(tdel_u) {
  ArityCheck(2);
  ob x = fp->argv[0];
  Check(tblp(x));
  CallOut(x = tbl_del(v, (tbl) x, fp->argv[1]));
  return ApC(ret, x); }

Vm(tget) { return
  xp = tbl_get(v, (tbl) xp, *sp++),
  ApN(1, xp ? xp : nil); }

Vm(thas) { return
  xp = tbl_get(v, (tbl) xp, *sp++),
  ApN(1, xp ? T : nil); }

Vm(tlen) { return ApN(1, putnum(((tbl) xp)->len)); }

Vm(tkeys) {
  ob x;
  CallOut(x = tks(v));
  return x ? ApN(1, x) : ApC(xoom, nil); }

Vm(thas_u) {
  ArityCheck(2);
  xp = fp->argv[0];
  Check(tblp(xp));
  xp = tbl_get(v, (tbl) xp, fp->argv[1]);
  return ApC(ret, xp ? T : nil); }

Vm(tset_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(tblp(xp));
  ob x;
  CallOut(x = tblss(v, 1, fp->argc));
  return x ? ApC(ret, x) : ApC(xoom, nil); }

Vm(tbl_u) {
  ob x = fp->argc;
  CallOut(x = (v->xp = (ob) table(v)) && tblss(v, 0, x));
  return x ? ApC(ret, xp) : ApC(xoom, nil); }

Vm(tkeys_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(tblp(xp));
  ob x;
  CallOut(x = tks(v));
  return x ? ApC(ret, x) : ApC(xoom, xp); }

Vm(tlen_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(tblp(xp));
  return ApC(ret, putnum(((tbl) xp)->len)); }

Vm(tset) {
  ob x = *sp++, y = *sp++;
  CallOut(x = tbl_set(v, (tbl) xp, x, y));
  return x ? ApN(1, x) : ApC(xoom, xp); }

// FIXME so bad :(
static ob tbl_del(la v, tbl y, ob key) {
  size_t b = tbl_idx(y->cap, hash(v, key));
  ob val = nil,
     e = y->tab[b],
     prev[] = {0,0,e};
  for (ob l = (ob) &prev; l != nil && NEXT(l) != nil; l = NEXT(l))
    if (eql(v, KEY(NEXT(l)), key)) {
      val = VAL(NEXT(l));
      NEXT(l) = NEXT(NEXT(l));
      y->len--;
      break; }
  y->tab[b] = NEXT(prev);
  tbl_shrink(v, y);
  return val; }

// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static NoInline tbl tbl_grow(la v, tbl t) {
  ob *tab0, *tab1;
  size_t cap0 = t->cap,
         cap1 = cap0 + 1,
         len = 1ul << cap1;

  with(t, tab1 = (ob*) mkmo(v, len));
  if (!tab1) return 0;
  setw(tab1, nil, len);
  tab0 = t->tab;

  for (size_t i, cap = 1 << cap0; cap--;)
    for (ob e, es = tab0[cap]; !nilp(es);
      e = es,
      es = NEXT(es),
      i = tbl_idx(cap1, hash(v, KEY(e))),
      NEXT(e) = tab1[i],
      tab1[i] = e);

  t->cap = cap1;
  t->tab = tab1;
  return t; }

static ob tbl_set_s(la v, tbl t, ob k, ob x) {
  size_t hc = hash(v, k);
  ob e = tbl_ent_hc(v, t, k, hc);
  if (!nilp(e)) return VAL(e) = x;
  size_t i = tbl_idx(t->cap, hc);
  with(t, e = Tupl(k, x, t->tab[i]));
  if (!e) return e;
  t->tab[i] = e;
  t->len++;
  return VAL(e); }

// get table keys
// XXX calling convention: table in v->xp
static NoInline ob tks(la v) {
  size_t len = ((tbl) v->xp)->len;
  two ks;
  ks = cells(v, Width(two) * len);
  if (!ks) return 0;
  ob r = nil, *tab = ((tbl) v->xp)->tab;
  while (len) for (ob e = *tab++; !nilp(e);
    ini_two(ks, KEY(e), r),
    r = (ob) ks++,
    e = NEXT(e),
    len--);
  return r; }

// do a bunch of table assignments.
// XXX calling convention: table in v->xp
// FIXME gross!
static ob tblss(la v, intptr_t i, intptr_t l) {
  ob r = nil;
  while (r && i <= l - 2)
    r = tbl_set(v,
      (tbl) v->xp,
      v->fp->argv[i],
      v->fp->argv[i+1]),
    i += 2;
  return r; }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static NoInline void tbl_shrink(la v, tbl t) {
  // do nothing if we're already overloaded
  if (tbl_load(t)) return;

  ob e = nil, f, g;
  size_t i = 1ul << t->cap;

  // collect all entries
  while (i--) for (f = t->tab[i], t->tab[i] = nil; !nilp(f);
    g = NEXT(f), NEXT(f) = e, e = f, f = g);

  // shrink bucket array
  while (t->cap && tbl_load(t) < 1) t->cap--;
  i = 1ul << t->cap;
  ini_mo(t->tab, i);

  // reinsert
  while (e != nil)
    i = tbl_idx(t->cap, hash(v, KEY(e))),
    f = NEXT(e),
    NEXT(e) = t->tab[i],
    t->tab[i] = e,
    e = f; }

static Vm(ap_tbl) {
  ob a = fp->argc;
  switch (a) {
    case 0: return ApC(ret, putnum(((tbl) ip)->len));
    case 1: return
      xp = tbl_get(v, (tbl) ip, fp->argv[0]),
      ApC(ret, xp ? xp : nil);
    default: return
      xp = (ob) ip,
      CallOut(a = tblss(v, 1, a)),
      a ? ApC(ret, a) : ApC(xoom, nil); } }

static Gc(cp_tbl) {
  tbl src = (tbl) x, dst = bump(v, Width(tbl));
  src->disp = (vm*) dst;
  return (ob) ini_tbl(dst, src->len, src->cap,
    (ob*) cp(v, (ob) src->tab, pool0, top0)); }

static long tx_tbl(la v, FILE *o, ob _) {
  tbl t = (tbl) _;
  return fprintf(o, "#tbl:%ld/%ld", t->len, 1ul << t->cap); }

static intptr_t hx_tbl(la v, ob _) {
  return ror(mix * 9, 3 * sizeof(intptr_t) / 4); }

const struct mtbl mtbl_tbl = {
  .does = ap_tbl,
  .emit = tx_tbl,
  .evac = cp_tbl,
  .hash = hx_tbl,
  .equi = eq_not, };
