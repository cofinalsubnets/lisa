#include "la.h"

// hash tables
static Inline tbl ini_tbl(void *_, size_t len, size_t cap, ob *tab) {
  tbl t = _;
  t->disp = disp, t->mtbl = &mtbl_tbl;
  t->len = len, t->cap = cap, t->tab = tab;
  return t; }
// some of the worst code is here :(

#define KEY(e) ((ob*)(e))[0]
#define VAL(e) ((ob*)(e))[1]
#define NEXT(e) ((ob*)(e))[2]

static Inline size_t tbl_idx(size_t cap, size_t co) {
  return co & ((1 << cap) - 1); }

static Inline size_t tbl_load(tbl t) {
  return t->len >> t->cap; }

static Inline ob tbl_bkt_hc(la v, ob t, size_t hc) {
  return ((tbl) t)->tab[tbl_idx(((tbl) t)->cap, hc)]; }

static ob tbl_ent_hc(la v, ob t, ob k, size_t hc) {
  t = tbl_bkt_hc(v, t, hc);
  while (!nilp(t) && !eql(v, KEY(t), k)) t = NEXT(t);
  return t; }

static ob tbl_ent(la v, ob t, ob k) {
  return tbl_ent_hc(v, t, k, hash(v, k)); }

static ob
  tbl_grow(la, ob),
  tbl_del(la, ob, ob),
  tblss(la, intptr_t, const intptr_t),
  tks(la, ob),
  tbl_set_s(la, ob, ob, ob) NoInline;

ob table(la v) {
  tbl t = cells(v, Width(tbl) + 3);
  if (t) t = ini_tbl(t, 0, 0, (ob*) (t+1)),
         t->tab[0] = nil,
         t->tab[1] = 0,
         t->tab[2] = (ob) t->tab;
  return (ob) t; }

ob tbl_set(la v, ob t, ob k, ob x) {
  with(t, x = tbl_set_s(v, t, k, x));
  if (x && tbl_load((tbl) t) > 1)
    with(x, t = tbl_grow(v, t)),
    x = t ? x : t;
  return x; }

ob tbl_get(la v, ob t, ob k) { return
  t = tbl_ent(v, t, k),
  nilp(t) ? 0 : VAL(t); }

Vm(tget_u) {
  ArityCheck(2);
  xp = fp->argv[0];
  Check(tblp(xp));
  xp = tbl_get(v, xp, fp->argv[1]);
  return ApC(ret, xp ? xp : nil); }

Vm(tdel_u) {
  ArityCheck(2);
  xp = fp->argv[0];
  Check(tblp(xp));
  CallOut(v->xp = tbl_del(v, xp, fp->argv[1]));
  return ApC(ret, xp); }

Vm(tget) { return
  xp = tbl_get(v, xp, *sp++),
  ApN(1, xp ? xp : nil); }

Vm(thas) { return
  xp = tbl_get(v, xp, *sp++),
  ApN(1, xp ? T : nil); }

Vm(tlen) { return ApN(1, putnum(((tbl) xp)->len)); }

Vm(tkeys) { return
  CallOut(v->xp = tks(v, xp)),
  xp ? ApN(1, xp) : ApC(xoom, nil); }

Vm(thas_u) {
  ArityCheck(2);
  xp = fp->argv[0];
  Check(tblp(xp));
  xp = tbl_get(v, xp, fp->argv[1]);
  return ApC(ret, xp ? T : nil); }

Vm(tset_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(tblp(xp));
  CallOut(v->xp = tblss(v, 1, fp->argc));
  return ApC(xp ? ret : xoom, xp); }

Vm(tbl_u) {
  ob _ = fp->argc;
  CallOut(_ = (v->xp = table(v)) && tblss(v, 0, _));
  return ApC(_ ? ret : xoom, xp); }

Vm(tkeys_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(tblp(xp));
  CallOut(v->xp = tks(v, xp));
  return ApC(xp ? ret : xoom, xp); }

Vm(tlen_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(tblp(xp));
  return ApC(ret, putnum(((tbl) xp)->len)); }

Vm(tset) {
  ob x = *sp++, y = *sp++;
  CallOut(v->xp = tbl_set(v, xp, x, y));
  return xp ? ApN(1, xp) : ApC(xoom, xp); }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static void tbl_fit(la v, ob t) {
  if (tbl_load((tbl) t)) return;

  ob e = nil, f, g;
  tbl u = (tbl) t;
  size_t i = 1ul << u->cap;

  // collect all entries
  while (i--) for (f = u->tab[i], u->tab[i] = nil; !nilp(f);
    g = NEXT(f), NEXT(f) = e, e = f, f = g);

  // shrink bucket array
  while (u->cap && tbl_load((tbl) t) < 1) u->cap--;
  i = 1ul << u->cap;
  u->tab[i] = 0;
  u->tab[i+1] = (ob) u->tab;

  // reinsert
  while (e != nil)
    i = tbl_idx(u->cap, hash(v, KEY(e))),
    f = NEXT(e),
    NEXT(e) = u->tab[i],
    u->tab[i] = e,
    e = f; }

// FIXME so bad :(
static ob tbl_del(la v, ob t, ob key) {
  tbl y = (tbl) t;
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
  tbl_fit(v, t);
  return val; }

// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static ob tbl_grow(la v, ob t) {
  ob *tab0, *tab1;
  size_t cap0 = ((tbl) t)->cap,
         cap1 = cap0 + 1,
         len = 1l << cap1;
  with(t, tab1 = cells(v, len + 2));
  if (!tab1) return 0;
  tab1[len] = 0, tab1[len+1] = (ob) tab1;
  setw(tab1, nil, len);
  tab0 = ((tbl) t)->tab;

  for (size_t i, cap = 1 << cap0; cap--;)
    for (ob e, es = tab0[cap]; !nilp(es);
      e = es,
      es = NEXT(es),
      i = tbl_idx(cap1, hash(v, KEY(e))),
      NEXT(e) = tab1[i],
      tab1[i] = e);

  ((tbl) t)->cap = cap1;
  ((tbl) t)->tab = tab1;
  return t; }

static ob tbl_set_s(la v, ob t, ob k, ob x) {
  size_t hc = hash(v, k);
  ob e = tbl_ent_hc(v, t, k, hc);
  if (!nilp(e)) return VAL(e) = x;
  size_t i = tbl_idx(((tbl)t)->cap, hc);
  with(t, e = Tupl(k, x, ((tbl)t)->tab[i]));
  if (!e) return e;
  ((tbl)t)->tab[i] = e;
  ((tbl)t)->len++;
  return VAL(e); }

// get table keys
static ob tks(la v, ob t) {
  size_t len = ((tbl) t)->len;
  two ks;
  with(t, ks = cells(v, Width(two) * len));
  if (!ks) return 0;
  ob r = nil, *tab = ((tbl) t)->tab;
  while (len) for (ob e = *tab++; !nilp(e);
    ini_two(ks, KEY(e), r),
    r = (ob) ks++,
    e = NEXT(e),
    len--);
  return r; }

// do a bunch of table assignments.
// XXX calling convention: table in v->xp
// FIXME gross!
static ob tblss(la v, intptr_t i, const intptr_t l) {
  ob r = nil;
  for (;i <= l - 2; i += 2)
    if (!(r = tbl_set(v, v->xp, v->fp->argv[i], v->fp->argv[i+1])))
      break;
  return r; }

static Vm(do_tbl) {
  size_t a = fp->argc;
  switch (a) {
    case 0: return ApC(ret, putnum(((tbl) ip)->len));
    case 1: return
      xp = tbl_get(v, (ob) ip, fp->argv[0]),
      ApC(ret, xp ? xp : nil);
    default: return
      xp = (ob) ip,
      CallOut(v->xp = tblss(v, 1, a)),
      ApC(xp ? ret : xoom, xp); } }

static Gc(cp_tbl) {
  tbl src = (tbl) x, dst = bump(v, Width(tbl));
  src->disp = (vm*) dst;
  return (ob) ini_tbl(dst, src->len, src->cap,
    (ob*) cp(v, (ob) src->tab, pool0, top0)); }

static long em_tbl(la v, FILE *o, ob _) {
  tbl t = (tbl) _;
  return fprintf(o, "#tbl:%ld/%ld", t->len, 1ul << t->cap); }

static size_t hash_tbl(la v, ob _) {
  return ror(mix * 9, 3 * sizeof(size_t) / 4); }

const struct mtbl mtbl_tbl = {
  .does = do_tbl,
  .emit = em_tbl,
  .evac = cp_tbl,
  .hash = hash_tbl,
  .equi = eq_no, };
