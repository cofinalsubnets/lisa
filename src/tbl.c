#include "lisa.h"
#include "vm.h"

// hash tables
// some of the worst code is here :(

struct mtbl s_mtbl_tbl = { do_tbl, em_tbl, cp_tbl, hash_tbl };

static Inline size_t tbl_idx(size_t cap, size_t co) {
  return co & ((1 << cap) - 1); }

static Inline size_t tbl_load(ob t) {
  return ((tbl) t)->len >> ((tbl) t)->cap; }

static ob
  tbl_ent(la, ob, ob),
  tbl_grow(la, ob),
  tbl_del(la, ob, ob),
  tblss(la, intptr_t, const intptr_t),
  tks(la, ob),
  tbl_set_s(la, ob, ob, ob);

ob table(la v) {
  tbl t = cells(v, Width(tbl) + 3);
  if (!t) return 0;
  ob *b = (ob*) (t + 1);
  t->disp = disp;
  t->mtbl = mtbl_tbl;
  t->len = t->cap = 0;
  t->tab = b;
  b[0] = nil;
  b[1] = 0;
  b[2] = (ob) b;
  return (ob) t; }

ob tbl_set(la v, ob t, ob k, ob x) {
  with(t, x = tbl_set_s(v, t, k, x));
  if (!x) return 0;
  if (tbl_load(t) > 1) {
    with(x, t = tbl_grow(v, t));
    if (!t) return 0; }
  return x; }

ob tbl_get(la v, ob t, ob k) {
  ob e = tbl_ent(v, t, k);
  return e == nil ? 0 : ((ob*) e)[1]; }

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
  xp ? ApN(1, xp) : ApC(oom_err, nil); }

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
  CallOut(v->xp = tblss(v, 1, getnum(fp->argc)));
  return ApC(xp ? ret : oom_err, xp); }

Vm(tbl_u) {
  bool _;
  CallOut(xp = getnum(fp->argc),
          _ = (v->xp = table(v)) && tblss(v, 0, xp));
  return ApC(_ ? ret : oom_err, xp); }

Vm(tkeys_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(tblp(xp));
  CallOut(v->xp = tks(v, xp));
  return ApC(xp ? ret : oom_err, xp); }

Vm(tlen_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(tblp(xp));
  return ApC(ret, putnum(((tbl) xp)->len)); }

Vm(tset) {
  ob x = *sp++, y = *sp++;
  CallOut(v->xp = tbl_set(v, xp, x, y));
  return xp ? ApN(1, xp) : ApC(oom_err, xp); }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static void tbl_fit(la v, ob t) {
  if (tbl_load(t)) return;

  ob e = nil, f, g;
  tbl u = (tbl) t;
  size_t i = 1ul << u->cap;

  // collect all entries
  while (i--) for (f = u->tab[i], u->tab[i] = nil; !nilp(f);
    g = ((ob*) f)[2],
    ((ob*) f)[2] = e,
    e = f,
    f = g);

  // shrink bucket array
  while (u->cap && tbl_load(t) < 1) u->cap--;
  i = 1ul << u->cap;
  u->tab[i] = 0;
  u->tab[i+1] = (ob) u->tab;

  // reinsert
  while (e != nil)
    i = tbl_idx(u->cap, hash(v, ((ob*) e)[0])),
    f = ((ob*) e)[2],
    ((ob*) e)[2] = u->tab[i],
    u->tab[i] = e,
    e = f; }

static ob tbl_del(la v, ob t, ob key) {
  tbl y = (tbl) t;
  size_t b = tbl_idx(y->cap, hash(v, key));
  ob val = nil,
     e = y->tab[b],
     prev[] = {0,0,e};
  for (ob l = (ob) &prev; l != nil && ((ob*) l)[2] != nil; l = ((ob*) l)[2])
    if (((ob*) ((ob*) l)[2])[0] == key) {
      val = ((ob*) ((ob*) l)[2])[1];
      ((ob*) l)[2] = ((ob*) ((ob*) l)[2])[2];
      y->len--;
      break; }
  y->tab[b] = prev[2];
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
      es = ((ob*) es)[2],
      i = tbl_idx(cap1, hash(v, ((ob*) e)[0])),
      ((ob*) e)[2] = tab1[i],
      tab1[i] = e);

  ((tbl) t)->cap = cap1;
  ((tbl) t)->tab = tab1;
  return t; }

static ob tbl_set_s(la v, ob t, ob k, ob x) {
  ob e = tbl_ent(v, t, k);
  size_t i = tbl_idx(((tbl) t)->cap, hash(v, k));
  if (!nilp(e)) return ((ob*) e)[1] = x;
  with(t, with(k, with(x, e = (ob) mkmo(v, 3))));
  if (!e) return 0;
  tbl y = (tbl) t;
  ((ob*) e)[0] = k;
  ((ob*) e)[1] = x;
  ((ob*) e)[2] = y->tab[i];
  y->tab[i] = e;
  y->len += 1;
  return x; }

// get table keys
static ob tks(la v, ob t) {
  size_t len = ((tbl) t)->len;
  two ks;
  with(t, ks = cells(v, Width(two) * len));
  if (!ks) return 0;
  ob r = nil, *tab = ((tbl) t)->tab;
  while (len) for (ob *e = *(ob**)tab++; !nilp((ob) e);
    ks->disp = disp,
    ks->mtbl = mtbl_two,
    ks->a = e[0],
    ks->b = r,
    r = (ob) ks++,
    e = (ob*) e[2],
    len--);
  return r; }

// do a bunch of table assignments.
// XXX calling convention: table in v->xp
static ob tblss(la v, intptr_t i, const intptr_t l) {
  for (;i <= l - 2; i += 2)
    if (!tbl_set(v, v->xp, v->fp->argv[i], v->fp->argv[i+1]))
      return 0;
  return v->fp->argv[i - 1]; }

static ob tbl_ent(la v, ob e, ob k) {
  e = ((tbl) e)->tab[tbl_idx(((tbl) e)->cap, hash(v, k))];
  while (!nilp(e) && !eql(((ob*) e)[0], k)) e = ((ob*) e)[2];
  return e; }

Vm(do_tbl) {
  size_t a = getnum(fp->argc);
  switch (a) {
    case 0: return ApC(ret, putnum(((tbl) ip)->len));
    case 1: return
      xp = tbl_get(v, (ob) ip, fp->argv[0]),
      ApC(ret, xp ? xp : nil);
    default: return
      xp = (ob) ip,
      CallOut(v->xp = tblss(v, 1, a)),
      ApC(xp ? ret : oom_err, xp); } }

Gc(cp_tbl) {
  tbl src = (tbl) x, dst = bump(v, Width(tbl));
  src->disp = (vm*) dst;
  dst->disp = disp;
  dst->mtbl = mtbl_tbl;
  dst->len = src->len;
  dst->cap = src->cap;
  dst->tab = (ob*) cp(v, (ob) src->tab, len0, pool0);
  return (ob) dst; }

void em_tbl(la v, FILE *o, ob x) {
  fprintf(o, "#tbl:%ld/%ld", ((tbl) x)->len, 1ul<<((tbl) x)->cap); }

bool tblp(ob _) { return homp(_) && GF(_) == (vm*) mtbl_tbl; }
