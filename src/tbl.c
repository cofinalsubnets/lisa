#include "lisa.h"
#include "vm.h"

// hash tables
// some of the worst code is here :(

struct mtbl s_mtbl_tbl = { do_tbl, em_tbl, cp_tbl, hash_tbl };

static Inline size_t tbl_idx(size_t cap, size_t co) {
  return co & ((1 << cap) - 1); }

static Inline size_t tbl_load(ob t) {
  return gettbl(t)->len >> gettbl(t)->cap; }

static ob
  tbl_ent(la, ob, ob),
  tbl_grow(la, ob),
  tbl_del(la, ob, ob),
  tblss(la, intptr_t, intptr_t),
  tks_i(la, ob, size_t),
  tks_j(la, ob, ob),
  tbl_set_s(la, ob, ob, ob);

ob table(la v) {
  tbl t = cells(v, Width(tbl) + 3);
  if (!t) return 0;
  ob *b = (ob*) (t + 1);
  t->len = t->cap = 0;
  t->tab = b;
  t->disp = disp;
  t->mtbl = mtbl_tbl;
  b[0] = nil;
  b[1] = 0;
  b[2] = (ob) b;
  return puttbl(t); }

ob tbl_set(la v, ob t, ob k, ob x) {
  with(t, x = tbl_set_s(v, t, k, x));
  if (!x) return 0;
  if (tbl_load(t) > 1) {
    with(x, t = tbl_grow(v, t));
    if (!t) return 0; }
  return x; }

ob tbl_get(la v, ob t, ob k) {
  ob e = tbl_ent(v, t, k);
  return e == nil ? 0 : R(e)[1]; }

Vm(tget_u) {
  ArityCheck(2);
  xp = Argv[0];
  Check(tblp(xp));
  xp = tbl_get(v, xp, Argv[1]);
  return ApC(ret, xp ? xp : nil); }

Vm(tdel_u) {
  ArityCheck(2);
  xp = Argv[0];
  Check(tblp(xp));
  Pack();
  v->xp = tbl_del(v, xp, Argv[1]);
  Unpack();
  return ApC(ret, xp); }

Vm(tget) {
  xp = tbl_get(v, xp, *sp++);
  return ApN(1, xp ? xp : nil); }

Vm(thas) {
  xp = tbl_get(v, xp, *sp++);
  return ApN(1, xp ? T : nil); }

Vm(tlen) { return ApN(1, putnum(gettbl(xp)->len)); }

Vm(tkeys) {
  Pack();
  v->xp = tks_i(v, xp, 0);
  Unpack();
  return xp ? ApN(1, xp) : ApC(oom_err, nil); }

Vm(thas_u) {
  ArityCheck(2);
  xp = Argv[0];
  Check(tblp(xp));
  xp = tbl_get(v, xp, Argv[1]);
  return ApC(ret, xp ? T : nil); }

Vm(tset_u) {
  ArityCheck(1);
  xp = Argv[0];
  Check(tblp(xp));
  Pack();
  v->xp = tblss(v, 1, getnum(Argc));
  Unpack();
  return ApC(xp ? ret : oom_err, xp); }

Vm(tbl_u) {
  Pack();
  bool _ = (v->xp = table(v)) && tblss(v, 0, getnum(Argc));
  Unpack();
  return ApC(_ ? ret : oom_err, xp); }

Vm(tkeys_u) {
  ArityCheck(1);
  xp = Argv[0];
  Check(tblp(xp));
  Pack();
  v->xp = tks_i(v, xp, 0);
  Unpack();
  return ApC(xp ? ret : oom_err, xp); }

Vm(tlen_u) {
  ArityCheck(1);
  xp = Argv[0];
  Check(tblp(xp));
  return ApC(ret, putnum(gettbl(xp)->len)); }

Vm(tset) {
  ob x = *sp++, y = *sp++;
  Pack();
  v->xp = tbl_set(v, xp, x, y);
  Unpack();
  return xp ? ApN(1, xp) : ApC(oom_err, xp); }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static void tbl_fit(la v, ob t) {
  if (tbl_load(t)) return;

  ob e = nil, f, g;
  tbl u = gettbl(t);

  // collect all entries
  for (size_t i = 1 << u->cap; i--;)
    for (f = u->tab[i], u->tab[i] = nil; f != nil;
      g = R(f)[2], R(f)[2] = e,
      e = f, f = g);

  // shrink bucket array
  while (u->cap && tbl_load(t) < 1) u->cap--;

  // reinsert
  while (e != nil) {
    size_t i = tbl_idx(u->cap, hash(v, R(e)[0]));
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
  y->tab[b] = prev[2];
  tbl_fit(v, t);
  return val; }

// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static ob tbl_grow(la v, ob t) {
  ob *tab0, *tab1;
  size_t cap0 = gettbl(t)->cap, cap1 = cap0 + 1,
         len = 1<<cap1;
  with(t, tab1 = cells(v, len + 2));
  if (!tab1) return 0;
  tab1[len] = 0, tab1[len+1] = (ob) tab1;
  setw(tab1, nil, 1<<cap1);
  tab0 = gettbl(t)->tab;

  for (size_t i, cap = 1 << cap0; cap--;)
    for (ob e, es = tab0[cap]; es != nil;)
      e = es,
      es = R(es)[2],
      i = tbl_idx(cap1, hash(v, R(e)[0])),
      R(e)[2] = tab1[i],
      tab1[i] = e;

  gettbl(t)->cap = cap1;
  gettbl(t)->tab = tab1;
  return t; }

static ob tbl_set_s(la v, ob t, ob k, ob x) {
  tbl y;
  ob e = tbl_ent(v, t, k);
  size_t i = tbl_idx(gettbl(t)->cap, hash(v, k));
  if (!nilp(e)) return ptr(e)[1] = x;
  with(t, with(k, with(x, e = (ob) mkmo(v, 3))));
  if (!e) return 0;
  y = gettbl(t),
  ptr(e)[0] = k,
  ptr(e)[1] = x,
  ptr(e)[2] = (ob) y->tab[i],
  y->tab[i] = (ob) e,
  y->len += 1;
  return x; }

static ob tks_j(la v, ob e, ob l) {
  if (nilp(e)) return l;
  ob x = R(e)[0];
  with(x, l = tks_j(v, R(e)[2], l));
  return l ? pair(v, x, l) : 0; }

static ob tks_i(la v, ob t, size_t i) {
  if (i == 1 << gettbl(t)->cap) return nil;
  ob k;
  with(t, k = tks_i(v, t, i+1));
  return k ? tks_j(v, gettbl(t)->tab[i], k) : 0; }

static ob tblss(la v, intptr_t i, intptr_t l) {
  fr fp = v->fp;
  return
    i > l - 2 ? Argv[i - 1] :
    !tbl_set(v, v->xp, Argv[i], Argv[i + 1]) ? 0 :
    tblss(v, i + 2, l); }

static ob tbl_ent_(la v, ob e, ob k) {
  return nilp(e) || eql(R(e)[0], k) ?
    e :
    tbl_ent_(v, R(e)[2], k); }

static ob tbl_ent(la v, ob u, ob k) {
  tbl t = gettbl(u);
  u = t->tab[tbl_idx(t->cap, hash(v, k))];
  return tbl_ent_(v, u, k); }

Vm(do_tbl) {
  tbl t = (tbl) ip;
  size_t a = getnum(Argc);
  switch (a) {
    case 0: return ApC(ret, putnum(t->len));
    case 1:
      xp = tbl_get(v, (ob) ip, Argv[0]);
      return ApC(ret, xp ? xp : nil);
    default:
      Pack();
      v->xp = tblss(v, 1, a);
      Unpack();
      return ApC(xp ? ret : oom_err, xp); } }

Gc(cp_tbl) {
  tbl src = gettbl(x);
  size_t src_cap = src->cap;
  tbl dst = bump(v, Width(tbl) + (1l<<src_cap));
  dst->disp = disp;
  dst->mtbl = mtbl_tbl;
  dst->len = src->len;
  dst->cap = src_cap;
  dst->tab = (ob*) (dst + 1);
  ob *src_tab = src->tab;
  G(src) = (vm*) puttbl(dst);
  dst->tab = (ob*) cp(v, (ob) src_tab, len0, pool0);
  return puttbl(dst); }

void em_tbl(la v, FILE *o, ob x) {
  tbl t = gettbl(x);
  fprintf(o, "#tbl:%ld/%ld", t->len, 1l<<t->cap); }
