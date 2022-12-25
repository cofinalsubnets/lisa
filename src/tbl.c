#include "la.h"

static vm ap_tbl;
static u0  tx_tbl(la, FILE*, I);
static I hx_tbl(la, I), cp_tbl(la, I, I*, I*);

const struct typ tbl_typ = {
  .does = ap_tbl, .emit = tx_tbl, .evac = cp_tbl,
  .hash = hx_tbl, .equi = neql, };

// FIXME this is a totally ad hoc, unproven hashing method.
//
// its performance on hash tables and anonymous functions
// is very bad (they all go to the same bucket!)
//
// strings, symbols, and numbers do better. for pairs it
// depends on what they contain.
//
// copying GC complicates the use of memory addresses for
// hashing mutable data, which is the obvious way to fix
// the bad cases. we would either need to assign each datum
// a unique identifier when it's created & hash using that,
// or use the address but rehash as part of garbage collection.
//
// TODO replace with something better, verify & benchmark

I hash(la v, ob x) {
  if (nump(x)) return ror(mix * x, sizeof(intptr_t) * 2);
  if (G(x) == data) return ((typ) GF(x))->hash(v, x);
  if (!livep(v, x)) return mix ^ (x * mix);
  return mix ^ hash(v, hnom(v, (mo) x)); }

// hash tables
// some of the worst code is here :(

SI U tbl_load(tbl t) { return t->len / t->cap; }
SI U tbl_idx(U cap, U co) { return co & (cap - 1); }
SI tbl ini_tbl(u0 *_, U len, U cap, struct tbl_e **tab) {
  tbl t = _; return
    t->data = data,
    t->typ = &tbl_typ,
    t->len = len,
    t->cap = cap,
    t->tab = tab,
    t; }

static struct tbl_e *tbl_ent_hc(la v, tbl t, ob k, size_t hc) {
  struct tbl_e *e = t->tab[tbl_idx(t->cap, hc)];
  while (e && !eql(v, e->key, k)) e = e->next;
  return e; }

static struct tbl_e *tbl_ent(la v, tbl t, ob k) {
  return tbl_ent_hc(v, t, k, hash(v, k)); }

static u1 tblss(la, I, I);
static u0 tbl_shrink(la, tbl);
static ob tbl_del_s(la, tbl, ob, ob), tbl_keys(la);
static tbl tbl_grow(la, tbl), tbl_set_s(la, tbl, ob, ob);

tbl mktbl(la v) {
  tbl t = cells(v, wsizeof(struct tbl) + 1);
  if (t) ini_tbl(t, 0, 1, (struct tbl_e**) (t + 1)), t->tab[0] = 0;
  return t; }

tbl tbl_set(la v, tbl t, ob k, ob x) { return
  t = tbl_set_s(v, t, k, x),
  t ? tbl_grow(v, t) : 0; }

ob tbl_get(la v, tbl t, ob k, ob d) {
  struct tbl_e *e = tbl_ent(v, t, k);
  return e ? e->val : d; }

Vm(tget_f) { return
  fp->argc < 2 ? ApC(xary, putnum(2)) :
  !tblp(fp->argv[0]) ? ApC(xdom, xp) :
  ApC(ret, tbl_get(v, (tbl) fp->argv[0], fp->argv[1], nil)); }

Vm(tdel_f) {
  ArityCheck(1);
  Check(tblp(fp->argv[0]));
  tbl t = (tbl) fp->argv[0];
  for (size_t i = 1, l = fp->argc; i < l; i++)
    xp = tbl_del_s(v, t, fp->argv[i], xp);
  if (!tbl_load(t)) tbl_shrink(v, t);
  return ApC(ret, xp); }

Vm(tget) { return
  xp = tbl_get(v, (tbl) xp, *sp++, nil),
  ApN(1, xp); }

Vm(thas) { return
  xp = tbl_get(v, (tbl) xp, *sp++, 0),
  ApN(1, xp ? T : nil); }

Vm(tlen) { return ApN(1, putnum(((tbl) xp)->len)); }

Vm(thas_f) { return
  fp->argc < 2 ? ApC(xary, putnum(2)) :
  !tblp(fp->argv[0]) ? ApC(xdom, xp) :
  (xp = tbl_get(v, (tbl) fp->argv[0], fp->argv[1], 0),
   ApC(ret, xp ? T : nil)); }

Vm(tset_f) { bool _; return
  !fp->argc ? ApC(ret, xp) :
  !tblp(xp = fp->argv[0]) ? ApC(xdom, xp) :
  (CallOut(_ = tblss(v, 1, fp->argc)),
   _ ? ApC(ret, fp->argv[fp->argc-1]) :
       ApC(xoom, nil)); }

Vm(tbl_f) {
  ob x = fp->argc;
  CallOut(x = (v->xp = (ob) mktbl(v)) && tblss(v, 0, x));
  return x ? ApC(ret, xp) : ApC(xoom, nil); }

Vm(tkeys_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(tblp(xp));
  ob x;
  CallOut(x = tbl_keys(v));
  return x ? ApC(ret, x) : ApC(xoom, xp); }

Vm(tlen_f) { return
  !fp->argc ? ApC(xary, putnum(1)) :
  !tblp(xp = fp->argv[0]) ? ApC(xdom, xp) :
  ApC(ret, putnum(((tbl) xp)->len)); }

Vm(tset) {
  ob x = *sp++; return
    CallOut(x = (ob) tbl_set(v, (tbl) xp, x, *sp)),
    x ? ApN(1, *sp++) : ApC(xoom, xp); }

// FIXME so bad :(
static ob tbl_del_s(la v, tbl y, ob key, ob val) {
  U b = tbl_idx(y->cap, hash(v, key));
  struct tbl_e
   *e = y->tab[b],
   prev = {0,0,e};
  for (struct tbl_e *l = &prev; l && l->next; l = l->next)
    if (eql(v, l->next->key, key)) {
      val = l->next->val;
      l->next = l->next->next;
      y->len--;
      break; }
  y->tab[b] = prev.next;
  return val; }

// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static tbl tbl_grow(la v, tbl t) {
  struct tbl_e **tab0, **tab1;
  U cap0 = t->cap,
    cap1 = cap0,
    load = tbl_load(t);
  while (load > 1) cap1 <<= 1, load >>= 1;
  if (cap0 == cap1) return t;

  with(t, tab1 = (struct tbl_e**) cells(v, cap1));
  if (!tab1) return 0;
  setw(tab1, 0, cap1);
  tab0 = t->tab;

  for (U i; cap0--;)
    for (struct tbl_e *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = tbl_idx(cap1, hash(v, e->key)),
      e->next = tab1[i],
      tab1[i] = e);

  t->cap = cap1;
  t->tab = tab1;
  return t; }

static tbl tbl_set_s(la v, tbl t, ob k, ob x) {
  U hc = hash(v, k);
  struct tbl_e *e = tbl_ent_hc(v, t, k, hc);
  if (e) return e->val = x, t;
  U i = tbl_idx(t->cap, hc);
  with(t, with(k, with(x, e = cells(v, wsizeof(struct tbl_e)))));
  if (!e) return 0;
  e->key = k, e->val = x, e->next = t->tab[i];
  t->tab[i] = e;
  t->len++;
  return t; }

// get table keys
// XXX calling convention: table in v->xp
static ob tbl_keys(la v) {
  U len = ((tbl) v->xp)->len;
  two ks;
  ks = cells(v, wsizeof(struct two) * len);
  if (!ks) return 0;
  ob r = nil;
  struct tbl_e **tab = ((tbl) v->xp)->tab;
  while (len) for (struct tbl_e *e = *tab++; e;
    two_ini(ks, e->key, r),
    r = (ob) ks++,
    e = e->next,
    len--);
  return r; }

// do a bunch of table assignments.
// XXX calling convention: table in v->xp
// FIXME gross!
static u1 tblss(la v, I i, I l) {
  u1 _ = true;
  while (_ && i <= l - 2)
    _ = tbl_set(v,
      (tbl) v->xp,
      v->fp->argv[i],
      v->fp->argv[i+1]),
    i += 2;
  return _; }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static u0 tbl_shrink(la v, tbl t) {

  struct tbl_e *e = NULL, *f, *g;
  U i = t->cap;

  // collect all entries
  while (i--) for (f = t->tab[i], t->tab[i] = 0; f;
    g = f->next, f->next = e, e = f, f = g);

  // shrink bucket array
  while (t->cap > 1 && !tbl_load(t)) t->cap >>= 1;

  // reinsert
  while (e)
    i = tbl_idx(t->cap, hash(v, e->key)),
    f = e->next,
    e->next = t->tab[i],
    t->tab[i] = e,
    e = f; }

static Vm(ap_tbl) {
  u1 _; ob a = fp->argc; switch (a) {
    case 0: return ApC(ret, putnum(((tbl) ip)->len));
    case 1: return
      xp = tbl_get(v, (tbl) ip, fp->argv[0], nil),
      ApC(ret, xp);
    default: return
      xp = (ob) ip,
      CallOut(_ = tblss(v, 1, a)),
      _ ? ApC(ret, fp->argv[a-1]) : ApC(xoom, nil); } }

static u0 tx_tbl(la_carrier v, la_io o, ob _) {
  fprintf(o, "#tbl:%ld/%ld",
    ((tbl)_)->len,
    ((tbl)_)->cap); }

static I hx_tbl(la_carrier v, ob _) {
  return ror(mix, 3 * sizeof(I) / 4); }

static struct tbl_e *cp_tbl_e(la v, struct tbl_e *src, ob *pool0, ob *top0) {
  if (!src) return src;
  struct tbl_e *dst = bump(v, wsizeof(struct tbl_e));
  dst->next = cp_tbl_e(v, src->next, pool0, top0);
  dst->val = cp(v, src->val, pool0, top0);
  dst->key = cp(v, src->key, pool0, top0);
  return dst; }

static Gc(cp_tbl) {
  tbl src = (tbl) x;
  U i = src->cap;
  tbl dst = bump(v, wsizeof(struct tbl) + i);
  src->data = (vm*) dst;
  ini_tbl(dst, src->len, i, (struct tbl_e**) (dst+1));
  while (i--) dst->tab[i] = cp_tbl_e(v, src->tab[i], pool0, top0);
  return (ob) dst; }
