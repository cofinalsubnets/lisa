#include "i.h"
struct tbl { // hash tables
  vm *act;
  const struct typ *typ;
  uintptr_t len, cap;
  struct tbl_e {
    ob key, val;
    struct tbl_e *next; } **tab; };

static Inline size_t tbl_load(tbl t) {
  return t->len / t->cap; }

static Inline size_t tbl_idx(size_t cap, size_t co) {
  return (cap - 1) & co; }

static Inline tbl ini_tbl(void *_, size_t len, size_t cap, struct tbl_e **tab) {
  tbl t = _; return
    t->act = act,
    t->typ = &tbl_typ,
    t->len = len,
    t->cap = cap,
    t->tab = tab,
    t; }

tbl tbl_new(li v) {
  tbl t = cells(v, Width(struct tbl) + 1);
  if (t) ini_tbl(t, 0, 1, (struct tbl_e**) (t + 1))->tab[0] = 0;
  return t; }

static void tx_tbl(la, FILE*, ob);
static ob cp_tbl(la, ob, ob*, ob*);

// hash tables are hashed by their type
static uintptr_t hx_typ(la v, ob _) {
  return ror(mix * (uintptr_t) GF(_), 16); }

// hash tables
// some of the worst code is here :(

static NoInline struct tbl_e *tbl_ent_hc(la v, tbl t, ob k, uintptr_t hc) {
  struct tbl_e *e = t->tab[tbl_idx(t->cap, hc)];
  while (e && !eql(v, e->key, k)) e = e->next;
  return e; }

static struct tbl_e *tbl_ent(la v, tbl t, ob k) {
  return tbl_ent_hc(v, t, k, hash(v, k)); }

NoInline ob tbl_get(la v, tbl t, ob k, ob d) {
  struct tbl_e *e = tbl_ent(v, t, k);
  return e ? e->val : d; }

static tbl tbl_grow(la, tbl);

tbl tbl_set(la v, tbl t, ob k, ob x) {
  uintptr_t hc = hash(v, k);
  struct tbl_e *e = tbl_ent_hc(v, t, k, hc);
  if (e) return e->val = x, t;

  uintptr_t i = tbl_idx(t->cap, hc);
  with(t, with(k, with(x, e = cells(v, Width(struct tbl_e)))));
  if (!e) return 0;

  e->key = k, e->val = x, e->next = t->tab[i];
  t->tab[i] = e;
  t->len++;
  return tbl_grow(v, t); }

// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static NoInline tbl tbl_grow(la v, tbl t) {
  size_t cap0 = t->cap,
         cap1 = cap0,
         load = tbl_load(t);
  while (load > 1) cap1 <<= 1, load >>= 1;
  if (cap0 == cap1) return t;

  struct tbl_e **tab0, **tab1;
  with(t, tab1 = (struct tbl_e**) cells(v, cap1));
  if (!tab1) return 0;
  setw(tab1, 0, cap1);
  tab0 = t->tab;

  for (size_t i; cap0--;)
    for (struct tbl_e *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = tbl_idx(cap1, hash(v, e->key)),
      e->next = tab1[i],
      tab1[i] = e);

  return t->cap = cap1,
         t->tab = tab1,
         t; }

static ob tbl_del_s(la, tbl, ob, ob), tbl_keys(la);
// get table keys
// XXX calling convention: table in v->xp
static ob tbl_keys(la v) {
  size_t len = ((tbl) v->xp)->len;
  two ks = cells(v, Width(struct two) * len);
  if (!ks) return 0;
  ob r = nil;
  struct tbl_e **tab = ((tbl) v->xp)->tab;
  while (len) for (struct tbl_e *e = *tab++; e;
    two_ini(ks, e->key, r),
    r = (ob) ks++,
    e = e->next,
    len--);
  return r; }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static void tbl_shrink(la v, tbl t) {
  struct tbl_e *e = NULL, *f, *g;
  size_t i = t->cap;

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

// do a bunch of table assignments.
// XXX calling convention: table in v->xp
static NoInline bool tblss(la v, I i, I l) {
  return i > l - 2 ||
    (tbl_set(v, (tbl) v->xp, v->fp->argv[i], v->fp->argv[i + 1]) &&
     tblss(v, i + 2, l)); }

static NoInline ob tbl_del_s(la v, tbl y, ob key, ob val) {
  size_t b = tbl_idx(y->cap, hash(v, key));
  struct tbl_e *e = y->tab[b], prev = {0,0,e};

  for (struct tbl_e *l = &prev; l && l->next; l = l->next)
    if (eql(v, l->next->key, key)) {
      val = l->next->val;
      l->next = l->next->next;
      y->len--;
      break; }

  return y->tab[b] = prev.next,
         val; }

Vm(tget_f) { return
  fp->argc < 2 ? Yield(ArityError, putnum(2)) :
  !tblp(fp->argv[0]) ? Yield(DomainError, xp) :
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
  xp = tbl_get(v, (tbl) xp, *sp++, 0) ? T : nil,
  ApN(1, xp); }

Vm(tlen) { return ApN(1, putnum(((tbl) xp)->len)); }

Vm(thas_f) { return
  fp->argc < 2 ? Yield(ArityError, putnum(2)) :
  !tblp(fp->argv[0]) ? Yield(DomainError, xp) :
  (xp = tbl_get(v, (tbl) fp->argv[0], fp->argv[1], 0),
   ApC(ret, xp ? T : nil)); }

Vm(tset_f) {
  bool _; return
    !fp->argc ? ApC(ret, xp) :
    !tblp(xp = fp->argv[0]) ? Yield(DomainError, xp) :
    (CallOut(_ = tblss(v, 1, fp->argc)),
     _ ? ApC(ret, fp->argv[fp->argc-1]) :
         Yield(OomError, nil)); }

Vm(tbl_f) {
  ob x = fp->argc; return
    CallOut(x = (v->xp = (ob) tbl_new(v)) && tblss(v, 0, x)),
    x ? ApC(ret, xp) : Yield(OomError, nil); }

Vm(tkeys_f) { ob x; return
  !fp->argc ? Yield(ArityError, putnum(1)) :
  !tblp(xp = fp->argv[0]) ? Yield(DomainError, xp) :
  (CallOut(x = tbl_keys(v)), !x) ?
    Yield(OomError, xp) : ApC(ret, x); }

Vm(tlen_f) { return
  !fp->argc ? Yield(ArityError, putnum(1)) :
  !tblp(xp = fp->argv[0]) ? Yield(DomainError, xp) :
  ApC(ret, putnum(((tbl) xp)->len)); }

Vm(tset) {
  ob x = *sp++;
  CallOut(x = (ob) tbl_set(v, (tbl) xp, x, *sp));
  return x ? ApN(1, *sp++) : Yield(OomError, xp); }

static Vm(do_tbl) {
  bool _;
  ob a = fp->argc;
  switch (a) {
    case 0: return ApC(ret, putnum(((tbl) ip)->len));
    case 1: return
      xp = tbl_get(v, (tbl) ip, fp->argv[0], nil),
      ApC(ret, xp);
    default: return
      xp = (ob) ip,
      CallOut(_ = tblss(v, 1, a)),
      _ ? ApC(ret, fp->argv[a-1]) : Yield(OomError, nil); } }

static void tx_tbl(la v, FILE* o, ob _) {
  fprintf(o, "#tbl:%ld/%ld", ((tbl)_)->len, ((tbl)_)->cap); }

static Gc(cp_tbl) {
  tbl src = (tbl) x;
  size_t i = src->cap;
  tbl dst = bump(v, Width(struct tbl) + i);
  src->act = (vm*) dst;
  ini_tbl(dst, src->len, i, (struct tbl_e**) (dst+1));
  for (struct tbl_e *s, *e, *d; i--; dst->tab[i] = e)
    for (s = src->tab[i], e = NULL; s;
      d = bump(v, Width(struct tbl_e)),
      d->key = s->key, d->val = s->val,
      d->next = e, e = d,
      s = s->next);
  return (ob) dst; }

static void wk_tbl(li v, ob x, ob *pool0, ob *top0) {
  tbl t = (tbl) x;
  v->cp += Width(struct tbl) + t->cap +
           t->len * Width(struct tbl_e);
  for (size_t i = 0, lim = t->cap; i < lim; i++)
    for (struct tbl_e *e = t->tab[i]; e; e = e->next)
      e->key = cp(v, e->key, pool0, top0),
      e->val = cp(v, e->val, pool0, top0); }

const struct typ tbl_typ = {
  .does = do_tbl, .emit = tx_tbl, .evac = cp_tbl,
  .hash = hx_typ, .equi = neql, .walk = wk_tbl, };
