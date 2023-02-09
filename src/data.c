#include "i.h"

// function functions
//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|?|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.

// try to get the name of a function
ob hnom(li v, mo x) {
  if (!livep(v, (ob) x)) return nil;
  vm *k = G(x);

  if (k == setclo || k == genclo0 || k == genclo1) // closure?
    return hnom(v, (mo) G(FF(x)));

  ob n = ((ob*) mo_tag(x))[-1];
  return homp(n) && livep(v, n) && G(n) == act ? n : nil; }
//symbols

// FIXME this should probably change at some point.
// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle which seems to keep it
// from getting too bad. this is much more performant than a
// list & uses less memory than a hash table, but maybe we
// should use a table anyway.
//
static Inline sym ini_sym(void *_, str nom, U code) {
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
  return *y = ini_sym(bump(v, Width(struct sym)), b,
    hash(v, putnum(hash(v, (ob) b)))); }

sym symof(la v, str s) {
  if (Avail < Width(struct sym)) {
    bool _; with(s, _ = please(v, Width(struct sym)));
    if (!_) return 0; }
  return intern(v, &v->syms, s); }

static Gc(cp_sym) {
  sym src = (sym) x,
      dst = src->nom ?
        intern(v, &v->syms, (str) cp(v, (ob) src->nom, pool0, top0)) :
        ini_anon(bump(v, Width(struct sym) - 2), src->code);
  return (ob) (src->act = (vm*) dst); }

static void wk_sym(li v, ob x, ob *pool0, ob *top0) {
  v->cp += Width(struct sym) - (((sym) x)->nom ? 0 : 2); }

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

static void tx_sym(la v, FILE* o, ob _) {
  str s = ((sym) _)->nom;
  if (!s) fputs("#sym", o);
  else {
    size_t n = s->len;
    const char *c = s->text;
    while (n--) putc(*c++, o); } }

static uintptr_t hx_sym(la v, ob _) {
  return ((sym) _)->code; }

const struct typ sym_typ = {
  .does = do_id, .emit = tx_sym, .evac = cp_sym,
  .hash = hx_sym, .walk = wk_sym, .equi = neql, };
str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  if (!o) return 0;
  memcpy(o->text, c, bs);
  return str_ini(o, bs); }

static uintptr_t hx_str(la v, ob _) {
  str s = (str) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(ob),
         bytes = s->len % sizeof(ob);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const I *ws = (I*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static Inline bool escapep(char c) {
  return c == '\\' || c == '"'; }

static void tx_str(struct V *v, FILE *o, ob _) {
  str s = (str) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if (escapep(c = *text++)) putc('\\', o);
  putc('"', o); }

static Gc(cp_str) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  memcpy(dst, src, sizeof(struct str) + src->len);
  src->act = (vm*) dst;
  return (ob) dst; }

static void wk_str(li v, ob x, ob *pool0, ob *top0) {
  v->cp += Width(struct str) + b2w(((str) x)->len); }

static bool eq_str(struct V *v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len && !strncmp(a->text, b->text, a->len); }

const struct typ str_typ = {
  .does = do_id,
  .emit = tx_str,
  .evac = cp_str,
  .hash = hx_str,
  .walk = wk_str,
  .equi = eq_str, };
// pairs and lists
static size_t llenr(ob l, size_t n) {
  return twop(l) ? llenr(B(l), n + 1) : n; }
size_t llen(ob l) { return llenr(l, 0); }

static Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, Width(struct two));
  src->act = (vm*) dst;
  return (ob) two_ini(dst, src->a, src->b); }

static void wk_two(li v, ob x, ob *pool0, ob *top0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), pool0, top0);
  B(x) = cp(v, B(x), pool0, top0); }

static void tx_two(la v, FILE* o, ob x) {
  putc('(', o);
  for (;;) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) break;
    putc(' ', o); }
  putc(')', o); }

static uintptr_t hx_two(la v, ob x) {
  uintptr_t hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(I)); }

static bool eq_two(la v, ob x, ob y) {
  return gettyp(y) == &two_typ &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

static Vm(do_two) { return
  ApC(ret, fp->argc ? B(ip) : A(ip)); }

const struct typ two_typ = {
  .does = do_two,
  .emit = tx_two,
  .evac = cp_two,
  .hash = hx_two,
  .walk = wk_two,
  .equi = eq_two, };

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

const uintptr_t mix = 2708237354241864315;
uintptr_t hash(la v, ob x) {
  if (nump(x)) return ror(mix * x, sizeof(I) * 2);
  if (G(x) == act) return gettyp(x)->hash(v, x);
  if (!livep(v, x)) return mix ^ (x * mix);
  return mix ^ hash(v, hnom(v, (mo) x)); }
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
    for (struct tbl_e *e = t->tab[i]; e;
      e->key = cp(v, e->key, pool0, top0),
      e->val = cp(v, e->val, pool0, top0),
      e = e->next); }

const struct typ tbl_typ = {
  .does = do_tbl, .emit = tx_tbl, .evac = cp_tbl,
  .hash = hx_typ, .equi = neql, .walk = wk_tbl, };
