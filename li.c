#include "i.h"

// push things onto the stack
NoInline bool pushsr(la v, size_t i, va_list xs) {
  bool _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i+1, xs)),
     _ && (*--v->sp = x, true)); }

bool pushs(la v, ...) {
  bool _; va_list xs; return
  va_start(xs, v),
  _ = pushsr(v, 0, xs),
  va_end(xs),
  _; }

static NoInline mo thdr(la v, U n, va_list xs) {
  vm *x = va_arg(xs, vm*);
  if (!x) return mo_n(v, n);
  mo k; with(x, k = thdr(v, n + 1, xs));
  if (k) k[n].ap = x;
  return k; }

NoInline mo thd(la v, ...) {
  mo k; va_list xs; return
  va_start(xs, v),
  k = thdr(v, 0, xs),
  va_end(xs),
  k; }


// unchecked allocator -- make sure there's enough memory!
void *bump(la v, size_t n) {
  void *x = v->hp;
  return v->hp += n, x; }

void *cells(la v, size_t n) { return
  Avail >= n || please(v, n) ? bump(v, n) : 0; }

#define Gc(n) static ob n(la v, ob x, ob *pool0, ob *top0)


str str_ini(void *_, size_t len) {
  str s = _; return
    s->act = act, s->typ = &str_typ,
    s->len = len,
    s; }

#include "vm.h"

// initialization helpers

// pairs and lists
two two_ini(void *_, ob a, ob b) {
  two w = _; return
    w->act = act,
    w->typ = &two_typ,
    w->a = a, w->b = b,
    w; }

static size_t llenr(ob l, size_t n) {
  return twop(l) ? llenr(B(l), n + 1) : n; }
size_t llen(ob l) { return llenr(l, 0); }

static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok; return
    with(a, with(b, ok = please(v, Width(struct two)))),
    ok ? pair(v, a, b) : 0; }

NoInline two pair(la v, ob a, ob b) {
  return Avail >= Width(struct two) ?
    two_ini(bump(v, Width(struct two)), a, b) :
    pair_gc(v, a, b); }

static bool eq_two(la, I, I);
static void tx_two(la, FILE*, I);
static ob cp_two(la, I, I*, I*), hx_two(la, I);

const struct typ two_typ = {
  .actn = ap_two,
  .emit = tx_two,
  .evac = cp_two,
  .hash = hx_two,
  .equi = eq_two, };

Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, Width(struct two));
  src->act = (vm*) dst;
  return (ob) two_ini(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

static void tx_two(la v, FILE* o, ob x) {
  putc('(', o);
  for (;;) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) break;
    putc(' ', o); }
  putc(')', o); }

static intptr_t hx_two(la v, ob x) {
  intptr_t hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(I)); }

static bool eq_two(la v, ob x, ob y) {
  return (typ) GF(y) == &two_typ &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }


static void tx_tbl(la, FILE*, ob);
static intptr_t hx_tbl(la, ob), cp_tbl(la, ob, ob*, ob*);

const struct typ tbl_typ = {
  .actn = ap_tbl, .emit = tx_tbl, .evac = cp_tbl,
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

// just a big random number!
static const intptr_t mix = 2708237354241864315;

intptr_t hash(la v, ob x) {
  if (nump(x)) return ror(mix * x, sizeof(I) * 2);
  if (G(x) == act) return ((typ) GF(x))->hash(v, x);
  if (!livep(v, x)) return mix ^ (x * mix);
  return mix ^ hash(v, hnom(v, (mo) x)); }

// hash tables
// some of the worst code is here :(

tbl tbl_ini(void *_, U len, U cap, struct tbl_e **tab) {
  tbl t = _; return
    t->act = act,
    t->typ = &tbl_typ,
    t->len = len,
    t->cap = cap,
    t->tab = tab,
    t; }

tbl mktbl(la v) {
  tbl t = cells(v, Width(struct tbl) + 1);
  if (t) tbl_ini(t, 0, 1, (struct tbl_e**) (t + 1)), t->tab[0] = 0;
  return t; }

static struct tbl_e *tbl_ent_hc(la v, tbl t, ob k, U hc) {
  struct tbl_e *e = t->tab[tbl_idx(t->cap, hc)];
  while (e && !eql(v, e->key, k)) e = e->next;
  return e; }

static struct tbl_e *tbl_ent(la v, tbl t, ob k) {
  return tbl_ent_hc(v, t, k, hash(v, k)); }

static tbl tbl_grow(la, tbl), tbl_set_s(la, tbl, ob, ob);

tbl tbl_set(la v, tbl t, ob k, ob x) { return
  t = tbl_set_s(v, t, k, x),
  t ? tbl_grow(v, t) : 0; }

ob tbl_get(la v, tbl t, ob k, ob d) {
  struct tbl_e *e = tbl_ent(v, t, k);
  return e ? e->val : d; }

// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static tbl tbl_grow(la v, tbl t) {
  struct tbl_e **tab0, **tab1;
  size_t cap0 = t->cap,
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
  uintptr_t hc = hash(v, k);
  struct tbl_e *e = tbl_ent_hc(v, t, k, hc);
  if (e) return e->val = x, t;
  U i = tbl_idx(t->cap, hc);
  with(t, with(k, with(x, e = cells(v, Width(struct tbl_e)))));
  if (!e) return 0;
  e->key = k, e->val = x, e->next = t->tab[i];
  t->tab[i] = e;
  t->len++;
  return t; }

static void tx_tbl(la v, FILE* o, ob _) {
  fprintf(o, "#tbl:%ld/%ld", ((tbl)_)->len, ((tbl)_)->cap); }

static I hx_tbl(la v, ob _) {
  return ror(mix, 3 * sizeof(I) / 4); }

static struct tbl_e *cp_tbl_e(la v, struct tbl_e *src, ob *pool0, ob *top0) {
  if (!src) return src;
  struct tbl_e *dst = bump(v, Width(struct tbl_e));
  dst->next = cp_tbl_e(v, src->next, pool0, top0);
  dst->val = cp(v, src->val, pool0, top0);
  dst->key = cp(v, src->key, pool0, top0);
  return dst; }

Gc(cp_tbl) {
  tbl src = (tbl) x;
  U i = src->cap;
  tbl dst = bump(v, Width(struct tbl) + i);
  src->act = (vm*) dst;
  tbl_ini(dst, src->len, i, (struct tbl_e**) (dst+1));
  while (i--)
    dst->tab[i] = cp_tbl_e(v, src->tab[i], pool0, top0);
  return (ob) dst; }


static intptr_t hx_str(la v, ob _) {
  str s = (str) _;
  intptr_t h = 1;
  size_t words = s->len / sizeof(ob),
         bytes = s->len % sizeof(ob);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const I *ws = (I*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static Inline bool escapep(char c) { return c == '\\' || c == '"'; }

static void tx_str(struct V *v, FILE *o, ob _) {
  str s = (str) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if (escapep(c = *text++)) putc('\\', o);
  putc('"', o); }

Gc(cp_str) {
  str src = (str) x;
  return (ob) (src->act = (vm*)
    memcpy(bump(v, Width(struct str) + b2w(src->len)),
      src, sizeof(struct str) + src->len)); }

static bool eq_str(struct V *v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len && !strncmp(a->text, b->text, a->len); }

const struct typ str_typ = {
  .actn = ap_str,
  .emit = tx_str,
  .evac = cp_str,
  .hash = hx_str,
  .equi = eq_str, };

//symbols
//
sym ini_anon(void *_, U code) {
  sym y = _;
  y->act = act;
  y->typ = &sym_typ;
  y->nom = 0;
  y->code = code;
  return y; }

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

// FIXME the caller must ensure Avail >= Width(struct sym)
// (because GC here would void the tree)
sym intern(la v, sym *y, str b) {
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

Gc(cp_sym) {
  sym src = (sym) x;
  return (ob) (src->act = (vm*) (src->nom ?
    intern(v, &v->syms, (str) cp(v, (ob) src->nom, pool0, top0)) :
    ini_anon(bump(v, Width(struct sym) - 2), src->code))); }

static I hx_sym(la v, ob _) { return ((sym) _)->code; }

static void tx_sym(la v, FILE* o, ob _) {
  str s = ((sym) _)->nom;
  s ? fputsn(s->text, s->len, o) : fputs("#sym", o); }

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
//
// two easy potential optimizations are:
// - add a tail pointer to the start of the function,
//   so GC can find the head quickly (since often we
//   won't have an internal pointer)
// - tag the tail/head pointers instead of using a null
//   sentinel (but then the C compiler would need to
//   align functions)

mo mo_ini(void *_, size_t len) {
  struct tag *t = (struct tag*) ((mo) _ + len);
  return t->null = NULL, t->head = _; }

// allocate a thread
mo mo_n(la v, U n) {
  mo k = cells(v, n + Width(struct tag));
  return k ? mo_ini(k, n) : k; }

const struct typ sym_typ = {
  .actn = ap_nop,
  .emit = tx_sym,
  .evac = cp_sym,
  .hash = hx_sym,
  .equi = neql, };

sym symof(la v, str s) {
  if (Avail < Width(struct sym)) {
    bool _; with(s, _ = please(v, Width(struct sym)));
    if (!_) return 0; }
  return s ? intern(v, &v->syms, s) :
    ini_anon(bump(v, Width(struct sym) - 2),
      v->rand = lcprng(v->rand)); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.

vm setclo, genclo0, genclo1;

// try to get the name of a function
ob hnom(la v, mo x) {
  if (!livep(v, (ob) x)) return nil;
  vm *k = G(x);

  if (k == setclo || k == genclo0 || k == genclo1) // closure?
    return hnom(v, (mo) G(FF(x)));

  ob n = ((ob*) mo_tag(x))[-1];
  return homp(n) && livep(v, n) && G(n) == act ? n : nil; }


// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer. if a function has
// locals, this will have been initialized before they are
// referenced.
