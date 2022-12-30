#include "la.h"
#include <stdarg.h>

// push things onto the stack
static NoInline u1 pushsr(la v, U i, va_list xs) {
  u1 _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i+1, xs)),
     _ && (*--v->sp = x, true)); }

u1 pushs(la v, ...) {
  u1 _; va_list xs; return
  va_start(xs, v),
  _ = pushsr(v, 0, xs),
  va_end(xs),
  _; }

static vm ap_two;
static u1 eq_two(la, I, I);
static u0 tx_two(la, FILE*, I);
static I cp_two(la, I, I*, I*),
         hx_two(la, I);

const struct typ two_typ = {
  .does = ap_two,
  .emit = tx_two,
  .evac = cp_two,
  .hash = hx_two,
  .equi = eq_two, };

two two_ini(void *_, ob a, ob b) {
  two w = _; return
    w->data = data,
    w->typ = &two_typ,
    w->a = a, w->b = b,
    w; }

// pairs and lists
static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok; return
    with(a, with(b, ok = please(v, wsizeof(struct two)))),
    ok ? pair(v, a, b) : 0; }

NoInline two pair(la v, ob a, ob b) {
  return Avail >= wsizeof(struct two) ?
    two_ini(bump(v, wsizeof(struct two)), a, b) :
    pair_gc(v, a, b); }

// length of list
U llen(ob l) {
  for (U i = 0;;)
    if (twop(l)) l = B(l), i++;
    else return i; }

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(wsizeof(struct two));
  xp = (ob) two_ini(hp, xp, *sp++);
  hp += wsizeof(struct two);
  return ApN(1, xp); }

Vm(car_f) {
  if (fp->argc)
    xp = fp->argv[0],
    xp = twop(xp) ? A(xp) : xp;
  return ApC(ret, xp); }

Vm(cdr_f) {
  if (fp->argc)
    xp = fp->argv[0],
    xp = twop(xp) ? B(xp) : nil;
  return ApC(ret, xp); }

Vm(cons_f) {
  if (fp->argc) {
    U n = wsizeof(struct two) * (fp->argc - 1);
    Have(n);
    two w = (two) hp;
    hp += n;
    xp = fp->argv[fp->argc-1];
    for (size_t i = fp->argc - 1; i--;
      xp = (ob) two_ini(w+i, fp->argv[i], xp)); }
  return ApC(ret, xp); }

static Vm(ap_two) { return
  ApC(ret, fp->argc ? B(ip) : A(ip)); }

static Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, wsizeof(struct two));
  src->data = (vm*) dst;
  return (ob) two_ini(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

static void tx_two(la v, la_io o, ob x) {
  putc('(', o);
  for (;;) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) break;
    putc(' ', o); }
  putc(')', o); }

static I hx_two(la v, ob x) {
  I hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(I)); }

static u1 eq_two(la v, ob x, ob y) {
  return (typ) GF(y) == &two_typ &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }


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

#include <string.h>

str str_ini(void *_, size_t len) {
  str s = _;
  s->data = data, s->typ = &str_typ;
  s->len = len;
  return s; }

static I hx_str(la v, ob _) {
  str s = (str) _;
  I h = 1;
  U words = s->len / sizeof(ob),
    bytes = s->len % sizeof(ob);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const I *ws = (I*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

SI u1 escapep(char c) { return c == '\\' || c == '"'; }

static u0 tx_str(struct carrier *v, FILE *o, ob _) {
  str s = (str) _;
  U len = s->len;
  const char *text = s->text;
  putc('"', o);
  while (len--) {
    char c = *text++;
    if (escapep(c)) putc('\\', o);
    putc(c, o); }
  putc('"', o); }

static Gc(cp_str) {
  str src = (str) x;
  return (ob) (src->data = (vm*)
    memcpy(bump(v, wsizeof(struct str) + b2w(src->len)),
      src, sizeof(struct str) + src->len)); }

static u1 eq_str(struct carrier *v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len &&
    !strncmp(a->text, b->text, a->len); }

static vm ap_str;

const struct typ str_typ = {
  .does = ap_str,
  .emit = tx_str,
  .evac = cp_str,
  .hash = hx_str,
  .equi = eq_str, };

static Vm(ap_str) {
  str s = (str) ip;
  fputsn(s->text, s->len, stdout);
  return ApC(ret, (ob) ip); }

// string instructions
Vm(slen_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(strp(xp));
  return ApC(ret, putnum(((str)xp)->len)); }

Vm(sget_f) {
  ArityCheck(2);
  Check(strp(fp->argv[0]));
  str s = (str) fp->argv[0];
  intptr_t i = getnum(fp->argv[1]);
  xp = i < 0 || i >= s->len ? nil : putnum(s->text[i]);
  return ApC(ret, xp); }

Vm(scat_f) {
  size_t sum = 0, i = 0;
  for (size_t l = fp->argc; i < l;) {
    ob x = fp->argv[i++];
    Check(strp(x));
    sum += ((str)x)->len; }
  size_t words = wsizeof(struct str) + b2w(sum);
  Have(words);
  str d = str_ini(hp, sum);
  hp += words;
  for (str x; i--;
    x = (str) fp->argv[i],
    sum -= x->len,
    memcpy(d->text+sum, x->text, x->len));
  return ApC(ret, (ob) d); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_f) {
  ArityCheck(2);
  Check(strp(fp->argv[0]));
  str src = (str) fp->argv[0];
  intptr_t
    lb = getnum(fp->argv[1]),
    ub = fp->argc > 2 ? getnum(fp->argv[2]) : INTPTR_MAX;
  lb = max(lb, 0);
  ub = min(ub, src->len);
  ub = max(ub, lb);
  U len = ub - lb,
    words = wsizeof(struct str) + b2w(len);
  Have(words);
  str dst = str_ini(hp, len);
  hp += words;
  memcpy(dst->text, src->text + lb, len);
  return ApC(ret, (ob) dst); }

Vm(str_f) {
  U len = fp->argc,
    words = wsizeof(struct str) + b2w(len);
  Have(words);
  str s = str_ini(hp, len);
  hp += words;
  while (len--) s->text[len] = getnum(fp->argv[len]);
  return ApC(ret, (ob) s); }


//symbols
//
static sym ini_anon(void *_, size_t code) {
  sym y = _;
  y->data = data;
  y->typ = &sym_typ;
  y->nom = 0;
  y->code = code;
  return y; }

static sym ini_sym(void *_, str nom, size_t code) {
  sym y = _;
  y->data = data;
  y->typ = &sym_typ;
  y->nom = nom;
  y->code = code;
  y->l = y->r = 0;
  return y; }

// FIXME this should probably change at some point.
// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle which seems to keep it
// from getting too bad. this is much more performant than a
// list & uses less memory than a hash table, but maybe we
// should use a table anyway.
//
// FIXME the caller must ensure Avail >= wsizeof(struct sym)
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
  return *y = ini_sym(bump(v, wsizeof(struct sym)), b,
    hash(v, putnum(hash(v, (ob) b)))); }

static Gc(cp_sym) {
  sym src = (sym) x;
  return (ob) (src->data = (vm*) (src->nom ?
    intern(v, &v->syms, (str) cp(v, (ob) src->nom, pool0, top0)) :
    ini_anon(bump(v, wsizeof(struct sym) - 2), src->code))); }

static I hx_sym(la v, ob _) { return ((sym) _)->code; }

static u0 tx_sym(la v, la_io o, ob _) {
  str s = ((sym) _)->nom;
  s ? fputsn(s->text, s->len, o) : fputs("#sym", o); }

static Vm(ap_nop) { return ApC(ret, (ob) ip); }

const struct typ sym_typ = {
  .does = ap_nop,
  .emit = tx_sym,
  .evac = cp_sym,
  .hash = hx_sym,
  .equi = neql, };

sym symof(la v, str s) {
  if (Avail < wsizeof(struct sym)) {
    bool _; with(s, _ = please(v, wsizeof(struct sym)));
    if (!_) return 0; }
  return s ? intern(v, &v->syms, s) :
    ini_anon(bump(v, wsizeof(struct sym) - 2), v->rand = lcprng(v->rand)); }

Vm(sym_f) {
  str i = fp->argc && strp(fp->argv[0]) ? (str) fp->argv[0] : 0;
  sym y; CallOut(y = symof(v, i));
  return y ? ApC(ret, (ob) y) : ApC(xoom, xp); }

Vm(ynom_f) {
  if (fp->argc && symp(fp->argv[0]))
    xp = (ob) ((sym) fp->argv[0])->nom,
    xp = xp ? xp : nil;
  return ApC(ret, xp); }

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

mo mo_ini(void *_, U len) {
  struct tl *t = (struct tl*) ((mo) _ + len);
  return t->null = NULL, t->head = _; }

// allocate a thread
mo mo_n(la v, U n) {
  mo k = cells(v, n + wsizeof(struct tl));
  return k ? mo_ini(k, n) : k; }

// instructions for the internal compiler
// initialize a function
Vm(hom_f) {
  if (fp->argc && nump(fp->argv[0])) {
    U len = getnum(fp->argv[0]);
    Have(len + wsizeof(struct tl));
    mo k = setw(mo_ini(hp, len), nil, len);
    hp += len + wsizeof(struct tl);
    xp = (ob) (k + len); }
  return ApC(ret, xp); }

// trim a function after writing out code
Vm(hfin_f) {
  if (fp->argc) {
    ob x = fp->argv[0];
    if (homp(x) && G(x) != data)
      mo_tl((mo) x)->head = (mo) x,
      xp = x; }
  return ApC(ret, xp); }

// emit data
Vm(poke_f) {
  if (fp->argc) {
    U i = fp->argc - 1;
    if (homp(fp->argv[i])) {
      mo k = (mo) fp->argv[i];
      while (i--) G(--k) = (vm*) fp->argv[i];
      xp = (ob) k; } }
  return ApC(ret, xp); }

Vm(peek_f) {
  if (fp->argc && homp(fp->argv[0])) xp = (ob) G(fp->argv[0]);
  return ApC(ret, xp); }

// thread pointer arithmetic -- not bounds checked!
Vm(seek_f) {
  if (fp->argc >= 2 && homp(fp->argv[0]) && nump(fp->argv[1]))
    xp = (ob) ((mo) fp->argv[0] + getnum(fp->argv[1]));
  return ApC(ret, xp); }

// TODO maybe we could do this with closures instead?
Vm(data) { return ApC(((typ) GF(ip))->does, xp); }

// closure functions
//
// pop some things off the stack into an array.
Vm(take) {
  ob n = getnum((ob) GF(ip));
  Have(n + wsizeof(struct tl));
  mo k = mo_ini(cpyw_r2l(hp, sp, n), n);
  hp += n + wsizeof(struct tl);
  return ApC(ret, (ob) k); }

// set the closure for this frame
static Vm(setclo) { return
  fp->clos = (ob*) GF(ip),
  ApY(G(FF(ip)), xp); }

// finalize function instance closure
static Vm(genclo1) { return
  G(ip) = setclo,
  GF(ip) = (vm*) xp,
  ApY(ip, xp); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.

struct clo_env {
  mo cons;
  ob loc, *clo, argc, argv[]; };

static Vm(genclo0) {
  struct clo_env *ec = (void*) GF(ip);
  U adic = getnum(ec->argc);
  Have(wsizeof(struct sf) + adic + 1);
  sf subd = fp; return
    G(ip) = genclo1,
    sp = (ob*) (fp = (sf) (sp - adic) - 1),
    cpyw_r2l(fp->argv, ec->argv, adic),
    fp->retp = ip,
    fp->subd = subd,
    fp->argc = adic,
    fp->clos = (ob*) ec->clo,
    *--sp = ec->loc,
    ApY(ec->cons, xp); }

// the next few functions create and store
// lexical environments.
static Vm(enclose) {
  U thd_len = 3 + wsizeof(struct tl),
    env_len = fp->argc + wsizeof(struct tl) +
                         wsizeof(struct clo_env);
  Have(env_len + thd_len);
  ob codeXcons = (ob) GF(ip); // pair of the compiled thread & closure constructor
  ob *block = hp;
  hp += env_len + thd_len;

  struct clo_env *env = (void*)
    mo_ini(block, wsizeof(struct clo_env) + fp->argc); // holds the closure environment & constructor
  env->cons = (mo) B(codeXcons);
     // TODO get closure out of stack frame; configure via xp
  env->loc = nilp(xp) ? xp : ((ob*)fp)[-1];
  env->clo = fp->clos;
  env->argc = putnum(fp->argc);
  cpyw_r2l(env->argv, fp->argv, fp->argc);

  mo thd = mo_ini(block + env_len, 3); // the thread that actually gets returned
  G(thd) = genclo0;
  GF(thd) = (vm*) env;
  G(FF(thd)) = (vm*) A(codeXcons);

  return ApN(2, (ob) thd); }

// these pass the locals array to encl in xp
// TODO do the same thing with the closure ptr
Vm(encl1) { return ApC(enclose, putnum(1)); }
// FIXME if there are no locals we don't need to defer closure construction!
Vm(encl0) { return ApC(enclose, putnum(0)); }

// try to get the name of a function
ob hnom(la v, mo x) {
  if (!livep(v, (ob) x)) return nil;
  vm *k = G(x);

  if (k == setclo || k == genclo0 || k == genclo1) // closure?
    return hnom(v, (mo) G(FF(x)));

  ob n = ((ob*) mo_tl(x))[-1];
  return homp(n) && livep(v, n) && G(n) == data ? n : nil; }
