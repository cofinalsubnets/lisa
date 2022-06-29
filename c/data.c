#include "la.h"

// functions for pairs and lists
static ob pair_(la v, ob a, ob b) {
  bool _;
  with(a, with(b, _ = please(v, 2)));
  return _ ? pair(v, a, b) : 0; }

ob pair(la v, ob a, ob b) {
  if (Avail < 2) return pair_(v, a, b);
  two w = bump(v, 2);
  w->a = a;
  w->b = b;
  return put2(w); }

uintptr_t llen(ob l) {
  uintptr_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

// pairs
Op(1, car, A(xp))
Op(1, cdr, B(xp))

Vm(cons) {
  if (Slack == 0) return Pray(1);
  hp[0] = xp;
  hp[1] = *sp++;
  xp = put2(hp);
  hp += 2;
  return ApN(1, xp); }

Vm(car_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, A(*fp->argv)); }

Vm(cdr_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, B(*fp->argv)); }

Ll(cons_u) {
  ArityCheck(2);
  if (Slack < 2) return Pray(2);
  two w = (two) hp;
  hp += 2;
  w->a = fp->argv[0];
  w->b = fp->argv[1];
  return ApC(ret, put2(w)); }


// hash tables
Vm(tblg) {
  Ary(2);
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
  Ary(2);
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
  Ary(1);
  TypeCheck(*fp->argv, Tbl);
  return
    xp = *fp->argv,
    Pack(),
    v->xp = tblss(v, 1, getZ(fp->argc)),
    Unpack(),
    ApC(xp ? ret : oom_err, xp); }

Vm(tblmk) {
  bool _;
  return
    Pack(),
    _ = (v->xp = table(v)) && tblss(v, 0, getZ(fp->argc)),
    Unpack(),
    ApC(_ ? ret : oom_err, xp); }

Vm(tblks) {
  Ary(1);
  TypeCheck(*fp->argv, Tbl);
  return
    Pack(),
    v->xp = tks_i(v, *fp->argv, 0),
    Unpack(),
    ApC(xp ? ret : oom_err, xp); }

Vm(tbll) {
  Ary(1);
  TypeCheck(*fp->argv, Tbl);
  return ApC(ret, putnum(gettbl(*fp->argv)->len)); }

Vm(tset) {
  ob x = *sp++, y = *sp++;
  return
    Pack(),
    v->xp = tbl_set(v, xp, x, y),
    Unpack(),
    xp ? ApN(1, xp) : ApC(oom_err, xp); }


static Inline N tbl_idx(N cap, N co) {
  return co & ((1 << cap) - 1); }

static Inline uintptr_t tbl_load(ob t) {
  return gettbl(t)->len >> gettbl(t)->cap; }

static ob tbl_ent_(la v, ob e, ob k) { return
  e == nil ? e : eql(R(e)[0], k) ? e : tbl_ent_(v, R(e)[2], k); }

static ob tbl_ent(la v, ob u, ob k) {
  tbl t = gettbl(u); return
    tbl_ent_(v, t->tab[tbl_idx(t->cap, hash(v, k))], k); }

static hasher
  hash_sym, hash_str, hash_two, hash_hom, hash_num, hash_tbl,
  *hashers[] = {
    [Hom] = hash_hom, [Two] = hash_two,
    [Str] = hash_str, [Num] = hash_num,
    [Sym] = hash_sym, [Tbl] = hash_tbl, };

Inline N hash(la v, ob x) { return hashers[Q(x)](v, x); }

static const N mix = 2708237354241864315;
static Inline N ror64(N x, N n) {
  return (x<<(64-n))|(x>>n); }
static Hash(hash_sym) {
  return getsym(x)->code; }
static Hash(hash_two) {
  return ror64(hash(v, A(x)) * hash(v, B(x)), 32); }
static Hash(hash_hom) {
  return hash(v, hnom(v, x)) ^ mix; }
static Hash(hash_num) {
  return ror64(mix * x, 16); }
static Hash(hash_tbl) {
  return ror64(mix * Tbl, 48); }
static Hash(hash_str) {
  str s = getstr(x);
  uintptr_t len = s->len;
  char *us = s->text;
  for (uintptr_t h = 1;; h ^= *us++, h *= mix)
    if (!len--) return h; }

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
  Z b = tbl_idx(y->cap, hash(v, key));
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
  return !t ? 0 :
    (t->len = t->cap = 0,
     t->tab = b,
     b[0] = nil,
     b[1] = 0,
     b[2] = (ob) b,
     puttbl(t)); }

Vm(tbld) {
  Ary(2);
  TypeCheck(fp->argv[0], Tbl);
  return
    Pack(),
    v->xp = tbl_del(v, fp->argv[0], fp->argv[1]),
    Unpack(),
    ApC(ret, xp); }

#include <string.h>

ob string(la v, const char* c) {
  Z bs = 1 + strlen(c);
  str o = cells(v, Width(str) + b2w(bs));
  return !o ? 0 :
    (o->len = bs,
     o->ext = 0,
     memcpy(o->text, c, bs),
     putstr(o)); }

// string instructions
Vm(strl) {
  Ary(1);
  TypeCheck(fp->argv[0], Str);
  return ApC(ret, putZ(getstr(*fp->argv)->len-1)); }

Vm(strg) {
  Ary(2);
  TypeCheck(fp->argv[0], Str);
  TypeCheck(fp->argv[1], Num);
  return ApC(ret,
    getZ(fp->argv[1]) < getstr(fp->argv[0])->len-1 ?
      putZ(getstr(fp->argv[0])->text[getZ(fp->argv[1])]) :
      nil); }

Vm(strconc) {
  Z l = getZ(fp->argc), sum = 0, i = 0;
  while (i < l) {
    ob x = fp->argv[i++];
    TypeCheck(x, Str);
    sum += getstr(x)->len - 1; }
  Z words = Width(str) + b2w(sum+1);
  Have(words);
  str d = (str) hp;
  hp += words;
  d->len = sum + 1;
  d->ext = 0;
  d->text[sum] = 0;
  for (str x; i;)
    x = getstr(fp->argv[--i]),
    sum -= x->len - 1,
    memcpy(d->text+sum, x->text, x->len - 1);
  return ApC(ret, putstr(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(strs) {
  Ary(3);
  TypeCheck(fp->argv[0], Str);
  TypeCheck(fp->argv[1], Num);
  TypeCheck(fp->argv[2], Num);
  str src = getstr(fp->argv[0]);
  Z lb = getnum(fp->argv[1]), ub = getnum(fp->argv[2]);
  lb = max(lb, 0);
  ub = min(ub, src->len-1);
  ub = max(ub, lb);
  Z words = Width(str) + b2w(ub - lb + 1);
  Have(words);
  str dst = (str) hp;
  hp += words;
  dst->len = ub - lb + 1;
  dst->ext = 0;
  dst->text[ub - lb] = 0;
  memcpy(dst->text, src->text + lb, ub - lb);
  return ApC(ret, putstr(dst)); }

Vm(strmk) {
  Z i = 0,
    bytes = getnum(fp->argc)+1,
    words = Width(str) + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (ob x; i < bytes-1; s->text[i++] = getnum(x)) {
    x = fp->argv[i];
    TypeCheck(x, Num);
    if (x == N0) break; }
  return s->text[i] = 0,
         s->ext = 0,
         s->len = i+1,
         ApC(ret, putstr(s)); }

//symbols

// FIXME this is bad
// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult. the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection.
ob sskc(pt v, ob *y, ob x) {
  int i;
  sym z;
  return !nilp(*y) ?
    (z = getsym(*y),
     i = strcmp(getstr(z->nom)->text, getstr(x)->text),
     i == 0 ? *y : sskc(v, i < 0 ? &z->r : &z->l, x)) :
    // FIXME the caller must ensure Avail >= Width(sym)
    // (because GC here would void the tree)
    (z = cells(v, Width(sym)),
     z->code = hash(v, putZ(hash(v, z->nom = x))),
     z->l = z->r = nil,
     *y = putsym(z)); }

ob intern(pt v, ob x) {
  bool _; return
    Avail >= Width(sym) ||
    (with(x, _ = please(v, Width(sym))), _) ?
      sskc(v, &v->syms, x) : 0; }

Vm(sym_u) { sym y; return 
  fp->argc > N0 && strp(*fp->argv) ?
    (Pack(),
     v->xp = intern(v, *fp->argv),
     Unpack(),
     ApC(xp ? ret : oom_err, xp)) :

  sp - hp < Width(sym) ?
    (v->xp = Width(sym),
     ApC(gc, xp)) :

  (y = (sym) hp,
   hp += Width(sym),
   y->nom = y->l = y->r = nil,
   y->code = v->rand = lcprng(v->rand),
   ApC(ret, putsym(y))); }

Dt(ystr_u) { return
  fp->argc == N0 ? ApC(ary_err, putZ(1)) :
  !IsA(Sym, xp = *fp->argv) ? ApC(dom_err, xp) :
  ApC(ret, getsym(xp)->nom); }
