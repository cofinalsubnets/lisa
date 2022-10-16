#include "la.h"
#include "vm.h"

// FIXME remove macros
#define mm_u(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);}\
  return ApC(ret, putnum(xp));}

Vm(sub_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  if (xp == 1) return ApC(ret, putnum(-getnum(*fp->argv)));
  mm_u(xp-1, fp->argv+1, getnum(*fp->argv), -); }

Vm(sar_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(fp->argv[0], Num);
  mm_u(getnum(fp->argc)-1, fp->argv+1, getnum(fp->argv[0]), >>); }

Vm(sal_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(fp->argv[0], Num);
  mm_u(getnum(fp->argc)-1, fp->argv+1, getnum(fp->argv[0]), <<); }

Vm(dqv) {
  if (xp == putnum(0)) return ApC(dom_err, xp);
  xp = putnum(getnum(*sp++) / getnum(xp));
  return ApN(1, xp); }

Vm(mod) {
  if (xp == putnum(0)) return ApC(dom_err, xp);
  xp = putnum(getnum(*sp++) % getnum(xp));
  return ApN(1, xp); }

#define mm_void(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);\
    if (x == N0) return ApC(dom_err, x);}\
  return ApC(ret, putnum(xp));}

Vm(div_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, T);
  TypeCheck(fp->argv[0], Num);
  mm_void(xp-1, fp->argv+1, getnum(fp->argv[0]), /); }

Vm(mod_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, T);
  TypeCheck(*fp->argv, Num);
  mm_void(xp-1, fp->argv+1, getnum(*fp->argv), %); }

Vm(rnd_u) { return
  xp = lcprng(v->rand),
  v->rand = xp,
  ApC(ret, putnum(xp)); }

Vm(neg) { return ApN(1, putnum(-getnum(xp))); }

Vm(bnot_u) {
  ArityCheck(1);
  return ApC(ret, putnum(~getnum(*fp->argv))); }

Vm(add_u) { mm_u(getnum(fp->argc), fp->argv, 0, +); }
Vm(bor_u) { mm_u(getnum(fp->argc), fp->argv, 0, |); }
Vm(bxor_u) { mm_u(getnum(fp->argc), fp->argv, 0, ^); }
Vm(mul_u) { mm_u(getnum(fp->argc), fp->argv, 1, *); }
Vm(band_u) { mm_u(getnum(fp->argc), fp->argv, -1, &); }

Vm(add) { xp = xp + *sp++ - Num; return ApN(1, xp); }
Vm(sub) { xp = *sp++ - xp + Num; return ApN(1, xp); }

Vm(bor) { xp = xp | *sp++; return ApN(1, xp); }
Vm(band) { xp = xp & *sp++; return ApN(1, xp); }
Vm(bxor) { xp = (xp ^ *sp++) | Num; return ApN(1, xp); }

Vm(mul) { xp = putnum(getnum(*sp++) * getnum(xp)); return ApN(1, xp); }
Vm(sar) { xp = putnum(getnum(*sp++) >> getnum(xp)); return ApN(1, xp); }
Vm(sal) { xp = putnum(getnum(*sp++) << getnum(xp)); return ApN(1, xp); }

#include <string.h>

ob string(la v, const char* c) {
  intptr_t bs = 1 + slen(c);
  str o = cells(v, Width(str) + b2w(bs));
  if (!o) return 0;
  o->len = bs;
  o->ext = 0;
  memcpy(o->text, c, bs);
  return putstr(o); }

// string instructions
Vm(slen_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Str);
  return ApC(ret, putnum(getstr(*fp->argv)->len-1)); }

Vm(sget_u) {
  ArityCheck(2);
  TypeCheck(fp->argv[0], Str);
  TypeCheck(fp->argv[1], Num);
  return ApC(ret,
    getnum(fp->argv[1]) < getstr(fp->argv[0])->len-1 ?
      putnum(getstr(fp->argv[0])->text[getnum(fp->argv[1])]) :
      nil); }

Vm(scat_u) {
  intptr_t l = getnum(fp->argc), sum = 0, i = 0;
  while (i < l) {
    ob x = fp->argv[i++];
    TypeCheck(x, Str);
    sum += getstr(x)->len - 1; }
  intptr_t words = Width(str) + b2w(sum+1);
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
Vm(ssub_u) {
  ArityCheck(3);
  TypeCheck(fp->argv[0], Str);
  TypeCheck(fp->argv[1], Num);
  TypeCheck(fp->argv[2], Num);
  str src = getstr(fp->argv[0]);
  intptr_t lb = getnum(fp->argv[1]),
           ub = getnum(fp->argv[2]);
  lb = max(lb, 0);
  ub = min(ub, src->len-1);
  ub = max(ub, lb);
  intptr_t words = Width(str) + b2w(ub - lb + 1);
  Have(words);
  str dst = (str) hp;
  hp += words;
  dst->len = ub - lb + 1;
  dst->ext = 0;
  dst->text[ub - lb] = 0;
  memcpy(dst->text, src->text + lb, ub - lb);
  return ApC(ret, putstr(dst)); }

Vm(str_u) {
  intptr_t i = 0,
    bytes = getnum(fp->argc)+1,
    words = Width(str) + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (ob x; i < bytes-1; s->text[i++] = getnum(x)) {
    x = fp->argv[i];
    TypeCheck(x, Num);
    if (x == N0) break; }
  s->text[i] = 0;
  s->ext = 0;
  s->len = i+1;
  return ApC(ret, putstr(s)); }

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
ob sskc(la v, ob *y, ob x) {
  int i; sym z; return
    !nilp(*y) ?
      (z = getsym(*y),
       i = scmp(getstr(z->nom)->text, getstr(x)->text),
       i == 0 ? *y : sskc(v, i < 0 ? &z->r : &z->l, x)) :
    // FIXME the caller must ensure Avail >= Width(sym)
    // (because GC here would void the tree)
    (z = cells(v, Width(sym)),
     z->code = hash(v, putnum(hash(v, z->nom = x))),
     z->l = z->r = nil,
     *y = putsym(z)); }

ob intern(la v, ob x) {
  bool _; return
    Avail >= Width(sym) ||
    (with(x, _ = please(v, Width(sym))), _) ?
      sskc(v, &v->syms, x) :
      0; }

Vm(sym_u) {
  Have(Width(sym));
  if (Argc > N0 && strp(Argv[0]))
    return ApC(ret, sskc(v, &v->syms, fp->argv[0]));
  sym y = (sym) hp;
  hp += Width(sym);
  y->nom = y->l = y->r = nil;
  y->code = v->rand = lcprng(v->rand);
  return ApC(ret, putsym(y)); }

Vm(ystr_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Sym);
  return ApC(ret, getsym(xp)->nom); }

// pairs and lists
ob pair(la v, ob a, ob b) {
  if (Avail < 2) {
    bool _;
    with(a, with(b, _ = please(v, 2)));
    if (!_) return 0; }
  two w = bump(v, 2);
  return w->a = a, w->b = b, puttwo(w); }

// length of list
size_t llen(ob l) {
  size_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

// vm functions
#include "vm.h"

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have1();
  hp[0] = xp;
  hp[1] = *sp++;
  xp = puttwo(hp);
  hp += 2;
  return ApN(1, xp); }

Vm(car_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, A(fp->argv[0])); }

Vm(cdr_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, B(fp->argv[0])); }

Vm(cons_u) {
  ArityCheck(2);
  Have(2);
  xp = puttwo(hp);
  hp += 2;
  A(xp) = fp->argv[0];
  B(xp) = fp->argv[1];
  return ApC(ret, xp); }


// hash tables
// some of the worst code is here :(
//
// a big random number
static const uint64_t mix = 2708237354241864315;

static Inline size_t ror(size_t x, size_t n) {
  return (x<<((8*sizeof(size_t))-n))|(x>>n); }

static Inline size_t tbl_idx(size_t cap, size_t co) {
  return co & ((1 << cap) - 1); }

static Inline size_t tbl_load(ob t) {
  return gettbl(t)->len >> gettbl(t)->cap; }

static ob
  tbl_ent(la, ob, ob),
  tbl_grow(la, ob),
  tbl_del(la, ob, ob),
  tblss(la, intptr_t, intptr_t),
  tks_i(la, ob, intptr_t),
  tks_j(la, ob, ob),
  tbl_set_s(la, ob, ob, ob);

size_t hash(la v, ob x) {
  switch (TypeOf(x)) {
    case Sym: return getsym(x)->code;
    case Two: return ror(hash(v, A(x)) * hash(v, B(x)), 32);
    case Hom: return hash(v, hnom(v, x)) ^ mix;
    case Tbl: return ror(mix * Tbl, 48);
    case Num: return ror(mix * x, 16);
    case Str: default: {
      str s = getstr(x);
      size_t len = s->len;
      char *us = s->text;
      for (size_t h = 1;; h ^= *us++, h *= mix)
        if (!len--) return h; } } }

ob table(la v) {
  tbl t = cells(v, Width(tbl) + 3);
  if (!t) return 0;
  ob *b = (ob*) (t + 1);
  t->len = t->cap = 0;
  t->tab = b;
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
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, fp->argv[0], fp->argv[1]);
  return ApC(ret, xp ? xp : nil); }

Vm(tdel_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  Pack();
  v->xp = tbl_del(v, fp->argv[0], fp->argv[1]);
  Unpack();
  return ApC(ret, xp); }

Vm(tget) { return
  xp = tbl_get(v, xp, *sp++),
  ApN(1, xp ? xp : nil); }

Vm(thas) { return
  xp = tbl_get(v, xp, *sp++),
  ApN(1, xp ? T : nil); }

Vm(tlen) { return ApN(1, putnum(gettbl(xp)->len)); }

Vm(tkeys) {
  Pack();
  v->xp = tks_i(v, xp, 0);
  Unpack();
  return xp ? ApN(1, xp) : ApC(oom_err, nil); }

Vm(thas_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, fp->argv[0], fp->argv[1]);
  return ApC(ret, xp ? T : nil); }

Vm(tset_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Tbl);
  Pack();
  v->xp = tblss(v, 1, getnum(fp->argc));
  Unpack();
  return ApC(xp ? ret : oom_err, xp); }

Vm(tbl_u) {
  Pack();
  bool _ = (v->xp = table(v)) && tblss(v, 0, getnum(fp->argc));
  Unpack();
  return ApC(_ ? ret : oom_err, xp); }

Vm(tkeys_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Tbl);
  Pack();
  v->xp = tks_i(v, Argv[0], 0);
  Unpack();
  return ApC(xp ? ret : oom_err, xp); }

Vm(tlen_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Tbl);
  return ApC(ret, putnum(gettbl(*fp->argv)->len)); }

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

static ob tks_i(la v, ob t, intptr_t i) {
  ob k; return i == 1 << gettbl(t)->cap ? nil :
    (with(t, k = tks_i(v, t, i+1)),
     k ? tks_j(v, gettbl(t)->tab[i], k) : 0); }

static ob tblss(la v, intptr_t i, intptr_t l) {
  fr fp = v->fp;
  return
    i > l - 2 ? fp->argv[i - 1] :
    !tbl_set(v, v->xp, fp->argv[i], fp->argv[i + 1]) ? 0 :
    tblss(v, i + 2, l); }

static ob tbl_ent_(la v, ob e, ob k) {
  return nilp(e) || eql(R(e)[0], k) ? e : tbl_ent_(v, R(e)[2], k); }

static ob tbl_ent(la v, ob u, ob k) {
  tbl t = gettbl(u);
  u = t->tab[tbl_idx(t->cap, hash(v, k))];
  return tbl_ent_(v, u, k); }
