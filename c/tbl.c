#include "lips.h"

SI i64 tbl_idx(u64 cap, u64 co) {
  return co & ((1 << cap) - 1); }

SI u64 tbl_load(obj t) {
  return T(t)->len >> T(t)->cap; }


static ent tbl_ent(lips v, obj u, obj k) {
  tbl t = gettbl(u);
  ent e = t->tab[tbl_idx(t->cap, hash(v, k))];
  for (; e; e = e->next) if (eql(e->key, k)) return e;
  return NULL; }

typedef u64 hasher(lips, obj);
static hasher hash_sym, hash_str, hash_two, hash_hom, hash_num, hash_vec, hash_nil;
static hasher *hashers[] = {
  [Nil] = hash_nil, [Hom] = hash_hom, [Two] = hash_two, [Vec] = hash_vec,
  [Str] = hash_str, [Num] = hash_num, [Sym] = hash_sym, [Tbl] = hash_nil };

Inline u64 hash(lips v, obj x) { return hashers[kind(x)](v, x); }

SI u64 ror64(u64 x, u64 n) { return (x<<(64-n))|(x>>n); }
static u64 hash_sym(lips v, obj y) { return getsym(y)->code; }
static u64 hash_two(lips v, obj w) { return ror64(hash(v, A(w)) * hash(v, B(w)), 32); }
static u64 hash_hom(lips v, obj h) { return hash(v, homnom(v, h)) ^ mix; }
static u64 hash_num(lips v, obj n) { return ror64(mix * n, 16); }
static u64 hash_vec(lips v, obj x) { return ror64(mix * V(x)->len, 32); }
static u64 hash_nil(lips v, obj _) { return ror64(mix * kind(nil), 48); }
static u64 hash_str(lips v, obj x) {
  str s = S(x);
  u64 len = s->len;
  char *us = s->text;
  for (u64 h = 1;; h ^= *us++, h *= mix)
    if (!len--) return h; }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static u0 tbl_fit(lips v, obj t) {
  if (tbl_load(t)) return;

  ent e = NULL, f, g;
  tbl u = T(t);

  // collect all entries
  for (u64 i = 1 << u->cap; i--;)
    for (f = u->tab[i], u->tab[i] = NULL; f;
      g = f->next, f->next = e,
      e = f, f = g);

  // shrink bucket array
  while (u->cap && tbl_load(t) < 1) u->cap--;

  // reinsert
  while (e) {
    u64 i = tbl_idx(u->cap, hash(v, e->key));
    f = e->next,
    e->next = u->tab[i],
    u->tab[i] = e,
    e = f; } }

static obj tbl_del(lips v, obj t, obj key) {
  tbl y = gettbl(t);
  obj val = nil;
  i64 b = tbl_idx(y->cap, hash(v, key));
  ent e = y->tab[b];
  struct ent prev = {0,0,e};
  for (ent l = &prev; l && l->next; l = l->next)
    if (l->next->key == key) {
      val = l->next->val;
      l->next = l->next->next;
      y->len--;
      break; }
  y->tab[b] = prev.next;
  tbl_fit(v, t);
  return val; }


// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static obj tbl_grow(lips v, obj t) {
  ent *tab0, *tab1;
  u64 cap0 = T(t)->cap, cap1 = cap0 + 1;
  with(t, tab1 = cells(v, 1<<cap1));
  bind(tab1, tab1);
  set64(tab1, 0, 1<<cap1);
  tab0 = T(t)->tab;

  for (u64 i, cap = 1 << cap0; cap--;)
    for (ent e, es = tab0[cap]; es;
      e = es, es = es->next,
      i = tbl_idx(cap1, hash(v, e->key)),
      e->next = tab1[i], tab1[i] = e);

  T(t)->cap = cap1, T(t)->tab = tab1;
  return t; }

obj tbl_set_s(lips v, obj t, obj k, obj x) {
  u64 i = tbl_idx(gettbl(t)->cap, hash(v, k));
  ent e = tbl_ent(v, t, k);
  if (e) return e->val = x;

  // it's not here, so allocate an entry
  with(t, with(k, with(x, e = cells(v, Width(ent)))));
  bind(e, e);
  e->key = k, e->val = x;
  tbl y = gettbl(t);
  e->next = y->tab[i];
  y->tab[i] = e;
  y->len += 1;

  return x; }

obj tbl_set(lips v, obj t, obj k, obj x) {
  with(t, x = tbl_set_s(v, t, k, x));
  bind(x, x);
  if (tbl_load(t) > 1) with(x, t = tbl_grow(v, t));
  bind(t, t);
  return x; }

obj tbl_get(lips v, obj t, obj k) {
  ent e = tbl_ent(v, t, k);
  return e ? e->val : 0; }

obj table(lips v) {
  tbl t;
  bind(t, cells(v, Width(tbl) + 1));
  ent *b = (ent*)(t+1);
  t->len = t->cap = 0, t->tab = b, *b = NULL;
  return puttbl(t); }

static obj tblkeys_j(lips v, ent e, obj l) {
  if (!e) return l;
  obj x = e->key;
  with(x, l = tblkeys_j(v, e->next, l));
  bind(l, l);
  return pair(v, x, l); }

static obj tblkeys_i(lips v, obj t, i64 i) {
  obj k;
  if (i == 1 << gettbl(t)->cap) return nil;
  with(t, k = tblkeys_i(v, t, i+1));
  bind(k, k);
  return tblkeys_j(v, gettbl(t)->tab[i], k); }

Inline obj tblkeys(lips v, obj t) {
  return tblkeys_i(v, t, 0); }

#include "terp.h"

// hash tables
Vm(tblg) {
  Ary(2);
  Tc(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  Go(ret, xp ? xp : nil); }

OP1(tget, (xp = tbl_get(v, xp, *sp++)) ? xp : nil)
OP1(thas, tbl_get(v, xp, *sp++) ? ok : nil)
OP1(tlen, _N(gettbl(xp)->len))

Vm(tkeys) {
  CallC(v->xp = tblkeys(v, xp));
  bind(xp, xp);
  Next(1); }

Vm(tblc) {
  Ary(2);
  Tc(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  Go(ret, xp ? ok : nil); }

static obj tblss(lips v, i64 i, i64 l) {
  mem fp = v->fp;
  if (i > l-2) return Argv[i-1];
  obj _;
  bind(_, tbl_set(v, v->xp, Argv[i], Argv[i+1]));
  return tblss(v, i+2, l); }

Vm(tbls) {
  Ary(1);
  xp = *Argv;
  Tc(xp, Tbl);
  CallC(v->xp = tblss(v, 1, N(Argc)));
  bind(xp, xp);
  Jump(ret); }

Vm(tblmk) {
  Pack();
  bind(v->xp, table(v)); // xp <- table
  bind(xp, tblss(v, 0, N(Argc))); // _ <- updates
  Unpack();
  Jump(ret); }

Vm(tbld) {
  Ary(2);
  Tc(Argv[0], Tbl);
  CallC(v->xp = tbl_del(v, Argv[0], Argv[1]));
  Jump(ret); }

Vm(tblks) {
  Ary(1);
  Tc(*Argv, Tbl);
  CallC(v->xp = tblkeys(v, *Argv));
  bind(xp, xp);
  Jump(ret); }

Vm(tbll) {
  Ary(1);
  Tc(*Argv, Tbl);
  Go(ret, _N(gettbl(*Argv)->len)); }

Vm(tset) {
  obj x = *sp++, y = *sp++;
  CallC(v->xp = tbl_set(v, xp, x, y));
  bind(xp, xp);
  Next(1); }
