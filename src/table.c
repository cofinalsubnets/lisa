#include "i.h"

static Inline word table_load(table t) {
  return t->len / t->cap; }

static Inline word tbl_idx(word cap, word co) {
  return (cap - 1) & co; }

// FIXME poor hashing method :(
static word hash_table(core f, word h) { return mix; }

static word copy_table(core f, word x, word *p0, word *t0) {
  table src = (table) x;
  word i = src->cap;
  table dst = bump(f, Width(struct table) + i);
  src->ap = (vm*) dst;
  dst->ap = data, dst->typ = &table_type;
  dst->len = src->len, dst->cap = src->cap;
  dst->tab = (void*) (dst + 1);

  //FIXME do these allocations in a block with the rest
  for (struct table_entry *s, *e, *d; i--; dst->tab[i] = e)
    for (s = src->tab[i], e = NULL; s;
      d = bump(f, Width(struct table_entry)),
      d->key = s->key, d->val = s->val,
      d->next = e, e = d,
      s = s->next);
  return (word) dst; }

static void walk_table(core f, word x, word *p0, word *t0) {
  table t = (table) x;
  f->cp += Width(struct table) + t->cap + t->len * Width(struct table_entry);
  for (word i = 0, lim = t->cap; i < lim; i++)
    for (struct table_entry *e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

struct typ table_type = {
  .hash = hash_table,
  .copy = copy_table,
  .evac = walk_table,
  .equal = literal_equal,
  .emit = generic_print, };

// this is a totally ad hoc, unproven hashing method.
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

word hash(core v, word x) {
  if (nump(x)) {
    const int half_word_bits = sizeof(word) * 4;
    x *= mix;
    x = (x << half_word_bits) | (x >> half_word_bits);
    return x; }

  if (datp(x)) return gettyp(x)->hash(v, x);
  if (!bounded(v->pool, x, v->pool+v->len)) return mix ^ (mix * x);
  // it's a function, hash by length
  struct tag *t = ttag((thread) x);
  word len = (cell) t - t->head;
  return mix ^ (mix * len); }

table new_table(core f) {
  table t = cells(f, Width(struct table) + 1);
  if (!t) return t;
  struct table_entry **tab = (void*) (t + 1);
  tab[0] = 0;
  t->ap = data, t->typ = &table_type;
  t->len = 0, t->cap = 1, t->tab = tab;
  return t; }


static NoInline table table_insert(core f, table t, word k, word v, word i) {
  struct table_entry *e;
  avec(f, t, avec(f, k, avec(f, v, e = cells(f, Width(struct table_entry)))));
  if (!e) return 0;
  e->key = k, e->val = v, e->next = t->tab[i];
  t->tab[i] = e;
  word cap0 = t->cap, load = ++t->len / cap0;
  if (load <= 1) return t;
  // grow the table
  struct table_entry **tab0 = t->tab, **tab1;
  word cap1 = 2 * cap0;
  avec(f, t, tab1 = cells(f, cap1));
  if (!tab1) return 0;
  memset(tab1, 0, cap1 * sizeof(word));
  for (word i; cap0--;)
    for (struct table_entry *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

  t->cap = cap1, t->tab = tab1;
  return t; }

static Inline word index_of_key(core f, table t, word k) {
  // relies on table capacity being a power of 2
  return (t->cap - 1) & hash(f, k); }

NoInline table table_set(core f, table t, word k, word v) {
  word index = index_of_key(f, t, k);
  struct table_entry *entry = t->tab[index];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  if (entry) return entry->val = v, t;
  return table_insert(f, t, k, v, index); }

static struct table_entry *table_delete_r(core f, table t, word k, word *v, struct table_entry *e) {
  if (!e) return e;
  if (eql(f, e->key, k)) return *v = e->val, e->next;
  return e->next = table_delete_r(f, t, k, v, e->next); }

static Inline word table_load_factor(table t) { return t->len / t->cap; }

static void table_shrink(core f, table t) {
  word cap = t->cap;
  struct table_entry *coll = 0, *x, *y; // collect all entries in one list
  for (word i = 0; i < cap; i++)
    for (x = t->tab[i], t->tab[i] = 0; x;)
      y = x, x = x->next, y->next = coll, coll = y;
  t->cap = cap >>= 2;
  for (word i; coll;)
    i = (cap - 1) & hash(f, coll->key),
    x = coll->next,
    coll->next = t->tab[i],
    t->tab[i] = coll,
    coll = x; }

NoInline word table_delete(core f, table t, word k, word v) {
  word idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->len / t->cap > 1) table_shrink(f, t);
  return v; }

Vm(tnew) {
  Have(Width(struct table) + 1);
  table t = (void*) hp;
  struct table_entry **tab = (void*) (t + 1);
  hp += Width(struct table) + 1;
  t->ap = data, t->typ = &table_type;
  t->len = 0, t->cap = 1, t->tab = tab;
  return op(1, (word) t); }

word table_get(core f, table t, word k, word zero) {
  struct table_entry *entry = t->tab[index_of_key(f, t, k)];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  return entry ? entry->val : zero; }

Vm(tget) {
  return op(3, !tblp(sp[0]) ? sp[2] :
    table_get(f, (table) sp[0], sp[1], sp[2])); }

Vm(tset) {
  word x = sp[0];
  if (!tblp(x)) return op(3, nil);
  Pack(f);
  table t = table_set(f, (table) sp[0], sp[1], sp[2]);
  Unpack(f);
  return !t ? Oom : op(3, sp[2]); }

Vm(tdel) {
  word x = sp[0];
  return op(3, !tblp(x) ? nil :
    table_delete(f, (table) x, sp[1], sp[2])); }

Vm(tlen) {
  word x = sp[0];
  if (!tblp(x)) return op(1, nil);
  table t = (table) x;
  return op(1, putnum(t->len)); }

Vm(tkeys) {
  if (!tblp(sp[0])) return op(1, nil);
  table t = (table) sp[0];
  word len = t->len, list = nil;
  Have(len * Width(struct pair));
  pair pairs = (void*) hp;
  hp += len * Width(struct pair);
  for (int i = t->cap; i;)
    for (struct table_entry *e = t->tab[--i]; e;)
      pairs->ap = data, pairs->typ = &typ_two,
      pairs->a = e->key, pairs->b = list,
      list = (word) pairs, pairs++;
  return op(1, list); }
