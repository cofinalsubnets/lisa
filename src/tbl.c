#include "lips.h"
#include "tbl.h"
#include "eql.h"
#include "two.h"
#include "hom.h"
#include "mem.h"

static Inline u64 hash_bytes(u64 len, char *us) {
  for (u64 h = mix;; h ^= *us++, h *= mix)
    if (!len--) return h; }

static Inline i64 bucket_index(u64 cap, u64 co) {
  return co % cap; }

static Inline ent bucket(obj t, u64 code) {
  return gettbl(t)->tab[bucket_index(gettbl(t)->cap, code)]; }

u64 hash(lips v, obj x) {
  switch (kind(x)) {
    case Sym: return getsym(x)->code;
    case Str: return hash_bytes(getstr(x)->len, getstr(x)->text);
    case Two: return hash(v, X(x)) ^ hash(v, Y(x));
    case Hom: return hash(v, homnom(v, x)) ^ mix;
    case Num: return rotr64(mix * x, 16);
    case Vec: return rotr64(mix * V(x)->len, 32);
    default:  return rotr64(mix * kind(x), 48); } }

// grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static obj grow(lips v, obj t, i64 ns) {
  ent e, ch, *b, *d;
  with(t, set64((mem) (b = cells(v, ns)), 0, ns));
  tbl o = gettbl(t);
  i64 u, n = o->cap;
  d = o->tab; o->tab = b; o->cap = ns;
  while (n--) for (ch = d[n]; ch;
    e = ch,
    ch = ch->next,
    u = bucket_index(ns, hash(v, e->key)),
    e->next = b[u],
    b[u] = e);
  return t; }

// it's possible to shrink a table without allocating any
// new memory.
static u0 shrink(lips v, obj t) {
  ent e = NULL, f, g;
  tbl u = gettbl(t);
  for (t = 0; t < u->cap; t++)
    for (f = u->tab[t], u->tab[t] = NULL; f;
      g = f->next,
      f->next = e,
      e = f, f = g);
  for (u->cap >>= 1; e;
    t = bucket_index(u->cap, hash(v, e->key)),
    f = e->next,
    e->next = u->tab[t],
    u->tab[t] = e,
    e = f); }

obj tblset(lips v, obj t, obj k, obj x) {
  i64 bucket =
    bucket_index(gettbl(t)->cap, hash(v, k));
  ent e = gettbl(t)->tab[bucket];
  while (e)
    if (eql(e->key, k))
      return e->val = x;
    else e = e->next;

  // allocate an entry
  with(t, with(k, with(x,
    e = cells(v, Size(ent)))));

  tbl y = gettbl(t);
  e->key = k;
  e->val = x;
  e->next = y->tab[bucket];

  y->tab[bucket] = e;
  y->len += 1;

  if (y->len > 2 * y->cap)
    with(x, grow(v, t, y->cap*2));

  return x; }


obj tbldel(lips v, obj t, obj k) {
  tbl y = gettbl(t);
  obj r = nil;
  i64 b = bucket_index(y->cap, hash(v, k));
  ent e = y->tab[b];
  struct ent _v = {0,0,e};
  for (ent l = &_v; l && l->next; l = l->next)
    if (l->next->key == k) {
      r = l->next->val;
      l->next = l->next->next;
      y->len--;
      break; }
  y->tab[b] = _v.next;
  if (y->len && y->cap > 2 * y->len)
    with(r, shrink(v, t));
  return r; }

obj tblget(lips v, obj t, obj k) {
  for (ent e = bucket(t, hash(v, k)); e; e = e->next)
    if (eql(e->key, k)) return e->val;
  return 0; }

obj table(lips v) {
  tbl t = cells(v, Size(tbl) + 1);
  ent *b = (ent*)(t+1);
  t->len = 0, t->cap = 1, t->tab = b, *b = NULL;
  return puttbl(t); }
