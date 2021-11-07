#include "lips.h"
#include "tbl.h"
#include "eql.h"
#include "two.h"
#include "hom.h"
#include "mem.h"
#include "terp.h"

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

  // collect all entries
  for (t = 0; t < u->cap; t++)
    for (f = u->tab[t], u->tab[t] = NULL; f;
      g = f->next,
      f->next = e,
      e = f, f = g);

  // shrink bucket array
  while (u->len >= 2 * u->cap) u->cap >>= 1;

  // reinsert
  while (e)
    t = bucket_index(u->cap, hash(v, e->key)),
    f = e->next,
    e->next = u->tab[t],
    u->tab[t] = e,
    e = f; }

ent tbl_entry(lips v, obj k, obj x) {
  ent e;
  with(k, with(x, e = cells(v, Size(ent))));
  return e; }

obj tblset_s(lips v, obj t, obj k, obj x) {
  i64 bucket =
    bucket_index(gettbl(t)->cap, hash(v, k));
  ent e = gettbl(t)->tab[bucket];
  while (e)
    if (eql(e->key, k))
      return e->val = x;
    else e = e->next;

  // allocate an entry
  with(t, e = tbl_entry(v, k, x));

  tbl y = gettbl(t);
  e->key = k;
  e->val = x;
  e->next = y->tab[bucket];

  y->tab[bucket] = e;
  y->len += 1;

  return x; }

static u0 maybe_grow(lips v, obj t) {
  tbl y = gettbl(t);
  if (y->len > 2 * y->cap) grow(v, t, y->cap*2); }

u0 maybe_shrink(lips v, obj t) {
  tbl y = gettbl(t);
  if (y->len && y->cap > 2 * y->len) shrink(v, t); }

obj tblset(lips v, obj t, obj k, obj x) {
  with(t, x = tblset_s(v, t, k, x));
  with(x, maybe_grow(v, t));
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
  with(r, maybe_shrink(v, t));
  return r; }

ent tblget_entry(lips v, obj t, obj k) {
  for (ent e = bucket(t, hash(v, k)); e; e = e->next)
    if (eql(e->key, k)) return e;
  return 0; }

obj tblget(lips v, obj t, obj k) {
  ent e = tblget_entry(v, t, k);
  return e ? e->val : 0; }

obj table(lips v) {
  tbl t = cells(v, Size(tbl) + 1);
  ent *b = (ent*)(t+1);
  t->len = 0, t->cap = 1, t->tab = b, *b = NULL;
  return puttbl(t); }

// hash tables
VM(tblg) {
 ARY(2);
 TC(ARGV[0], Tbl);
 xp = tblget(v, ARGV[0], ARGV[1]);
 GO(ret, xp ? xp : nil); }

OP1(tget, (xp = tblget(v, xp, *sp++)) ? xp : nil)

static obj tblkeys_j(lips v, ent e, obj l) {
 obj x;
 return !e ? l :
  (x = e->key,
   with(x, l = tblkeys_j(v, e->next, l)),
   pair(v, x, l)); }

static obj tblkeys_i(lips v, obj t, i64 i) {
 obj k;
 return i == gettbl(t)->cap ? nil :
  (with(t, k = tblkeys_i(v, t, i+1)),
   tblkeys_j(v, gettbl(t)->tab[i], k)); }

static Inline obj tblkeys(lips v, obj t) {
 return tblkeys_i(v, t, 0); }

OP1(thas, tblget(v, xp, *sp++) ? ok : nil)
OP1(tlen, N_(gettbl(xp)->len))
VM(tkeys) { CALLC(v->xp = tblkeys(v, xp)); NEXT(1); }

VM(tblc) {
 ARY(2);
 TC(ARGV[0], Tbl);
 xp = tblget(v, ARGV[0], ARGV[1]);
 GO(ret, xp ? ok : nil); }

static obj tblss(lips v, i64 i, i64 l) {
 mem fp = Fp;
 return i > l-2 ? ARGV[i-1] :
  (tblset(v, v->xp, ARGV[i], ARGV[i+1]),
   tblss(v, i+2, l)); }

VM(tbls) {
 ARY(1);
 xp = *ARGV;
 TC(xp, Tbl);
 RETC(v->xp = tblss(v, 1, N(ARGC))); }

VM(tblmk) { RETC(v->xp = table(v), tblss(v, 0, N(ARGC))); }

VM(tbld) {
 ARY(2); TC(ARGV[0], Tbl);
 RETC(v->xp = tbldel(v, ARGV[0], ARGV[1])); }

VM(tblks) {
 ARY(1); TC(*ARGV, Tbl);
 RETC(v->xp = tblkeys(v, *ARGV)); }

VM(tbll) {
 ARY(1); TC(*ARGV, Tbl);
 GO(ret, N_(gettbl(*ARGV)->len)); }
