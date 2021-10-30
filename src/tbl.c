#include "lips.h"
#include "tbl.h"
#include "eql.h"
#include "two.h"
#include "hom.h"
#include "mem.h"

static Inline u64 hash_bytes(u64 len, char *us) {
 for (u64 h = mix;; h ^= *us++, h *= mix)
  if (!len--) return h; }

static Inline i64 hbi(u64 cap, u64 co) { return co % cap; }

static Inline ent hb(obj t, u64 code) {
 return gettbl(t)->tab[hbi(gettbl(t)->cap, code)]; }

u64 hash(lips v, obj x) {
 u64 r;
 switch (kind(x)) {
  case Sym: r = getsym(x)->code; break;
  case Str: r = hash_bytes(getstr(x)->len, getstr(x)->text); break;
  case Two: r = hash(v, X(x)) ^ hash(v, Y(x)); break;
  case Hom: r = hash(v, homnom(v, x)) ^ (mix * (u64) G(x)); break;
  case Vec: // mutable data aren't really hashable ...
  case Tbl: r = mix; // umm lol, linear search
  default:  r = mix * x; }
 return rotr64(r, 16); }

// rehash(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static obj rehash(lips v, obj t, i64 ns) {
 ent e, ch, *b, *d;
 with(t, set64((mem) (b = cells(v, ns)), 0, ns));
 tbl o = gettbl(t);
 i64 u, n = o->cap;
 d = o->tab; o->tab = b; o->cap = ns;
 while (n--) for (ch = d[n]; ch;
  e = ch,
  ch = ch->next,
  u = hbi(ns, hash(v, e->key)),
  e->next = b[u],
  b[u] = e);
 return t; }

// it's possible to shrink a table without allocating any
// new memory.
static u0 tblshrink(lips v, obj t) {
 ent e = NULL, f, g;
 tbl u = gettbl(t);
 for (t = 0; t < u->cap; t++)
  for (f = u->tab[t], u->tab[t] = NULL; f;
   g = f->next, f->next = e, e = f, f = g);
 for (u->cap >>= 1; e;
  t = hbi(u->cap, hash(v, e->key)),
  f = e->next,
  e->next = u->tab[t],
  u->tab[t] = e,
  e = f); }

static obj tblade(lips v, obj t, obj k, obj x, i64 bkt) {
 ent e; tbl y;
 with(t, with(k, with(x, e = cells(v, Size(ent)))));
 y = gettbl(t);
 e->key = k, e->val = x;
 e->next = y->tab[bkt], y->tab[bkt] = e;
 ++y->len;
 return x; }

obj tblset_s(lips v, obj t, obj k, obj x) {
 i64 b = hbi(gettbl(t)->cap, hash(v, k));
 for (ent e = gettbl(t)->tab[b]; e; e = e->next)
  if (e->key == k) return e->val = x;
 return tblade(v,t,k,x,b); }

obj tblset(lips v, obj t, obj k, obj val) {
 with(t, val = tblset_s(v, t, k, val));
 if (gettbl(t)->len > 2 * gettbl(t)->cap)
  with(val, rehash(v, t, gettbl(t)->cap*2));
 return val; }

obj tbldel(lips v, obj t, obj k) {
 tbl y = gettbl(t);
 obj r = nil;
 i64 b = hbi(y->cap, hash(v, k));
 ent e = y->tab[b];
 struct ent _v = {0,0,e};
 for (ent l = &_v; l && l->next; l = l->next)
  if (l->next->key == k) {
   r = l->next->val;
   l->next = l->next->next;
   y->len--;
   break; }
 y->tab[b] = _v.next;
 if (y->len && y->cap > 2 * y->len) with(r, tblshrink(v, t));
 return r; }

obj tblget(lips v, obj t, obj k) {
 for (ent e = hb(t, hash(v, k)); e; e = e->next)
  if (eql(e->key, k)) return e->val;
 return 0; }

obj table(lips v) {
 tbl t = cells(v, Size(tbl) + 1);
 ent *b = (ent*)(t+1);
 t->len = 0, t->cap = 1, t->tab = b, *b = NULL;
 return puttbl(t); }
