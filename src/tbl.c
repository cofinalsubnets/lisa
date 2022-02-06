#include "lips.h"
#include "tbl.h"

static Inline i64 tbl_idx(u64 cap, u64 co) {
  return co & ((1 << cap) - 1); }

static Inline u64 tbl_load(obj t) {
  return T(t)->len / (1<<T(t)->cap); }

#include "cmp.h"

static ent tbl_ent(lips v, obj u, obj k) {
  tbl t = gettbl(u);
  ent e = t->tab[tbl_idx(t->cap, hash(v, k))];
  for (; e; e = e->next) if (eql(e->key, k)) return e;
  return NULL; }

#include "two.h"
#include "str.h"
#include "hom.h"
#include "vec.h"
#include "sym.h"

static u64 hash_str(str s) {
  u64 len = s->len;
  char *us = s->text;
  for (u64 h = 1;; h ^= *us++, h *= mix)
    if (!len--) return h; }

u64 hash(lips v, obj x) {
  switch (kind(x)) {
    case Sym: return getsym(x)->code;
    case Str: return hash_str(S(x));
    case Two: return hash(v, A(x)) ^ hash(v, B(x));
    case Hom: return hash(v, homnom(v, x)) ^ mix;
    case Num: return rotr64(mix * x, 16);
    case Vec: return rotr64(mix * V(x)->len, 32);
    default:  return rotr64(mix * kind(x), 48); } }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static u0 shrink(lips v, obj t) {
  ent e = NULL, f, g;
  tbl u = T(t);

  // collect all entries
  for (u64 i = 1<<u->cap; i--;)
    for (f = u->tab[i], u->tab[i] = NULL; f;)
      g = f->next,
      f->next = e,
      e = f,
      f = g;

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
  if (tbl_load(t) < 1) shrink(v, t);
  return val; }

#include "mem.h"

// grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static obj grow(lips v, obj t) {
  ent *tab0, *tab1;
  u64 cap0 = T(t)->cap, cap1 = cap0 + 1;
  with(t, tab1 = cells(v, 1<<cap1));
  set64(tab1, 0, 1<<cap1);
  tab0 = T(t)->tab;

  for (u64 cap = 1<<cap0;cap--;)
    for (ent e, es = tab0[cap]; es;) {
      e = es, es = es->next;
      u64 i = tbl_idx(cap1, hash(v, e->key));
      e->next = tab1[i], tab1[i] = e; }

  T(t)->cap = cap1;
  T(t)->tab = tab1;
  return t; }

obj tbl_set_s(lips v, obj t, obj k, obj x) {
  u64 i = tbl_idx(gettbl(t)->cap, hash(v, k));
  ent e = tbl_ent(v, t, k);
  if (e) return e->val = x;

  // it's not here, so allocate an entry
  tbl y;
  with(t, with(k, with(x,
    e = cells(v, Width(ent)),
    e->key = k, e->val = x,
    y = gettbl(t),
    e->next = y->tab[i],
    y->tab[i] = e,
    y->len += 1)));

  return x; }

obj tbl_set(lips v, obj t, obj k, obj x) {
  with(t, x = tbl_set_s(v, t, k, x));
  if (tbl_load(t) > 1) with(x, grow(v, t));
  return x; }

obj tbl_get(lips v, obj t, obj k) {
  ent e = tbl_ent(v, t, k);
  return e ? e->val : 0; }

obj table(lips v) {
  tbl t = cells(v, Width(tbl) + 1);
  ent *b = (ent*)(t+1);
  t->len = t->cap = 0, t->tab = b, *b = NULL;
  return puttbl(t); }

static obj tblkeys_j(lips v, ent e, obj l) {
 obj x;
 return !e ? l :
  (x = e->key,
   with(x, l = tblkeys_j(v, e->next, l)),
   pair(v, x, l)); }

static obj tblkeys_i(lips v, obj t, i64 i) {
 obj k;
 return i == 1<<gettbl(t)->cap ? nil :
  (with(t, k = tblkeys_i(v, t, i+1)),
   tblkeys_j(v, gettbl(t)->tab[i], k)); }

Inline obj tblkeys(lips v, obj t) {
 return tblkeys_i(v, t, 0); }

static ent cpent(lips v, ent src, i64 len0, mem base0) {
 if (!src) return NULL;
 ent dst = (ent) bump(v, Width(ent));
 dst->next = cpent(v, src->next, len0, base0);
 COPY(dst->key, src->key);
 COPY(dst->val, src->val);
 return dst; }

GC(cptbl) {
  tbl src = gettbl(x);
  if (fresh(src->tab)) return (obj) src->tab;
  i64 src_cap = src->cap;
  tbl dst = bump(v, Width(tbl) + (1<<src_cap));
  dst->len = src->len;
  dst->cap = src_cap;
  dst->tab = (ent*) (dst + 1);
  ent *src_tab = src->tab;
  src->tab = (ent*) puttbl(dst);
  for (u64 ii = 1<<src_cap; ii--;)
    dst->tab[ii] = cpent(v, src_tab[ii], len0, base0);
  return puttbl(dst); }

u0 emtbl(lips v, FILE *o, obj x) {
  tbl t = gettbl(x);
  fprintf(o, "#tbl:%ld/%ld", (long)t->len, (long)t->cap); }


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
  Next(1); }

Vm(tblc) {
 Ary(2);
 Tc(Argv[0], Tbl);
 xp = tbl_get(v, Argv[0], Argv[1]);
 Go(ret, xp ? ok : nil); }

static obj tblss(lips v, i64 i, i64 l) {
 mem fp = v->fp;
 return i > l-2 ? Argv[i-1] :
  (tbl_set(v, v->xp, Argv[i], Argv[i+1]),
   tblss(v, i+2, l)); }

Vm(tbls) {
 Ary(1);
 xp = *Argv;
 Tc(xp, Tbl);
 CallC(v->xp = tblss(v, 1, N(Argc)));
 Jump(ret); }



Vm(tblmk) {
  CallC(v->xp = table(v), tblss(v, 0, N(Argc)));
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
 Jump(ret); }

Vm(tbll) {
 Ary(1);
 Tc(*Argv, Tbl);
 Go(ret, _N(gettbl(*Argv)->len)); }

Vm(tset) {
 obj x = *sp++, y = *sp++;
 CallC(v->xp = tbl_set(v, xp, x, y));
 Next(1); }
