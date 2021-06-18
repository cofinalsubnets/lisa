#include "lips.h"
////
/// data constructors and utility functions
//

// functions for pairs and lists
obj pair(lips v, obj a, obj b) {
 two t;
 if (Avail < 2) Mm(a, Mm(b, reqsp(v, 2)));
 return t = bump(v, 2), t->x = a, t->y = b, puttwo(t); }

u64 llen(obj l) {
 for (u64 i = 0;; l = Y(l), i++) if (!twop(l)) return i; }

// for strings
obj string(lips v, const char* c) {
 i64 bs = 1 + slen(c);
 str o = cells(v, sizeof(struct oct)/W + b2w(bs));
 cpy8(o->text, c, o->len = bs);
 return putoct(o); }

u64 hc(lips, obj);
//symbols

// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult. the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection.
static obj ssk(lips v, obj y, obj x) {
 int i = scmp(symnom(y), chars(x));
 return i == 0 ? y :
  sskc(v, i < 0 ? &(getsym(y)->r) : &(getsym(y)->l), x); }

obj sskc(lips v, mem y, obj x) {
 if (!nilp(*y)) return ssk(v, *y, x);
 sym u = bump(v, sizeof(struct sym) / W);
 u->nom = x, u->code = hc(v, x);
 u->l = nil, u->r = nil;
 return *y = putsym(u); }

obj intern(lips v, obj x) {
 if (Avail < Size(sym))
  Mm(x, reqsp(v, Size(sym)));
 return sskc(v, &Syms, x); }

static Inline u64 hash_bytes(i64 len, char *us) {
 u64 h = 1;
 for (; len--; h *= mix, h ^= *us++);
 return mix * h; }

u64 hc(lips v, obj x) {
 u64 r;
 switch (kind(x)) {
  case Sym: r = getsym(x)->code; break;
  case Oct: r = hash_bytes(getoct(x)->len, getoct(x)->text); break;
  case Two: r = hc(v, X(x)) ^ hc(v, Y(x)); break;
  case Hom: r = hc(v, homnom(v, x)) ^ (mix * (uintptr_t) G(x)); break;
  case Tbl: r = mix; // umm lol
  default:  r = mix * x; }
 return (r<<48)|(r>>16); }

// tblrsz(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static obj tblrsz(lips v, obj t, i64 ns) {
 tble e, ch, *b, *d;
 with(t, set64((mem) (b = cells(v, ns)), 0, ns));
 tbl o = gettbl(t);
 i64 u, n = o->cap;
 d = o->tab; o->tab = b; o->cap = ns;
 while (n--) for (ch = d[n]; ch;
  e = ch,
  ch = ch->next,
  u = hbi(ns, hc(v, e->key)),
  e->next = b[u],
  b[u] = e);
 return t; }

static u0 tblshrink(lips v, obj t) {
  tble e = NULL;
  tbl u = gettbl(t);
  for (i64 i = 0; i < u->cap; i++) {
   for (tble f = u->tab[i], g; f;
    g = f->next,
    f->next = e,
    e = f,
    f = g);
   u->tab[i] = NULL; }
  u->cap >>= 1;
  for (tble f; e; e = f) {
   i64 b = hbi(u->cap, hc(v, e->key));
   f = e->next;
   e->next = u->tab[b];
   u->tab[b] = e; } }

static Inline obj tblade(lips v, obj t, obj k, obj x, i64 bkt) {
 tble e; tbl y;
 Mm(t, Mm(k, Mm(x, e = cells(v, Size(tble)))));
 y = gettbl(t);
 e->key = k, e->val = x;
 e->next = y->tab[bkt], y->tab[bkt] = e;
 ++y->len;
 return x; }

obj tblset_s(lips v, obj t, obj k, obj val) {
 i64 b = hbi(gettbl(t)->cap, hc(v, k));
 tble e = gettbl(t)->tab[b];
 for (;e; e = e->next) if (e->key == k) return e->val = val;
 return tblade(v,t,k,val,b); }

obj tblset(lips v, obj t, obj k, obj val) {
 with(t, val = tblset_s(v, t, k, val));
 if (gettbl(t)->len > 2 * gettbl(t)->cap)
  with(val, tblrsz(v, t, gettbl(t)->cap*2));
 return val; }

obj tbldel(lips v, obj t, obj k) {
 tbl y = gettbl(t);
 obj r = nil;
 i64 b = hbi(y->cap, hc(v, k));
 tble e = y->tab[b];
 struct tble _v = {0,0,e};
 for (tble l = &_v; l && l->next; l = l->next)
  if (l->next->key == k) {
   r = l->next->val;
   l->next = l->next->next;
   y->len--;
   break; }
 y->tab[b] = _v.next;
 if (y->len && y->cap > 2 * y->len)
  Mm(r, tblshrink(v, t));
 return r; }

obj tblget(lips v, obj t, obj k) {
 for (tble e = hb(t, hc(v, k)); e; e = e->next)
  if (eql(e->key, k)) return e->val;
 return 0; }

obj table(lips v) {
 tbl t = cells(v, sizeof(struct tbl)/W + 1);
 tble *b = (tble*)(t+1);
 t->len = 0, t->cap = 1, t->tab = b, *b = NULL;
 return puttbl(t); }
