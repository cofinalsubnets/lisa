#include "lips.h"
////
/// data constructors and utility functions
//

// functions for pairs and lists
obj pair(lips v, obj a, obj b) {
 two t;
 if (Avail < 2) Mm(a, Mm(b, reqsp(v, 2)));
 return t = bump(v, 2), t->x = a, t->y = b, puttwo(t); }

i64 idx(obj l, obj x) {
 for (i64 i = 0; twop(l); l = Y(l), i++)
  if (x == X(l)) return i;
 return -1; }

u64 llen(obj l) {
 for (u64 i = 0;; l = Y(l), i++)
  if (!twop(l)) return i; }

obj snoc(lips v, obj l, obj x) {
 if (!twop(l)) return pair(v, x, l);
 Mm(l, x = snoc(v, Y(l), x));
 return pair(v, X(l), x); }

obj linitp(lips v, obj x, mem d) {
 obj y;
 if (!twop(Y(x))) return *d = x, nil;
 Mm(x, y = linitp(v, Y(x), d));
 return pair(v, X(x), y); }

// for strings
obj string(lips v, const char* c) {
 i64 bs = 1;
 for (const char *d = c; *d++; bs++);
 str o = cells(v, sizeof(struct oct)/W + b2w(bs));
 cpy8(o->text, c, o->len = bs);
 return putoct(o); }

static u64 hc(lips, obj);
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

static u64 hc(lips v, obj x) {
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
static u0 tblrsz(lips v, obj t, i64 ns) {
 tble e, ch, *b, *d;
 Mm(t, rep64((mem) (b = cells(v, ns)), 0, ns));
 tbl o = gettbl(t);
 i64 u, n = o->cap;
 d = o->tab; o->tab = b; o->cap = ns;
 while (n--) for (ch = d[n]; ch;
  e = ch,
  ch = ch->next,
  u = hbi(ns, hc(v, e->key)),
  e->next = b[u],
  b[u] = e); }

static u0 tblade(lips v, obj t, obj k, obj x, i64 b) {
 tble e; tbl y;
 Mm(t, Mm(k, Mm(x, e = cells(v, Size(tble)))));
 y = gettbl(t);
 e->key = k, e->val = x;
 e->next = y->tab[b], y->tab[b] = e;
 ++y->len; }

obj tblset(lips v, obj t, obj k, obj val) {
 i64 b = hbi(gettbl(t)->cap, hc(v, k));
 tble e = gettbl(t)->tab[b];
 for (;e; e = e->next) if (e->key == k) return e->val = val;
 Mm(t, Mm(val,
  tblade(v,t,k,val,b),
  gettbl(t)->len / gettbl(t)->cap > 2 ?
   tblrsz(v, t, gettbl(t)->cap*2) : 0));
 return val; }

static obj tblkeys_j(lips v, tble e, obj l) {
 obj x;
 if (!e) return l;
 x = e->key;
 Mm(x, l = tblkeys_j(v, e->next, l));
 return pair(v, x, l); }

static obj tblkeys_i(lips v, obj t, i64 i) {
 obj k;
 if (i == gettbl(t)->cap) return nil;
 Mm(t, k = tblkeys_i(v, t, i+1));
 return tblkeys_j(v, gettbl(t)->tab[i], k); }

obj tblkeys(lips v, obj t) {
 return tblkeys_i(v, t, 0); }

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
 if (y->len && y->cap / y->len > 2)
  Mm(r, Mm(t, tblrsz(v, t, y->cap / 2)));
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
