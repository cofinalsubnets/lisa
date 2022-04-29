#include "lips.h"
#include "vm.h"
#include <stdlib.h>
#include <time.h>
#include <string.h>

typedef ob copier(en, ob, u64, ob*);
static Inline copier cp;
#define Gc(n) ob n(en v, ob x, u64 len0, ob*pool0)

#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((ob*)(o),v->pool,v->pool+v->len)
#define COPY(dst,src) (dst=cp(v,src,len0,pool0))
#define CP(x) COPY(x,x)

// unchecked allocator -- make sure there's enough memory!
static u0* bump(en v, u64 n) {
  u0* x = v->hp;
  return v->hp += n, x; }

// general purpose memory allocator
u0* cells(en v, u64 n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

#define oom_err_msg "out of memory : %d + %d"
// Run a GC cycle from inside the VM
Vm(gc) {
  u64 req = v->xp;
  Pack();
  if (!please(v, req)) return err(v, oom_err_msg, v->len, req); 
  Unpack();
  Next(0); }

// a simple copying garbage collector
//
// the first step in copying is to allocate
// a new pool of the given length, which must
// be at least enough to support the actual
// amount of reachable memory. if this fails
// then return 0. otherwise swap the pools,
// reset internal symbols, copy the stack,
// global variables, and any user saved
// locations, and free the old pool. then
// return u:
//
//     non-gc running time     t1    t2
// ,.........................,/      |
// -----------------------------------
// |                          `------'
// t0                  gc time (this cycle)
//
//       u = (t2 - t0) / (t2 - t1)
//
// t values come from clock(). if t0 < t1 < t2 then
// u will be >= 1. however, sometimes t1 == t2. in that case
// u = 1.
static clock_t copy(en v, u64 len1) {
  ob* pool1;
  clock_t t0, t1 = clock(), t2;
  bind(pool1, malloc(w2b(len1)));

  u64 len0 = v->len;
  ob *pool0 = v->pool,
     *sp0 = v->sp,
     *top0 = pool0 + len0;
  i64 shift = pool1 + len1 - top0;

  v->sp = sp0 + shift;
  v->fp = v->fp + shift;
  v->len = len1;
  v->pool = v->hp = pool1;
  v->syms = nil;

  CP(v->ip), CP(v->xp);
  for (int i = 0; i < NGlobs; i++) CP(v->glob[i]);
  for (ob *sp1 = v->sp; sp0 < top0; COPY(*sp1++, *sp0++));
  for (mm r = v->mm; r; r = r->et) CP(*(r->it));

  free(pool0);
  t0 = v->t0;
  v->t0 = t2 = clock();
  t1 = t2 - t1;
  return t1 ? (t2 - t0) / t1 : 1; }

// please : li x i64 -> u1
//
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// manages the size of the memory pool. here is the procedure
// in a nutshell:
//
// - copy into a new pool of the same size. if this fails,
//   the request fails (oom).
// - if there's enough space and the garbage collector
//   is running fast enough, return success.
// - otherwise adjust the size and copy again. if this fails,
//   we can still succeed if the first copy left us with
//   enough free space (ie. we tried to grow for performance
//   reasons). but otherwise the request fails (oom).
//
// at a constant rate of allocation, doubling the size of the
// heap halves the amount of time spent in garbage collection.
// the memory manager uses this relation to automatically trade
// space for time to keep the time spent in garbage collection
// under a certain proportion of total running time: amortized
// time in garbage collection should be less than about 6%, at
// the cost of more memory use under pressure.
#define growp (allocd > len || vit < 32) // lower bound
#define shrinkp (allocd < (len>>1) && vit >= 128) // upper bound
u1 please(en v, u64 req) {
  i64 len = v->len, vit;
  bind(vit, copy(v, len));
  i64 allocd = len - (Avail - req);
  if (growp) do len <<= 1, vit <<= 1; while (growp);
  else if (shrinkp) do len >>= 1, vit >>= 1; while (shrinkp);
  else return true; // no size change needed
  return copy(v, len) || allocd <= v->len; }


static copier cphom, cpvec, cptwo, cpsym, cpstr, cptbl;
static Gc(cpid) { return x; }
// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
static copier *copiers[] = {
  [Hom] = cphom, [Num] = cpid, [Two] = cptwo, [Vec] = cpvec,
  [Str] = cpstr, [Tbl] = cptbl, [Sym] = cpsym, [Nil] = cpid, };

static Inline Gc(cp) { return copiers[Q(x)](v, x, len0, pool0); }

#define stale(o) inb((ob*)(o),pool0,pool0+len0)
Gc(cphom) {
  yo src = H(x);
  if (fresh(src->ll)) return (ob) src->ll;
  yo end = button(src), start = (yo) end[1].ll,
     dst = bump(v, end - start + 2), j = dst;
  for (yo k = start; k < end;)
    j->ll = k->ll,
    k++->ll = (vm*) (ob) (j++);
  j[0].ll = NULL;
  j[1].ll = (vm*) dst;
  for (ob u; j-- > dst;
    u = (ob) j->ll,
    j->ll = (vm*) (!stale(u) ? u : cp(v, u, len0, pool0)));
  return (ob) (dst += src - start); }

Gc(cpstr) {
  str dst, src = S(x);
  return src->len == 0 ? *(ob*)src->text :
    (dst = bump(v, Width(str) + b2w(src->len)),
     cpy64(dst->text, src->text, b2w(src->len)),
     dst->len = src->len, src->len = 0,
     *(ob*) src->text = _S(dst)); }

Gc(cpsym) {
  sym src = getsym(x), dst;
  if (fresh(src->nom)) return src->nom;
  if (src->nom == nil) // anonymous symbol
    cpy64(dst = bump(v, Width(sym)), src, Width(sym));
  else dst = getsym(sskc(v, &v->syms, cp(v, src->nom, len0, pool0)));
  return src->nom = putsym(dst); }

static ent cpent(en v, ent src, i64 len0, ob *pool0) {
  bind(src, src);
  ent dst = (ent) bump(v, Width(ent));
  dst->next = cpent(v, src->next, len0, pool0);
  COPY(dst->key, src->key);
  COPY(dst->val, src->val);
  return dst; }

Gc(cptbl) {
  tbl src = gettbl(x);
  if (fresh(src->tab)) return (ob) src->tab;
  i64 src_cap = src->cap;
  tbl dst = bump(v, Width(tbl) + (1<<src_cap));
  dst->len = src->len;
  dst->cap = src_cap;
  dst->tab = (ent*) (dst + 1);
  ent *src_tab = src->tab;
  src->tab = (ent*) puttbl(dst);
  for (u64 ii = 1<<src_cap; ii--;)
    dst->tab[ii] = cpent(v, src_tab[ii], len0, pool0);
  return puttbl(dst); }

Gc(cptwo) {
  ob dst, src = x;
  if (fresh(A(x))) return A(x);
  dst = puttwo(bump(v, Width(two)));
  A(dst) = A(src), A(src) = dst;
  B(dst) = cp(v, B(src), len0, pool0);
  A(dst) = cp(v, A(dst), len0, pool0);
  return dst; }

Gc(cpvec) {
  vec dst, src = V(x);
  if (fresh(*src->xs)) return *src->xs;
  dst = bump(v, Width(vec) + src->len);
  i64 i, l = dst->len = src->len;
  dst->xs[0] = src->xs[0];
  src->xs[0] = putvec(dst);
  for (CP(dst->xs[0]), i = 1; i < l; ++i)
    COPY(dst->xs[i], src->xs[i]);
  return _V(dst); }

// functions for pairs and lists
ob pair(en v, ob a, ob b) {
  two w;
  with(a, with(b, w = cells(v, 2)));
  bind(w, w);
  w->a = a, w->b = b;
  return puttwo(w); }

SI i64 tbl_idx(u64 cap, u64 co) {
  return co & ((1 << cap) - 1); }

SI u64 tbl_load(ob t) {
  return T(t)->len >> T(t)->cap; }


static ent tbl_ent(en v, ob u, ob k) {
  tbl t = gettbl(u);
  ent e = t->tab[tbl_idx(t->cap, hash(v, k))];
  for (; e; e = e->next) if (eql(e->key, k)) return e;
  return NULL; }

typedef u64 hasher(en, ob);
static hasher hash_sym, hash_str, hash_two, hash_hom, hash_num, hash_vec, hash_nil;
static hasher *hashers[] = {
  [Nil] = hash_nil, [Hom] = hash_hom, [Two] = hash_two, [Vec] = hash_vec,
  [Str] = hash_str, [Num] = hash_num, [Sym] = hash_sym, [Tbl] = hash_nil };

Inline u64 hash(en v, ob x) { return hashers[Q(x)](v, x); }

SI u64 ror64(u64 x, u64 n) { return (x<<(64-n))|(x>>n); }
static u64 hash_sym(en v, ob y) { return getsym(y)->code; }
static u64 hash_two(en v, ob w) { return ror64(hash(v, A(w)) * hash(v, B(w)), 32); }
static u64 hash_hom(en v, ob h) { return hash(v, homnom(v, h)) ^ mix; }
static u64 hash_num(en v, ob n) { return ror64(mix * n, 16); }
static u64 hash_vec(en v, ob x) { return ror64(mix * V(x)->len, 32); }
static u64 hash_nil(en v, ob _) { return ror64(mix * Q(nil), 48); }
static u64 hash_str(en v, ob x) {
  str s = S(x);
  u64 len = s->len;
  char *us = s->text;
  for (u64 h = 1;; h ^= *us++, h *= mix)
    if (!len--) return h; }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static u0 tbl_fit(en v, ob t) {
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

static ob tbl_del(en v, ob t, ob key) {
  tbl y = gettbl(t);
  ob val = nil;
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
static ob tbl_grow(en v, ob t) {
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

ob tbl_set_s(en v, ob t, ob k, ob x) {
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

ob tbl_set(en v, ob t, ob k, ob x) {
  with(t, x = tbl_set_s(v, t, k, x));
  bind(x, x);
  if (tbl_load(t) > 1) with(x, t = tbl_grow(v, t));
  bind(t, t);
  return x; }

ob tbl_get(en v, ob t, ob k) {
  ent e = tbl_ent(v, t, k);
  return e ? e->val : 0; }

ob table(en v) {
  tbl t;
  bind(t, cells(v, Width(tbl) + 1));
  ent *b = (ent*)(t+1);
  t->len = t->cap = 0, t->tab = b, *b = NULL;
  return puttbl(t); }

static ob tblkeys_j(en v, ent e, ob l) {
  if (!e) return l;
  ob x = e->key;
  with(x, l = tblkeys_j(v, e->next, l));
  bind(l, l);
  return pair(v, x, l); }

static ob tblkeys_i(en v, ob t, i64 i) {
  ob k;
  if (i == 1 << gettbl(t)->cap) return nil;
  with(t, k = tblkeys_i(v, t, i+1));
  bind(k, k);
  return tblkeys_j(v, gettbl(t)->tab[i], k); }

Inline ob tblkeys(en v, ob t) { return tblkeys_i(v, t, 0); }

ob string(en v, const char* c) {
  i64 bs = 1 + strlen(c);
  str o;
  bind(o, cells(v, Width(str) + b2w(bs)));
  cpy8(o->text, c, o->len = bs);
  return _S(o); }

Vm(tbld) {
  Ary(2);
  Tc(Argv[0], Tbl);
  CallC(v->xp = tbl_del(v, Argv[0], Argv[1]));
  Jump(ret); }


// string instructions
Vm(strl) {
  Arity(1);
  CheckType(*Argv, Str);
  xp = _N(S(*Argv)->len-1);
  Jump(ret); }

Vm(strg) {
  Arity(2);
  CheckType(Argv[0], Str);
  CheckType(Argv[1], Num);
  xp = N(Argv[1]) < S(Argv[0])->len-1 ?
       _N(S(Argv[0])->text[N(Argv[1])]) :
       nil;
  Jump(ret); }

Vm(strconc) {
  i64 l = N(Argc), sum = 0, i = 0;
  while (i < l) {
    ob x = Argv[i++];
    CheckType(x, Str);
    sum += S(x)->len - 1; }
  i64 words = b2w(sum+1) + 1;
  Have(words);
  str d = (str) hp;
  hp += words;
  d->len = sum + 1;
  d->text[sum] = 0;
  while (i) {
    str x = S(Argv[--i]);
    sum -= x->len - 1;
    cpy8(d->text+sum, x->text, x->len - 1); }
  Go(ret, _S(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(strs) {
  Arity(3);
  CheckType(Argv[0], Str);
  CheckType(Argv[1], Num);
  CheckType(Argv[2], Num);
  str src = S(Argv[0]);
  i64 lb = N(Argv[1]), ub = N(Argv[2]);
  lb = max(lb, 0);
  ub = min(ub, src->len-1);
  ub = max(ub, lb);
  i64 words = 1 + b2w(ub - lb + 1);
  Have(words);
  str dst = (str) hp;
  hp += words;
  dst->len = ub - lb + 1;
  dst->text[ub - lb] = 0;
  cpy8(dst->text, src->text + lb, ub - lb);
  Go(ret, _S(dst)); }

Vm(strmk) {
  i64 i = 0, bytes = N(Argc)+1, words = 1 + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (ob x; i < bytes-1; s->text[i++] = N(x)) {
    x = Argv[i];
    Tc(x, Num);
    if (x == _N(0)) break; }
  s->text[i] = 0;
  s->len = i+1;
  Go(ret, _S(s)); }

//symbols

// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult. the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection.
ob interns(en v, const char *s) {
  ob _;
  bind(_, string(v, s));
  return intern(v, _); }


// FIXME this is too bad
ob sskc(en v, ob*y, ob x) {
  sym z;
  if (!nilp(*y)) {
    z = getsym(*y);
    int i = strcmp(S(z->nom)->text, S(x)->text);
    return i == 0 ? *y : sskc(v, i < 0 ? &z->r : &z->l, x); }
  // the caller must ensure Avail >= Width(sym) because to GC
  // here would cause the tree to be rebuilt
  z = cells(v, Width(sym));
  z->code = hash(v, z->nom = x) ^ mix;
  z->l = z->r = nil;
  return *y = putsym(z); }

ob intern(en v, ob x) {
  if (Avail < Width(sym)) {
    u1 o;
    with(x, o = please(v, Width(sym)));
    bind(o, o); }
  return sskc(v, &v->syms, x); }

Vm(gsym_u) {
  if (Argc > _N(0) && strp(*Argv)) {
    CallC(v->xp = intern(v, *Argv));
    Jump(ret); }
  Have(Width(sym));
  sym y = (sym) hp;
  hp += Width(sym);
  y->nom = y->l = y->r = nil;
  y->code = v->rand = lcprng(v->rand);
  Go(ret, putsym(y)); }

Vm(ystr_u) {
  Arity(1);
  xp = *Argv;
  CheckType(xp, Sym);
  Go(ret, getsym(xp)->nom); }
