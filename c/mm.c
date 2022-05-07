#include "em.h"
#include <stdlib.h>
#include <time.h>
#include <string.h>

static copier
  cphom, cptwo, cpsym, cpstr, cptbl, cpid,
  *copiers[] = {
    [Hom] = cphom, [Num] = cpid, [Two] = cptwo, [Xxx] = cpid,
    [Str] = cpstr, [Tbl] = cptbl, [Sym] = cpsym, [Nil] = cpid, };
static Inline Gc(cp) { return copiers[Q(x)](v, x, len0, pool0); }
static Gc(cpid) { return x; }

static ob sskc(em, ob*, ob);

// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.


#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((ob*)(o),v->pool,v->pool+v->len)
#define COPY(dst,src) (dst=cp(v,src,len0,pool0))
#define CP(x) COPY(x,x)

// unchecked allocator -- make sure there's enough memory!
static Inline void* bump(em v, intptr_t n) {
  void* x = v->hp; return v->hp += n, x; }
static void *cells_(em v, uintptr_t n) {
  return please(v, n) ? bump(v, n) : 0; }
void* cells(em v, uintptr_t n) {
  return Avail >= n ? bump(v, n) : cells_(v, n); }

#define oom_err_msg "out of memory : %d + %d"
// Run a GC cycle from inside the VM
Vm(gc) {
  uintptr_t req = v->xp;
  return Pack(), please(v, req) ?
    (Unpack(), ApY(ip, xp)) :
    err(v, oom_err_msg, v->len, req); }

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
static clock_t copy(em v, intptr_t len1) {
  clock_t t0, t1 = clock(), t2;
  ob len0 = v->len,
     *sp0 = v->sp,
     *pool0 = v->pool,
     *pool1 = malloc(len1 * sizeof(ob)),
     *top0 = pool0 + len0,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  if (!pool1) return 0;

  v->syms = nil;
  v->len = len1;
  v->pool = v->hp = pool1;
  v->sp = sp0 + shift;
  v->fp = (fr) ((ob*) v->fp + shift);
  CP(v->xp);
  v->ip = (yo) cp(v, (ob) v->ip, len0, pool0);
  for (int i = 0; i < NGlobs; CP(v->glob[i]), i++);
  for (ob *sp1 = v->sp; sp0 < top0; COPY(*sp1++, *sp0++));
  for (mm r = v->mm; r; CP(*r->it), r = r->et);

  return
    free(pool0),
    t0 = v->t0,
    v->t0 = t2 = clock(),
    t1 = t2 - t1,
    t1 ? (t2 - t0) / t1 : 1; }

// please : u1 em intptr_t
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

static ob gro(ob allocd, ob len, ob vit) {
  return len <<= 1, vit <<= 1, !growp ? len :
    gro(allocd, len, vit); }

static ob shr(ob allocd, ob len, ob vit) {
  return len >>= 1, vit >>= 1, !shrinkp ? len :
    shr(allocd, len, vit); }

bool please(em v, uintptr_t req) {
  intptr_t len = v->len,
           vit = copy(v, len),
           allocd = len - (Avail - req);
  return len = v->len, !vit ? 0 : !growp && !shrinkp ? 1 :
    copy(v, (growp ? gro : shr)(allocd, len, vit)) ||
    allocd <= v->len; }


#define stale(o) inb((ob*)(o),pool0,pool0+len0)
Gc(cphom) {
  if (fresh(G(x))) return (ob) G(x);
  yo src = (yo) x,
     end = button(src),
     start = (yo) end[1].ll,
     dst = bump(v, end - start + 2),
     j = dst;

  for (yo k = start; k < end;)
    j->ll = k->ll,
    k++->ll = (ll*) j++;
  j[0].ll = NULL, j[1].ll = (ll*) dst;

  for (ob u; j-- > dst;)
    u = (ob) j->ll,
    u = !stale(u) ? u : cp(v, u, len0, pool0),
    j->ll = (ll*) u;

  return (ob) (src - start + dst); }

Gc(cpstr) {
  str dst, src = getstr(x);
  return src->len == 0 ? *(ob*)src->text :
    (dst = bump(v, Width(str) + b2w(src->len)),
     cpyw(dst->text, src->text, b2w(src->len)),
     dst->len = src->len, src->len = 0,
     *(ob*) src->text = putstr(dst)); }

Gc(cpsym) {
  sym src = getsym(x), dst;
  return fresh(src->nom) ? src->nom :
    src->nom == nil ? // anonymous symbol
      (dst = bump(v, Width(sym)),
       cpyw(dst, src, Width(sym)),
       src->nom = putsym(dst)) :
      (x = cp(v, src->nom, len0, pool0),
       dst = getsym(sskc(v, &v->syms, x)),
       src->nom = putsym(dst) ); }

static ent cpent(em v, ent src, intptr_t len0, ob *pool0) {
  ent dst; return !src ? 0 :
    (dst = (ent) bump(v, Width(ent)),
     dst->next = cpent(v, src->next, len0, pool0),
     COPY(dst->key, src->key),
     COPY(dst->val, src->val),
     dst); }

Gc(cptbl) {
  tbl src = gettbl(x);
  if (fresh(src->tab)) return (ob) src->tab;
  intptr_t src_cap = src->cap;
  tbl dst = bump(v, Width(tbl) + (1<<src_cap));
  dst->len = src->len;
  dst->cap = src_cap;
  dst->tab = (ent*) (dst + 1);
  ent *src_tab = src->tab;
  src->tab = (ent*) puttbl(dst);
  for (uintptr_t ii = 1<<src_cap; ii--;)
    dst->tab[ii] = cpent(v, src_tab[ii], len0, pool0);
  return puttbl(dst); }

Gc(cptwo) {
  ob dst, src = x;
  return fresh(A(x)) ? A(x) :
    (dst = puttwo(bump(v, Width(two))),
     A(dst) = A(src), A(src) = dst,
     B(dst) = cp(v, B(src), len0, pool0),
     A(dst) = cp(v, A(dst), len0, pool0),
     dst); }

static ob pair_(em v, ob a, ob b) {
  bool _; two w;
  return with(a, with(b, _ = please(v, 2))), !_ ? 0 :
    (w = bump(v, 2), w->a = a, w->b = b, puttwo(w)); }

// functions for pairs and lists
ob pair(em v, ob a, ob b) {
  two w; return Avail < 2 ? pair_(v, a, b) :
    (w = bump(v, 2), w->a = a, w->b = b, puttwo(w)); }

static Inline intptr_t tbl_idx(uintptr_t cap, uintptr_t co) {
  return co & ((1 << cap) - 1); }

static Inline uintptr_t tbl_load(ob t) {
  return gettbl(t)->len >> gettbl(t)->cap; }

static ent tbl_ent_(em v, ent e, ob k) { return
  !e ? e : eql(e->key, k) ? e : tbl_ent_(v, e->next, k); }

static ent tbl_ent(em v, ob u, ob k) {
  tbl t = gettbl(u); return
    tbl_ent_(v, t->tab[tbl_idx(t->cap, hash(v, k))], k); }

static hasher
  hash_sym, hash_str, hash_two, hash_hom, hash_num, hash_nil,
  *hashers[] = {
    [Hom] = hash_hom, [Two] = hash_two,
    [Str] = hash_str, [Num] = hash_num, [Sym] = hash_sym,
    [Nil] = hash_nil, [Xxx] = hash_nil, [Tbl] = hash_nil, };

Inline uintptr_t hash(em v, ob x) { return hashers[Q(x)](v, x); }

static const uintptr_t mix = 2708237354241864315;
static Inline uintptr_t ror64(uintptr_t x, uintptr_t n) { return (x<<(64-n))|(x>>n); }
static Hash(hash_sym) { return getsym(x)->code; }
static Hash(hash_two) { return ror64(hash(v, A(x)) * hash(v, B(x)), 32); }
static Hash(hash_hom) { return hash(v, homnom(v, x)) ^ mix; }
static Hash(hash_num) { return ror64(mix * x, 16); }
static Hash(hash_nil) { return ror64(mix * Q(x), 48); }
static Hash(hash_str) {
  str s = getstr(x);
  uintptr_t len = s->len;
  char *us = s->text;
  for (uintptr_t h = 1;; h ^= *us++, h *= mix)
    if (!len--) return h; }

// shrinking a table never allocates memory, so it's safe
// to do at any time.
static void tbl_fit(em v, ob t) {
  if (tbl_load(t)) return;

  ent e = NULL, f, g;
  tbl u = gettbl(t);

  // collect all entries
  for (uintptr_t i = 1 << u->cap; i--;)
    for (f = u->tab[i], u->tab[i] = NULL; f;
      g = f->next, f->next = e,
      e = f, f = g);

  // shrink bucket array
  while (u->cap && tbl_load(t) < 1) u->cap--;

  // reinsert
  while (e) { uintptr_t i = tbl_idx(u->cap, hash(v, e->key));
              f = e->next,
              e->next = u->tab[i],
              u->tab[i] = e,
              e = f; } }

static ob tbl_del(em v, ob t, ob key) {
  tbl y = gettbl(t);
  ob val = nil;
  intptr_t b = tbl_idx(y->cap, hash(v, key));
  ent e = y->tab[b];
  struct ent prev = {0,0,e};
  for (ent l = &prev; l && l->next; l = l->next)
    if (l->next->key == key) {
      val = l->next->val;
      l->next = l->next->next;
      y->len--;
      break; }
  return y->tab[b] = prev.next, tbl_fit(v, t), val; }


// tbl_grow(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static ob tbl_grow(em v, ob t) {
  ent *tab0, *tab1;
  uintptr_t cap0 = gettbl(t)->cap, cap1 = cap0 + 1;
  with(t, tab1 = cells(v, 1<<cap1));
  if (!tab1) return 0;
  setw(tab1, 0, 1<<cap1);
  tab0 = gettbl(t)->tab;

  for (uintptr_t i, cap = 1 << cap0; cap--;)
    for (ent e, es = tab0[cap]; es;
      e = es, es = es->next,
      i = tbl_idx(cap1, hash(v, e->key)),
      e->next = tab1[i], tab1[i] = e);

  return gettbl(t)->cap = cap1, gettbl(t)->tab = tab1, t; }

static ob tbl_set_s(em v, ob t, ob k, ob x) {
  tbl y;
  ent e = tbl_ent(v, t, k);
  uintptr_t i = tbl_idx(gettbl(t)->cap, hash(v, k));
  return e ? e->val = x :
    (with(t, with(k, with(x, e = cells(v, Width(ent))))),
     !e ? 0 : (y = gettbl(t),
               e->key = k,
               e->val = x,
               e->next = y->tab[i],
               y->tab[i] = e,
               y->len += 1,
               x)); }

ob tbl_set(em v, ob t, ob k, ob x) {
  with(t, x = tbl_set_s(v, t, k, x));
  return !x ? 0 : tbl_load(t) <= 1 ? x :
    (with(x, t = tbl_grow(v, t)), t ? x : 0); }

ob tbl_get(em v, ob t, ob k) {
  ent e = tbl_ent(v, t, k);
  return e ? e->val : 0; }

ob table(em v) {
  tbl t = cells(v, Width(tbl) + 1);
  ent *b = (ent*)(t+1);
  return !t ? 0 :
    (t->len = t->cap = 0,
     t->tab = b,
     *b = NULL,
     puttbl(t)); }

ob string(em v, const char* c) {
  intptr_t bs = 1 + strlen(c);
  str o = cells(v, Width(str) + b2w(bs));
  return !o ? 0 :
    (memcpy(o->text, c, o->len = bs),
     putstr(o)); }

Vm(tbld) {
  Arity(2);
  TypeCheck(fp->argv[0], Tbl);
  return CallC(v->xp = tbl_del(v, fp->argv[0], fp->argv[1])),
         ApC(ret, xp); }


// string instructions
Vm(strl) {
  Arity(1);
  CheckType(*fp->argv, Str);
  return ApC(ret, putnum(getstr(*fp->argv)->len-1)); }

Vm(strg) {
  Arity(2);
  CheckType(fp->argv[0], Str);
  CheckType(fp->argv[1], Num);
  return ApC(ret,
    getnum(fp->argv[1]) < getstr(fp->argv[0])->len-1 ?
      putnum(getstr(fp->argv[0])->text[getnum(fp->argv[1])]) :
      nil); }

Vm(strconc) {
  intptr_t l = getnum(fp->argc), sum = 0, i = 0;
  while (i < l) {
    ob x = fp->argv[i++];
    CheckType(x, Str);
    sum += getstr(x)->len - 1; }
  intptr_t words = b2w(sum+1) + 1;
  Have(words);
  str d = (str) hp;
  hp += words;
  d->len = sum + 1;
  d->text[sum] = 0;
  for (str x; i;)
    x = getstr(fp->argv[--i]),
    sum -= x->len - 1,
    memcpy(d->text+sum, x->text, x->len - 1);
  return ApC(ret, putstr(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(strs) {
  Arity(3);
  CheckType(fp->argv[0], Str);
  CheckType(fp->argv[1], Num);
  CheckType(fp->argv[2], Num);
  str src = getstr(fp->argv[0]);
  intptr_t lb = getnum(fp->argv[1]), ub = getnum(fp->argv[2]);
  lb = max(lb, 0);
  ub = min(ub, src->len-1);
  ub = max(ub, lb);
  intptr_t words = 1 + b2w(ub - lb + 1);
  Have(words);
  str dst = (str) hp;
  hp += words;
  dst->len = ub - lb + 1;
  dst->text[ub - lb] = 0;
  memcpy(dst->text, src->text + lb, ub - lb);
  return ApC(ret, putstr(dst)); }

Vm(strmk) {
  intptr_t i = 0,
    bytes = getnum(fp->argc)+1,
    words = 1 + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (ob x; i < bytes-1; s->text[i++] = getnum(x)) {
    x = fp->argv[i];
    TypeCheck(x, Num);
    if (x == N0) break; }
  return s->text[i] = 0,
         s->len = i+1,
         ApC(ret, putstr(s)); }

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
static ob sskc(em v, ob*y, ob x) {
  int i;
  sym z;
  return !nilp(*y) ?
    (z = getsym(*y),
     i = strcmp(getstr(z->nom)->text, getstr(x)->text),
     i == 0 ? *y : sskc(v, i < 0 ? &z->r : &z->l, x)) :
    // FIXME the caller must ensure Avail >= Width(sym)
    // (because GC here would void the tree)
    (z = cells(v, Width(sym)),
     z->code = hash(v, z->nom = x) ^ mix,
     z->l = z->r = nil,
     *y = putsym(z)); }

ob intern(em v, ob x) {
  bool _; return
    Avail >= Width(sym) ||
    (with(x, _ = please(v, Width(sym))), _) ?
      sskc(v, &v->syms, x) :
      0; }

Vm(gsym_u) {
  sym y; return 
    fp->argc > N0 && strp(*fp->argv) ?
      (CallC(v->xp = intern(v, *fp->argv)),
       !xp ? 0 : ApC(ret, xp)) :

    sp - hp < Width(sym) ?
      (v->xp = Width(sym),
       ApC(gc, xp)) :

    (y = (sym) hp,
     hp += Width(sym),
     y->nom = y->l = y->r = nil,
     y->code = v->rand = lcprng(v->rand),
     ApC(ret, putsym(y))); }

Vm(ystr_u) {
  Arity(1);
  xp = *fp->argv;
  CheckType(xp, Sym);
  return ApC(ret, getsym(xp)->nom); }
