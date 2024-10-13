#include "i.h"

static intptr_t liprng(intptr_t seed) {
  const word steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (steele_vigna_2021 * seed + 1) >> 8; }

word l_rand(core f) {
  return f->rand = liprng(f->rand); }
Vm(add) { return op(2, putnum(getnum(sp[0])+getnum(sp[1]))); }
Vm(sub) { return op(2, putnum(getnum(sp[0])-getnum(sp[1]))); }
Vm(mul) { return op(2, putnum(getnum(sp[0])*getnum(sp[1]))); }
Vm(quot) { return op(2, nilp(sp[1]) ? nil : putnum(getnum(sp[0])/getnum(sp[1]))); }
Vm(rem) { return op(2, nilp(sp[1]) ? nil : putnum(getnum(sp[0])%getnum(sp[1]))); }
Vm(eq) { return op(2, eql(f, sp[0], sp[1]) ? putnum(-1) : nil); }
Vm(lt) { return op(2, sp[0] < sp[1] ? putnum(-1) : nil); }
Vm(le) { return op(2, sp[0] <= sp[1] ? putnum(-1) : nil); }
Vm(gt) { return op(2, sp[0] > sp[1] ? putnum(-1) : nil); }
Vm(ge) { return op(2, sp[0] >= sp[1] ? putnum(-1) : nil);}
Vm(not) { return op(1, ~sp[0] | 1); }
Vm(rng) { return op(1, putnum(l_rand(f))); }

Vm(Xp) { return
  ip = (thread) sp[1],
  sp[1] = twop(sp[0]) ? putnum(-1) : nil,
  ip->ap(f, ip, hp, sp + 1); }

Vm(Np) { return
  ip = (thread) sp[1],
  sp[1] = nump(sp[0]) ? putnum(-1) : nil,
  ip->ap(f, ip, hp, sp + 1); }

Vm(Sp) { return
  ip = (thread) sp[1],
  sp[1] = strp(sp[0]) ? putnum(-1) : nil,
  ip->ap(f, ip, hp, sp + 1); }

bool eql(core f, word a, word b) {
  if (a == b) return true;
  if (nump(a | b) ||
      ptr(a)->ap != data ||
      ptr(b)->ap != data ||
      ptr(a)[1].typ != ptr(b)[1].typ) return false;
  return ptr(a)[1].typ->equal(f, a, b); }

bool literal_equal(core f, word a, word b) { return a == b; }

//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
thread mo_ini(void *_, size_t len) {
  struct tag *t = (void*) ((cell) _ + len);
  return t->null = NULL, t->head = _; }

// allocate a thread
thread mo_n(core f, size_t n) {
  thread k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

struct tag *ttag(thread k) {
  return k->x ? ttag(k + 1) : (void*) k; }

Vm(trim) {
  thread k = (thread) sp[0];
  ttag(k)->head = k;
  return op(1, (word) k); }

Vm(seek) {
  thread k = (thread) sp[1];
  return op(2, (word) (k + getnum(sp[0]))); }

Vm(peek) {
  thread k = (thread) sp[0];
  return op(1, k[0].x); }

Vm(poke) {
  thread k = (thread) sp[1];
  k->x = sp[0];
  return op(2, (word) k); }

Vm(thda) {
  size_t n = getnum(sp[0]);
  Have(n + Width(struct tag));
  thread k = mo_ini(memset(hp, -1, n * sizeof(word)), n);
  hp += n + Width(struct tag);
  return op(1, (word) k); }

static word cp_two(state v, word x, word *p0, word *t0) {
  pair src = (pair) x,
       dst = bump(v, Width(struct pair));
  dst->ap = data, dst->typ = &pair_type;
  dst->a = src->a, dst->b = src->b;
  return (word) (src->ap = (vm*) dst); }

static void wk_two(state v, word x, word *p0, word *t0) {
  v->cp += Width(struct pair);
  A(x) = cp(v, A(x), p0, t0);
  B(x) = cp(v, B(x), p0, t0); }

static void print_two(core v, output o, word x) {
  for (o->putc(v, o, '(');; o->putc(v, o, ' ')) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) { o->putc(v, o, ')'); break; } } }

// FIXME could overflow the stack -- use off pool for this
static bool eq_two(state f, word x, word y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static word hash_two(core v, word x) {
  word hc = hash(v, A(x)) * hash(v, B(x));
  return hc ^ mix; }

struct typ pair_type = {
  .hash = hash_two,
  .copy = cp_two,
  .evac = wk_two,
  .emit = print_two,
  .equal = eq_two, };

pair ini_pair(two w, word a, word b) {
  w->ap = data, w->typ = &pair_type;
  w->a = a, w->b = b;
  return w; }

pair pairof(core f, word a, word b) {
  if (avail(f) < Width(struct pair)) {
    bool ok;
    avec(f, a, avec(f, b, ok = f->please(f, Width(struct pair))));
    if (!ok) return 0; }
  two w = (two) f->hp;
  f->hp += Width(struct pair);
  return ini_pair(w, a, b); }


Vm(car) { return op(1, twop(sp[0]) ? A(sp[0]) : sp[0]); }
Vm(cdr) { return op(1, twop(sp[0]) ? B(sp[0]) : nil); }
Vm(cons) {
  Have(Width(struct pair));
  pair w = ini_pair((pair) hp, sp[0], sp[1]);
  hp += Width(struct pair);
  return op(2, (word) w); }


static word copy_string(state v, word x, word *p0, word *t0) {
  string src = (string) x;
  size_t len = sizeof(struct string) + src->len;
  return (word) (src->ap = memcpy(bump(v, b2w(len)), src, len)); }

static void walk_string(state v, word x, word *p0, word *t0) {
  v->cp += Width(struct string) + b2w(((string) x)->len); }

static void print_string(core v, output o, word _) {
  string s = (string) _;
  size_t len = s->len;
  const char *text = s->text;
  o->putc(v, o, '"');
  for (char c; len--; o->putc(v, o, c))
    if ((c = *text++) == '\\' || c == '"') o->putc(v, o, '\\');
  o->putc(v, o, '"'); }

static word hash_string(core v, word _) {
  string s = (string) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(word),
         bytes = s->len % sizeof(word);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const intptr_t *ws = (intptr_t*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static bool string_equal(state f, word x, word y) {
  string a = (string) x, b = (string) y;
  if (a->len != b->len) return false;
  return 0 == strncmp(a->text, b->text, a->len); }

struct typ string_type = {
  .hash = hash_string,
  .copy = copy_string,
  .evac = walk_string,
  .emit = print_string,
  .equal = string_equal, };

Vm(slen) {
  word x = sp[0];
  ip = (thread) sp[1];
  sp[1] = strp(x) ? putnum(((string)x)->len) : nil;
  return ip->ap(f, ip, hp, sp + 1); }

#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))
Vm(ssub) {
  thread r = (thread) sp[3];
  if (!strp(sp[0])) sp[3] = nil;
  else {
    string s = (string) sp[0];
    size_t i = nump(sp[1]) ? getnum(sp[1]) : 0,
           j = nump(sp[2]) ? getnum(sp[2]) : 0;
    i = max(i, 0), j = min(j, s->len);
    Have(Width(struct string) + b2w(j - i));
    string t = ini_str((string) hp, j - i);
    memcpy(t->text, s->text + i, j);
    sp[3] = (word) t; }
  return r->ap(f, r, hp, sp + 3); }

Vm(sget) {
  thread r = (thread) sp[2];
  if (!strp(sp[0])) sp[2] = nil;
  else {
    string s = (string) sp[0];
    size_t i = min(s->len - 1, getnum(sp[1]));
    i = max(i, 0);
    sp[2] = putnum(s->text[i]); }
  return r->ap(f, r, hp, sp + 2); }

string ini_str(string s, size_t len) {
  s->ap = data, s->typ = &string_type, s->len = len;
  return s; }
Vm(scat) {
  word a = sp[0], b = sp[1];
  a = strp(a) ? a : nil;
  b = strp(b) ? b : nil;
  if (nilp(a)) return op(2, b);
  if (nilp(b)) return op(2, a);
  string x = (string) a, y = (string) b;
  size_t len = x->len + y->len,
         req = Width(struct string) + b2w(len);
  Have(req);
  string z = ini_str((string) hp, len);
  hp += req;
  memcpy(z->text, x->text, x->len);
  memcpy(z->text + x->len, y->text, y->len);
  return op(2, (word) z); }

static symbol intern_r(core, string, symbol*);

static Inline symbol ini_sym(void *_, string nom, uintptr_t code) {
  symbol y = _; return
    y->ap = data, y->typ = &symbol_type,
    y->nom = nom, y->code = code,
    y->l = y->r = 0, y; }

static Inline symbol ini_anon(void *_, word code) {
  symbol y = _;
  y->ap = data, y->typ = &symbol_type;
  y->nom = 0, y->code = code;
  return y;  }

static word hash_symbol(core v, word _) { return ((symbol) _)->code; }
static word copy_symbol(core f, word x, word *p0, word *t0) {
  symbol src = (symbol) x,
         dst = src->nom ?
           intern_r(f, (string) cp(f, (word) src->nom, p0, t0), &f->symbols) :
           ini_anon(bump(f, Width(struct symbol) - 2), src->code);
  return (word) (src->ap = (vm*) dst); }
static void walk_symbol(core f, word x, word *p0, word *t0) {
  f->cp += Width(struct symbol) - (((symbol)x)->nom ? 0 : 2); }

  /*
static bool atomp(string s) {
  const char cc[] = " \n\t;#()\"'";
  for (size_t i = 0; i < s->len; i++)
    for (const char *c = cc; *c; c++)
      if (s->text[i] == *c) return false;
  return true; }
  */

static void print_symbol(core f, output o, word x) {
  string s = ((symbol) x)->nom;
  if (s) for (int i = 0; i < s->len; o->putc(f, o, s->text[i++]));
  else outputs(f, o, "#gensym@"), print_num(f, o, x, 16); }

struct typ symbol_type = {
  .hash = hash_symbol,
  .copy = copy_symbol,
  .evac = walk_symbol,
  .equal = literal_equal,
  .emit = print_symbol,
};


static symbol intern_r(core v, string b, symbol *y) {
  symbol z = *y;
  if (!z) return *y =
    ini_sym(bump(v, Width(struct symbol)), b, hash(v, putnum(hash(v, (word) b))));
  string a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z : intern_r(v, b, i < 0 ? &z->l : &z->r); }

symbol intern(core f, string b) {
  if (avail(f) < Width(struct symbol)) {
    bool ok;
    avec(f, b, ok = f->please(f, Width(struct symbol)));
    if (!ok) return 0; }
  return intern_r(f, b, &f->symbols); }


symbol literal_symbol(core f, const char *nom) {
  size_t len = strlen(nom);
  string o = cells(f, Width(struct string) + b2w(len));
  if (!o) return 0;
  memcpy(o->text, nom, len);
  return intern(f, ini_str(o, len)); }

Vm(gensym) {
  const int req = Width(struct symbol) - 2;
  Have(req);
  symbol y = (symbol) hp;
  hp += req;
  return op(1, (word) ini_anon(y, l_rand(f))); }

Vm(string_of_symbol) {
  word x = sp[0];
  if (!symp(x)) return op(1, nil);
  string y = ((symbol) x)->nom;
  return op(1, y ? (word) y : nil); }

Vm(symbol_of_string) {
  word x = sp[0];
  if (!strp(x)) return op(1, nil);
  Pack(f);
  symbol y = intern(f, (string) x);
  Unpack(f);
  return !y ? Oom : op(1, (word) y); }


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

static void print_table(core f, output o, word x) {
  table t = (table) x;
  outputs(f, o, "#table:");
  print_num(f, o, t->len, 10);
  o->putc(f, o, '/');
  print_num(f, o, t->cap, 10);
  o->putc(f, o, '@');
  print_num(f, o, x, 16); }

struct typ table_type = {
  .hash = hash_table,
  .copy = copy_table,
  .evac = walk_table,
  .equal = literal_equal,
  .emit = print_table, };

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

  if (datp(x)) return dtyp(x)->hash(v, x);
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
  struct table_entry **tab0, **tab1;
  word cap1 = 2 * cap0;
  avec(f, t, tab1 = cells(f, cap1));
  tab0 = t->tab;
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
  if (eql(f, e->key, k)) return t->len--, *v = e->val, e->next;
  return e->next = table_delete_r(f, t, k, v, e->next), e; }

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
  t->len = 0, t->cap = 1, t->tab = tab, tab[0] = 0;
  return op(1, (word) t); }

word table_get(core f, table t, word k, word zero) {
  struct table_entry *entry = t->tab[index_of_key(f, t, k)];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  return entry ? entry->val : zero; }

Vm(tget) {
  return op(3, !tblp(sp[1]) ? sp[2] :
    table_get(f, (table) sp[1], sp[2], sp[0])); }

Vm(tset) {
  word x = sp[0];
  if (!tblp(x)) return op(3, nil);
  Pack(f);
  table t = table_set(f, (table) x, sp[1], sp[2]);
  Unpack(f);
  return !t ? Oom : op(3, sp[2]); }

Vm(tdel) {
  word x = sp[1];
  return op(3, !tblp(x) ? nil :
    table_delete(f, (table) x, sp[2], sp[0])); }

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
  pair pairs = (pair) hp;
  hp += len * Width(struct pair);
  for (int i = t->cap; i;)
    for (struct table_entry *e = t->tab[--i]; e; e = e->next)
      pairs->ap = data, pairs->typ = &pair_type,
      pairs->a = e->key, pairs->b = list,
      list = (word) pairs, pairs++;
  return op(1, list); }

