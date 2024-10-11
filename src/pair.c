#include "i.h"

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

// index of item in list
long lidx(core f, word l, word x) {
  for (long i = 0; twop(l); l = B(l), i++)
    if (eql(f, A(l), x)) return i;
  return -1; }

// list concat
word lconcat(core f, word l, word n) {
  if (!twop(l)) return n;
  avec(f, l, n = lconcat(f, B(l), n));
  return n ? (word) pairof(f, A(l), n) : n; }
  
// reverse list concat
word rlconcat(core f, word l, word n) {
  for (word m; twop(l);)
    m = l, l = B(l), B(m) = n, n = m;
  return n; }

// list length
size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

word lassoc(core f, word l, word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, A(A(l)))) return A(l);
  return 0; }

Vm(car) { return op(1, twop(sp[0]) ? A(sp[0]) : sp[0]); }
Vm(cdr) { return op(1, twop(sp[0]) ? B(sp[0]) : nil); }
Vm(cons) {
  Have(Width(struct pair));
  pair w = ini_pair((pair) hp, sp[0], sp[1]);
  hp += Width(struct pair);
  return op(2, (word) w); }

