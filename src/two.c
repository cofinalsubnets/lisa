#include "la.h"
#include "two.h"
#include "gc.h"
#include "alloc.h"

// pairs and lists
static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok;
  with(a, with(b, ok = please(v, b2w(sizeof(struct two)))));
  return ok ? pair(v, a, b) : 0; }

two pair(la v, ob a, ob b) {
  return Avail >= b2w(sizeof(struct two)) ?
    ini_two(bump(v, b2w(sizeof(struct two))), a, b) :
    pair_gc(v, a, b); }

#include "vm.h"
Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(b2w(sizeof(struct two)));
  xp = (ob) ini_two(hp, xp, *sp++);
  hp += b2w(sizeof(struct two));
  return ApN(1, xp); }

Vm(car_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(twop(xp));
  return ApC(ret, A(xp)); }

Vm(cdr_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(twop(xp));
  return ApC(ret, B(xp)); }

Vm(cons_f) {
  ArityCheck(2);
  Have(b2w(sizeof(struct two)));
  xp = (ob) ini_two(hp, fp->argv[0], fp->argv[1]);
  hp += b2w(sizeof(struct two));
  return ApC(ret, xp); }

static Vm(aptwo) {
  return ApC(ret, fp->argc ? B(ip) : A(ip)); }

static Gc(cptwo) {
  two src = (two) x,
      dst = bump(v, b2w(sizeof(struct two)));
  src->head.disp = (vm*) dst;
  return (ob) ini_two(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

#include "tx.h"
#include "lexicon.h"
static long txtwo(la v, FILE *o, ob x) {
  long r = 2;
  if (fputc(LA_CH_LPAREN, o) == EOF) return -1;
  for (;;) {
    long i = la_tx(v, o, A(x));
    if (i < 0) return i;
    else r += i;
    if (!twop(x = B(x))) break;
    else if (fputc(LA_CH_SPACE, o) == EOF) return -1;
    else r++; }
  if (fputc(LA_CH_RPAREN, o) == EOF) return -1;
  return r; }

#include "hash.h"
static intptr_t hxtwo(la v, ob x) {
  intptr_t hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(intptr_t)); }

#include "cmp.h"
static bool eqtwo(la v, ob x, ob y) {
  return twop(y) &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

const struct mtbl mtbl_two = {
  .does = aptwo,
  .emit = txtwo,
  .evac = cptwo,
  .hash = hxtwo,
  .equi = eqtwo, };
