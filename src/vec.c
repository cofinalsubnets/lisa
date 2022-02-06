#include "lips.h"
#include "terp.h"
#include "hom.h"
#include "vec.h"

// this is used to create closures.
Vm(take) {
 u64 n = N((obj) H(ip)[1]);
 Have(n + 1);
 vec t = (vec) hp;
 hp += n + 1;
 t->len = n;
 cpy64(t->xs, sp, n);
 sp += n;
 Go(ret, _V(t)); }

Vm(vset_u) {
  Ary(3);
  Tc(Argv[0], Vec);
  Tc(Argv[1], Num);
  i64 idx = N(Argv[1]);
  vec ary = V(Argv[0]);
  if (idx < 0 || idx >= ary->len) {
    v->xp = idx, v->ip = ary->len;
    Jump(oob_error); }
  Go(ret, ary->xs[idx] = Argv[2]); }

Vm(vget_u) {
 Ary(2);
 Tc(Argv[0], Vec);
 Tc(Argv[1], Num);
 i64 idx = N(Argv[1]);
 vec ary = V(Argv[0]);
 if (idx < 0 || idx >= ary->len) {
   v->xp = idx, v->ip = ary->len;
   Jump(oob_error); }
 Go(ret, ary->xs[idx]); }

Vm(vec_u) {
 obj n = N(Argc);
 Have(n + 1);
 vec t = (vec) hp;
 hp += 1 + n;
 cpy64(t->xs, Argv, t->len = n);
 Go(ret, putvec(t)); }

#include "write.h"
u0 emvec(lips v, FILE *o, obj x) {
  vec e = V(x);
  fputc('[', o);
  if (e->len) for (mem i = e->xs, l = i + e->len;;) {
    emit(v, o, *i++);
    if (i < l) fputc(' ', o);
    else break; }
  fputc(']', o); }

#include "mem.h"
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
