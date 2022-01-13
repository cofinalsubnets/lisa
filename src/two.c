#include "lips.h"
#include "two.h"

u64 llen(obj l) {
  for (u64 i = 0;; l = B(l), i++)
    if (!twop(l)) return i; }

#include "terp.h"
#include "hom.h"
// pairs
OP1(car, A(xp)) OP1(cdr, B(xp))
VM(cons) {
  Have1();
  hp[0] = xp;
  hp[1] = *sp++;
  xp = puttwo(hp);
  hp += 2;
  NEXT(1); }

VM(car_u) {
  Ary(1);
  Tc(*Argv, Two);
  Go(ret, A(*Argv)); }

VM(cdr_u) {
  Ary(1);
  Tc(*Argv, Two);
  Go(ret, B(*Argv)); }

VM(cons_u) {
  Ary(2);
  Have(2);
  two w = (two) hp;
  hp += 2;
  w->a = Argv[0], w->b = Argv[1];
  Go(ret, puttwo(w)); }

#include "mem.h"
// functions for pairs and lists
obj pair(lips v, obj a, obj b) {
  two w;
  with(a, with(b, w = cells(v, 2)));
  w->a = a, w->b = b;
  return puttwo(w); }

GC(cptwo) {
  obj dst, src = x;
  if (fresh(A(x))) return A(x);
  dst = puttwo(bump(v, Width(two)));
  A(dst) = A(src), A(src) = dst;
  B(dst) = cp(v, B(src), len0, base0);
  A(dst) = cp(v, A(dst), len0, base0);
  return dst; }

#include "write.h"
static u0 emtwo_(lips v, FILE *o, two w) {
  twop(w->b) ? (emsep(v, w->a, o, ' '),
                emtwo_(v, o, gettwo(w->b))) :
               emsep(v, w->a, o, ')'); }

u0 emtwo(lips v, FILE *o, obj x) {
  two w = gettwo(x);
  w->a == Qt && twop(w->b) && nilp(B(w->b)) ?
    (fputc('\'', o),
     emit(v, o, A(w->b))) :
    (fputc('(', o),
     emtwo_(v, o, w)); }
