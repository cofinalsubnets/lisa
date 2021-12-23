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
 Have1(); hp[0] = xp, hp[1] = *sp++;
 xp = puttwo(hp); hp += 2; NEXT(1); }

VM(car_u) { ARY(1); TC(*ARGV, Two); GO(ret, A(*ARGV)); }
VM(cdr_u) { ARY(1); TC(*ARGV, Two); GO(ret, B(*ARGV)); }
VM(cons_u) {
 ARY(2); Have(2);
 hp[0] = ARGV[0], hp[1] = ARGV[1];
 xp = puttwo(hp), hp += 2; Jump(ret); }

#include "mem.h"
// functions for pairs and lists
obj pair(lips v, obj a, obj b) {
 if (Avail < 2) with(a, with(b, reqsp(v, 2)));
 obj t = puttwo(bump(v, 2));
 return A(t) = a, B(t) = b, t; }

GC(cptwo) {
 obj dst, src = x;
 return fresh(A(x)) ? A(x) :
  (dst = puttwo(bump(v, Width(two))),
   A(dst) = A(src),
   A(src) = dst,
   B(dst) = cp(v, B(src), len0, base0),
   CP(A(dst)),
   dst); }
