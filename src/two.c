#include "lips.h"
#include "two.h"
#include "mem.h"
#include "terp.h"
#include "hom.h"
#include "err.h"

// functions for pairs and lists
obj pair(lips v, obj a, obj b) {
 if (Avail < 2) with(a, with(b, reqsp(v, 2)));
 obj t = puttwo(bump(v, 2));
 return A(t) = a, B(t) = b, t; }

u64 llen(obj l) {
 for (u64 i = 0;; l = Y(l), i++)
  if (!twop(l)) return i; }

// pairs
OP1(car, X(xp)) OP1(cdr, Y(xp))
VM(cons) {
 Have1(); hp[0] = xp, hp[1] = *sp++;
 xp = puttwo(hp); hp += 2; NEXT(1); }

VM(car_u) { ARY(1); TC(*ARGV, Two); GO(ret, X(*ARGV)); }
VM(cdr_u) { ARY(1); TC(*ARGV, Two); GO(ret, Y(*ARGV)); }
VM(cons_u) {
 ARY(2); Have(2);
 hp[0] = ARGV[0], hp[1] = ARGV[1];
 xp = puttwo(hp), hp += 2; Jump(ret); }

