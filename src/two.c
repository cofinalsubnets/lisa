#include "lips.h"
#include "two.h"
#include "mem.h"

// functions for pairs and lists
obj pair(lips v, obj a, obj b) {
 if (Avail < 2) with(a, with(b, reqsp(v, 2)));
 obj t = puttwo(bump(v, 2));
 return A(t) = a, B(t) = b, t; }

u64 llen(obj l) {
 for (u64 i = 0;; l = Y(l), i++)
  if (!twop(l)) return i; }
