#include "lips.h"
#include "symbol.h"
#include "table.h"

////
/// data constructors and utility functions
//

// functions for pairs and lists
obj pair(lips v, obj a, obj b) {
 if (Avail < 2) with(a, with(b, reqsp(v, 2)));
 obj t = puttwo(bump(v, 2));
 return A(t) = a, B(t) = b, t; }

// for strings
obj string(lips v, const char* c) {
 i64 bs = 1 + slen(c);
 str o = cells(v, Size(str) + b2w(bs));
 cpy8(o->text, c, o->len = bs);
 return putstr(o); }
