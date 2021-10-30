#include "lips.h"
#include "str.h"
#include "mem.h"

// for strings
obj string(lips v, const char* c) {
 i64 bs = 1 + slen(c);
 str o = cells(v, Size(str) + b2w(bs));
 cpy8(o->text, c, o->len = bs);
 return putstr(o); }
