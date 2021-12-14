#include "lips.h"
#include "str.h"
#include "mem.h"
#include "terp.h"

// for strings
obj string(lips v, const char* c) {
 i64 bs = 1 + slen(c);
 str o = cells(v, Size(str) + b2w(bs));
 cpy8(o->text, c, o->len = bs);
 return putstr(o); }

// string instructions
VM(strl) {
 ARY(1); TC(*ARGV, Str);
 GO(ret, N_(getstr(*ARGV)->len-1)); }

VM(strg) {
 ARY(2); TC(ARGV[0], Str); TC(ARGV[1], Num);
 GO(ret, N(ARGV[1]) < getstr(ARGV[0])->len-1 ?
  N_(getstr(ARGV[0])->text[N(ARGV[1])]) :
  nil); }

VM(strconc) {
 i64 l = Gn(ARGC), sum = 0, i = 0;
 while (i < l) {
  obj x = ARGV[i++];
  TC(x, Str);
  sum += S(x)->len - 1; }
 i64 words = b2w(sum+1) + 1;
 Have(words);
 str d = (str) hp;
 hp += words;
 d->len = sum + 1;
 d->text[sum] = 0;
 while (i) {
  str x = getstr(ARGV[--i]);
  sum -= x->len - 1;
  cpy8(d->text+sum, x->text, x->len - 1); }
 GO(ret, putstr(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
VM(strs) {
 ARY(3);
 TC(ARGV[0], Str); TC(ARGV[1], Num); TC(ARGV[2], Num);
 str src = getstr(ARGV[0]);
 i64 lb = Gn(ARGV[1]), ub = Gn(ARGV[2]);
 lb = max(lb, 0);
 ub = min(ub, src->len-1);
 ub = max(ub, lb);
 i64 words = 1 + b2w(ub - lb + 1);
 Have(words);
 str dst = (str) hp;
 hp += words;
 dst->len = ub - lb + 1;
 dst->text[ub - lb] = 0;
 cpy8(dst->text, src->text + lb, ub - lb);
 GO(ret, putstr(dst)); }

VM(strmk) {
 i64 i = 0, l = Gn(ARGC)+1, size = 1 + b2w(l);
 Have(size);
 str s = (str) hp;
 hp += size;
 for (obj x; i < l-1; s->text[i++] = Gn(x)) {
  x = ARGV[i];
  TC(x, Num);
  if (x == Pn(0)) break; }
 s->text[i] = 0;
 s->len = i+1;
 GO(ret, putstr(s)); }

GC(cpstr) {
 str dst, src = S(x);
 return src->len == 0 ? *(mem)src->text :
  (dst = bump(v, Width(str) + b2w(src->len)),
   cpy64(dst->text, src->text, b2w(src->len)),
   dst->len = src->len, src->len = 0,
   *(mem) src->text = _S(dst)); }
