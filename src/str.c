#include "lips.h"
#include "str.h"
#include "mem.h"

// for strings
obj string(lips v, const char* c) {
  i64 bs = 1 + slen(c);
  str o = cells(v, Width(str) + b2w(bs));
  cpy8(o->text, c, o->len = bs);
  return _S(o); }

GC(cpstr) {
  str dst, src = S(x);
  return src->len == 0 ? *(mem)src->text :
    (dst = bump(v, Width(str) + b2w(src->len)),
     cpy64(dst->text, src->text, b2w(src->len)),
     dst->len = src->len, src->len = 0,
     *(mem) src->text = _S(dst)); }

u0 emstr(lips v, FILE *o, obj x) {
  str s = S(x);
  fputc('"', o);
  for (char *t = s->text; *t; fputc(*t++, o))
    if (*t == '"') fputc('\\', o);
  fputc('"', o); }

#include "terp.h"
// string instructions
VM(strl) {
  ARY(1);
  TC(*Argv, Str);
  xp = _N(S(*Argv)->len-1);
  Jump(ret); }

VM(strg) {
  ARY(2);
  TC(Argv[0], Str);
  TC(Argv[1], Num);
  xp = 
     N(Argv[1]) < S(Argv[0])->len-1 ?
    _N(S(Argv[0])->text[N(Argv[1])]) :
    nil;
  Jump(ret); }

VM(strconc) {
  i64 l = N(Argc), sum = 0, i = 0;
  while (i < l) {
    obj x = Argv[i++];
    TC(x, Str);
    sum += S(x)->len - 1; }
  i64 words = b2w(sum+1) + 1;
  Have(words);
  str d = (str) hp;
  hp += words;
  d->len = sum + 1;
  d->text[sum] = 0;
  while (i) {
    str x = S(Argv[--i]);
    sum -= x->len - 1;
    cpy8(d->text+sum, x->text, x->len - 1); }
  GO(ret, _S(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
VM(strs) {
  ARY(3);
  TC(Argv[0], Str);
  TC(Argv[1], Num);
  TC(Argv[2], Num);
  str src = S(Argv[0]);
  i64 lb = N(Argv[1]), ub = N(Argv[2]);
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
  GO(ret, _S(dst)); }

VM(strmk) {
  i64 i = 0, bytes = N(Argc)+1, words = 1 + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (obj x; i < bytes-1; s->text[i++] = N(x)) {
    x = Argv[i];
    TC(x, Num);
    if (x == _N(0)) break; }
  s->text[i] = 0;
  s->len = i+1;
  GO(ret, _S(s)); }
