#include "lips.h"
#include "str.h"
#include "mem.h"
#include <string.h>

// for strings
obj string(lips v, const char* c) {
  i64 bs = 1 + strlen(c);
  str o;
  bind(o, cells(v, Width(str) + b2w(bs)));
  cpy8(o->text, c, o->len = bs);
  return _S(o); }

#include "terp.h"
// string instructions
Vm(strl) {
  Ary(1);
  Tc(*Argv, Str);
  xp = _N(S(*Argv)->len-1);
  Jump(ret); }

Vm(strg) {
  Ary(2);
  Tc(Argv[0], Str);
  Tc(Argv[1], Num);
  xp = N(Argv[1]) < S(Argv[0])->len-1 ?
       _N(S(Argv[0])->text[N(Argv[1])]) :
       nil;
  Jump(ret); }

Vm(strconc) {
  i64 l = N(Argc), sum = 0, i = 0;
  while (i < l) {
    obj x = Argv[i++];
    Tc(x, Str);
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
  Go(ret, _S(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(strs) {
  Ary(3);
  Tc(Argv[0], Str);
  Tc(Argv[1], Num);
  Tc(Argv[2], Num);
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
  Go(ret, _S(dst)); }

Vm(strmk) {
  i64 i = 0, bytes = N(Argc)+1, words = 1 + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (obj x; i < bytes-1; s->text[i++] = N(x)) {
    x = Argv[i];
    Tc(x, Num);
    if (x == _N(0)) break; }
  s->text[i] = 0;
  s->len = i+1;
  Go(ret, _S(s)); }
