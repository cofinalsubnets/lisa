#include "lips.h"
#include "terp.h"
#include <string.h>

ob string(en v, const char* c) {
  i64 bs = 1 + strlen(c);
  str o;
  bind(o, cells(v, Width(str) + b2w(bs)));
  cpy8(o->text, c, o->len = bs);
  return _S(o); }

// string instructions
Vm(strl) {
  Arity(1);
  CheckType(*Argv, Str);
  xp = _N(S(*Argv)->len-1);
  Jump(ret); }

Vm(strg) {
  Arity(2);
  CheckType(Argv[0], Str);
  CheckType(Argv[1], Num);
  xp = N(Argv[1]) < S(Argv[0])->len-1 ?
       _N(S(Argv[0])->text[N(Argv[1])]) :
       nil;
  Jump(ret); }

Vm(strconc) {
  i64 l = N(Argc), sum = 0, i = 0;
  while (i < l) {
    ob x = Argv[i++];
    CheckType(x, Str);
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
  Arity(3);
  CheckType(Argv[0], Str);
  CheckType(Argv[1], Num);
  CheckType(Argv[2], Num);
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
  for (ob x; i < bytes-1; s->text[i++] = N(x)) {
    x = Argv[i];
    Tc(x, Num);
    if (x == _N(0)) break; }
  s->text[i] = 0;
  s->len = i+1;
  Go(ret, _S(s)); }
