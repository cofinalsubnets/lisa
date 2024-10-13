#include "i.h"

static intptr_t liprng(intptr_t seed) {
  const word steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (steele_vigna_2021 * seed + 1) >> 8; }

word l_rand(core f) {
  return f->rand = liprng(f->rand); }
Vm(add) { return op(2, putnum(getnum(sp[0])+getnum(sp[1]))); }
Vm(sub) { return op(2, putnum(getnum(sp[0])-getnum(sp[1]))); }
Vm(mul) { return op(2, putnum(getnum(sp[0])*getnum(sp[1]))); }
Vm(quot) { return op(2, nilp(sp[1]) ? nil : putnum(getnum(sp[0])/getnum(sp[1]))); }
Vm(rem) { return op(2, nilp(sp[1]) ? nil : putnum(getnum(sp[0])%getnum(sp[1]))); }
Vm(eq) { return op(2, eql(f, sp[0], sp[1]) ? putnum(-1) : nil); }
Vm(lt) { return op(2, sp[0] < sp[1] ? putnum(-1) : nil); }
Vm(le) { return op(2, sp[0] <= sp[1] ? putnum(-1) : nil); }
Vm(gt) { return op(2, sp[0] > sp[1] ? putnum(-1) : nil); }
Vm(ge) { return op(2, sp[0] >= sp[1] ? putnum(-1) : nil);}
Vm(not) { return op(1, ~sp[0] | 1); }
Vm(rng) { return op(1, putnum(l_rand(f))); }

Vm(Xp) { return
  ip = (thread) sp[1],
  sp[1] = twop(sp[0]) ? putnum(-1) : nil,
  ip->ap(f, ip, hp, sp + 1); }

Vm(Np) { return
  ip = (thread) sp[1],
  sp[1] = nump(sp[0]) ? putnum(-1) : nil,
  ip->ap(f, ip, hp, sp + 1); }

Vm(Sp) { return
  ip = (thread) sp[1],
  sp[1] = strp(sp[0]) ? putnum(-1) : nil,
  ip->ap(f, ip, hp, sp + 1); }

bool eql(core f, word a, word b) {
  if (a == b) return true;
  if (nump(a | b) ||
      ptr(a)->ap != data ||
      ptr(b)->ap != data ||
      ptr(a)[1].typ != ptr(b)[1].typ) return false;
  return ptr(a)[1].typ->equal(f, a, b); }

bool literal_equal(core f, word a, word b) { return a == b; }
