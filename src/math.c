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
