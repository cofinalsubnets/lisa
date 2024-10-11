#include "i.h"

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
