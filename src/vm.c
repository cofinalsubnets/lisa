#include "i.h"

#define Vm(n, ...) enum status\
  n(struct G *f, union cell *ip, intptr_t *hp, intptr_t *sp, ##__VA_ARGS__)

static NoInline Vm(gc, size_t n) {
  return Pack(), !please(f, n) ? Oom :
    f->ip->ap(f, f->ip, f->hp, f->sp); }

Vm(rec) {
  word x = sp[0], j = sp[1];
  sp += getnum(ip[1].x) + 1;
  if (nump(j)) ip = (verb) *++sp, *sp = j;
  else ip = (verb) j, *sp = x;
  return ip->ap(f, ip, hp, sp); }

#define Have(n) if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)

Vm(var) { Have1(); return
  sp[-1] = sp[getnum(ip[1].x)],
  ip[2].ap(f, ip + 2, hp, sp - 1); }

Vm(br) { return
  ip = nilp(*sp) ? ip[1].m : ip + 2,
  ip->ap(f, ip, hp, sp + 1); }

Vm(jump) { return ip[1].m->ap(f, ip[1].m, hp, sp); }

Vm(yield) { return Pack(), Ok; }

static Vm(Kj) { Have1(); return
  sp[-1] = ip[1].x,
  ip[2].m->ap(f, ip[2].m, hp, sp - 1); }

Vm(ret) { word r = *sp; return
  sp += getnum(ip[1].x) + 1,
  ip = (verb) *sp,
  *sp = r,
  ip->ap(f, ip, hp, sp); }

Vm(ap) {
  if (nump(sp[1])) return ip[1].ap(f, ip + 1, hp, sp + 1);
  verb k = (verb) sp[1]; return
    sp[1] = (word) (ip + 1),
    k->ap(f, k, hp, sp); }

Vm(cur) {
  intptr_t n = getnum(ip[1].x);
  if (n == 1) return ip[2].ap(f, ip + 2, hp, sp); // XXX base case of 1 is wasteful
  const size_t S = 5 + Width(struct tag);
  Have(S);
  verb k = (verb) hp;
  k[0].ap = cur, k[1].x = putnum(n - 1);
  k[2].ap = Kj,  k[3].x = *sp++, k[4].m = ip + 2;
  k[5].x = 0,    k[6].m = k;
  ip = (verb) *sp, *sp = (word) k;
  return ip->ap(f, ip, hp + S, sp); }

Vm(data) { word r = (word) ip; return
  ip = (verb) *++sp,
  *sp = r,
  ip->ap(f, ip, hp, sp); }

Vm(K) { Have1(); return
  sp[-1] = ip[1].x,
  ip[2].ap(f, ip + 2, hp, sp - 1); }
