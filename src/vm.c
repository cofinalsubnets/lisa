#include "i.h"

#define Vm(n, ...) enum status\
  n(state f, thread ip, heap hp, stack sp, ##__VA_ARGS__)

static NoInline Vm(gc, size_t n) {
  return Pack(), !please(f, n) ? Oom :
    f->ip->ap(f, f->ip, f->hp, f->sp); }

Vm(tap) {
  word x = sp[0], j = sp[1];
  sp += getnum(ip[1].x) + 1;
  if (nump(j)) ip = (verb) *++sp, *sp = j;
  else ip = (verb) j, *sp = x;
  return ip->ap(f, ip, hp, sp); }

#define Have(n)\
  if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define Have1()\
  if (sp == hp) return gc(f, ip, hp, sp, 1)

Vm(ref) {
  Have1();
  sp[-1] = sp[getnum(ip[1].x)];
  return ip[2].ap(f, ip + 2, hp, sp - 1); }

Vm(cond) {
  ip = nilp(*sp) ? ip[1].m : ip + 2;
  return ip->ap(f, ip, hp, sp + 1); }

Vm(jump) {
  return ip[1].m->ap(f, ip[1].m, hp, sp); }

Vm(yield) {
  return Pack(), Ok; }

static Vm(Kj) {
  Have1();
  sp[-1] = ip[1].x;
  return ip[2].m->ap(f, ip[2].m, hp, sp - 1); }

Vm(ret) {
  word r = *sp;
  sp += getnum(ip[1].x) + 1,
  ip = (verb) *sp,
  *sp = r;
  return ip->ap(f, ip, hp, sp); }

Vm(ap) {
  if (nump(sp[1]))
    return ip[1].ap(f, ip + 1, hp, sp + 1);
  thread k = (verb) sp[1];
  sp[1] = (word) (ip + 1);
  return k->ap(f, k, hp, sp); }

/*
Vm(apply) {
  if (nump(sp[0])) return
    sp[1] = sp[0],
    ip[1].ap(f, ip + 1, hp, sp + 1);
}
*/

Vm(curry) {
  thread k;
  size_t n = getnum(ip[1].x),
         S = 3 + Width(struct tag);
  if (n < 3) {
    Have(S);
    k = (thread) hp;
    k[0].ap = Kj,    k[1].x = *sp++, k[2].m = ip + 2;
    k[3].x = 0,      k[4].m = k; }
  else {
    S += 2;
    Have(S);
    k = (thread) hp;
    k[0].ap = curry, k[1].x = putnum(n - 1);
    k[2].ap = Kj,    k[3].x = *sp++, k[4].m = ip + 2;
    k[5].x = 0,      k[6].m = k; }
  ip = (verb) *sp, *sp = (word) k;
  return ip->ap(f, ip, hp + S, sp); }

Vm(data) {
  word r = (word) ip;
  ip = (verb) *++sp;
  *sp = r;
  return ip->ap(f, ip, hp, sp); }

Vm(K) {
  Have1();
  sp[-1] = ip[1].x;
  return ip[2].ap(f, ip + 2, hp, sp - 1); }

Vm(print) {
  transmit(f, stdout, *sp), puts("");
  return ip[1].ap(f, ip + 1, hp, sp); }

Vm(add) {
  sp[1] = (sp[0] | 1) + (sp[1] & ~1);
  return ip[1].ap(f, ip + 1, hp, sp + 1); }

Vm(equal) {
  sp[1] = eql(f, sp[0], sp[1]) ? putnum(-1) : nil;
  return ip[1].ap(f, ip + 1, hp, sp + 1); }
