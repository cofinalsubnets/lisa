#include "i.h"

status yield(state f, verb ip, word *hp, word *sp) {
  return Pack(), Ok; }

status ref(state f, verb ip, word *hp, word *sp) {
  Have1();
  word x = sp[getnum(ip[1].x)];
  *--sp = x;
  ip += 2;
  return ip->ap(f, ip, hp, sp); }

status K(state f, verb ip, word *hp, word *sp) {
  Have1();
  *--sp = ip[1].x;
  ip += 2;
  return  ip->ap(f, ip, hp, sp); }

status jump(state f, verb ip, word *hp, word *sp) {
  ip = ip[1].m;
  return ip->ap(f, ip, hp, sp); }

status Kj(state f, verb ip, word *hp, word *sp) {
  Have1();
  *--sp = ip[1].x;
  ip = ip[2].m;
  return ip->ap(f, ip, hp, sp); }

status branch(state f, verb ip, word *hp, word *sp) {
  ip = nilp(*sp++) ? ip[1].m : ip + 2;
  return ip->ap(f, ip, hp, sp); }

status retn(state f, verb ip, word *hp, word *sp) {
  word r = *sp;
  sp += getnum(ip[1].x) + 1;
  ip = (verb) *sp;
  *sp = r;
  return ip->ap(f, ip, hp, sp); }

status apply(state f, verb ip, word *hp, word *sp) {
  if (nump(sp[1])) return ip[1].ap(f, ip + 1, hp, sp + 1);
  verb k = (verb) sp[1];
  sp[1] = (word) (ip + 1);
  return k->ap(f, k, hp, sp); }

status curry(state f, verb ip, word *hp, word *sp) {
  intptr_t n = getnum(ip[1].x);
  if (n == 1) return ip = ip[2].m, ip->ap(f, ip, hp, sp);
  const size S = 6 + Width(struct tag);
  Have(S);
  verb k = (verb) hp;
  hp += S;
  k[0].ap = curry;
  k[1].x = putnum(n - 1);
  k[2].m = k + 3;
  k[3].ap = Kj;
  k[4].x = *sp++;
  k[5].x = ip[2].x;
  k[6].x = 0;
  k[7].m = k;
  ip = (verb) *sp;
  *sp = (word) k;
  return ip->ap(f, ip, hp, sp); }
