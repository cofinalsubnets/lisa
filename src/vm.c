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
  return
    *--sp = ip[1].x,
    ip += 2,
    ip->ap(f, ip, hp, sp); }

status jump(state f, verb ip, word *hp, word *sp) {
  return ip = (mo) ip[1].x, ip->ap(f, ip, hp, sp); }
status Kj(state f, verb ip, word *hp, word *sp) {
  Have1();
  *--sp = ip[1].x;
  ip = (mo) ip[2].x;
  return ip->ap(f, ip, hp, sp); }

status branch(state f, verb ip, word *hp, word *sp) {
  ip = nilp(*sp++) ? ip[1].m : ip + 2;
  return ip->ap(f, ip, hp, sp); }

status retn(state f, verb ip, word *hp, word *sp) {
  ob r = *sp++;
  sp += getnum(ip[1].x);
  ip = (verb) *sp;
  *sp = r;
  return ip->ap(f, ip, hp, sp); }

status apply(state f, verb ip, word *hp, word *sp) {
  if (nump(sp[1])) ip++;
  else {
    mo j = (mo) sp[1];
    sp[1] = (ob) (ip + 1);
    ip = j; }
  return ip->ap(f, ip, hp, sp); }

status curry(state f, verb ip, word *hp, word *sp) {
  intptr_t n = getnum(ip[1].x);
  if (n == 1) return
    ip = ip[2].m, ip->ap(f, ip, hp, sp);
  const size S = 3 + Width(struct tag);
  Have(2 * S);
  verb c0 = (mo) hp, c1;
  hp += S;
  c0[0].ap = Kj;
  c0[1].x = *sp++;
  c0[2].x = ip[2].x;
  c1 = c0 + S,
  hp += S,
  c1[0].ap = curry,
  c1[1].x = putnum(n - 1),
  c1[2].x = (ob) c0,
  c0 = c1;
  ip = (mo) *sp;
  *sp = (ob) c0;
  return ip->ap(f, ip, hp, sp); }
