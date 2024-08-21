#include "i.h"

#define op(n,x) (ip = (thread) sp[n], sp[n] = (x), ip->ap(f, ip, hp, sp + n))
#define O op
#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)
#define Do(...) ((__VA_ARGS__), ip->ap(f, ip, hp, sp))
Vm(mbind) {
  Have(2 * Width(struct pair));
  two w = ini_two((two) hp, sp[0], sp[1]),
      x = ini_two(w + 1, (word) w, f->macro);
  hp += 2 * Width(struct pair);
  f->macro = (word) x;
  return O(2, sp[1]); }

Vm(ret) {
  word r = getnum(ip[1].x) + 1;
  return O(r, *sp); }

Vm(ap) {
  if (nump(sp[1])) return Do(sp++, ip++);
  thread k = (thread) sp[1];
  return Do(sp[1] = (word) (ip + 1), ip = k); }

Vm(tap) {
  word x = sp[0], j = sp[1];
  sp += getnum(ip[1].x) + 1;
  return nump(j) ? O(1, j) :
    Do(ip = (thread) j, *sp = x); }

Vm(apn) {
  size_t n = getnum(ip[1].x);
  word r = (word) (ip + 2); // return address
  ip = (thread) sp[n]; // only used by let form so will not be num
  sp[n] = r; // store return address
  // do this at compile time
  if (ip->ap == cur && getnum(ip[1].x) == n) ip += 2;
  return ip->ap(f, ip, hp, sp); }

Vm(tapn) {
  size_t n = getnum(ip[1].x),
         r = getnum(ip[2].x);
  // won't be num since is only emitted for let
  ip = (thread) sp[n];
  // do this at compile time
  if (ip->ap == cur && getnum(ip[1].x) == n) ip += 2;
  // generalize to other cases ...
  stack osp = sp;
  sp += r + 1;
  while (n--) sp[n] = osp[n];
  return ip->ap(f, ip, hp, sp); }

static Vm(Kj) { Have1(); return
  *--sp = ip[1].x,
  ip += 2,
  ip->m->ap(f, ip->m, hp, sp); }

Vm(cur) {
  thread k;
  size_t n = getnum(ip[1].x),
         S = 3 + Width(struct tag);
  if (n == 2) {
    Have(S);
    k = (thread) hp;
    k[0].ap = Kj, k[1].x = *sp++, k[2].m = ip + 2;
    k[3].x = 0,   k[4].m = k; }
  else {
    S += 2;
    Have(S);
    k = (thread) hp;
    k[0].ap = cur, k[1].x = putnum(n - 1);
    k[2].ap = Kj,  k[3].x = *sp++, k[4].m = ip + 2;
    k[5].x = 0,    k[6].m = k; }
  return Do(hp += S, ip = (cell) *sp, *sp = (word) k); }

Vm(data) {
  word r = (word) ip;
  return O(1, r); }

Vm(K) {
  Have1();
  return Do(*--sp = ip[1].x, ip += 2); }

Vm(prc) {
  return Do(ip = (thread) sp[1], fputc(getnum(sp[1] = sp[0]), stdout), sp++); }

Vm(print) {
  transmit(f, stdout, *sp), puts("");
  return op(1, *sp); }

Vm(cons) {
  Have(Width(struct pair));
  two w = ini_two((two) hp, sp[0], sp[1]);
  hp += Width(struct pair);
  return op(2, (word) w); }

Vm(car) { return op(1, twop(sp[0]) ? A(sp[0]) : sp[0]); }
Vm(cdr) { return op(1, twop(sp[0]) ? B(sp[0]) : sp[0]); }
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

Vm(trim) {
  thread k = (thread) sp[0];
  ttag(k)->head = k;
  return op(1, (word) k); }

Vm(seek) {
  thread k = (thread) sp[1];
  return op(2, (word) (k + getnum(sp[0]))); }

Vm(peek) {
  thread k = (thread) sp[0];
  return op(1, k[0].x); }

Vm(poke) {
  thread k = (thread) sp[1];
  k->x = sp[0];
  return op(2, (word) k); }

Vm(thda) {
  size_t n = getnum(sp[0]);
  Have(n + Width(struct tag));
  thread k = mo_ini(memset(hp, -1, n * sizeof(word)), n);
  hp += n + Width(struct tag);
  return op(1, (word) k); }

#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)
NoInline Vm(gc, size_t n) {
  return Pack(), !please(f, n) ? Oom :
    f->ip->ap(f, f->ip, f->hp, f->sp); }

Vm(ref) { Have1(); return Do(sp[-1] = sp[getnum(ip[1].x)], sp--, ip += 2); }
Vm(cond) { return Do(ip = nilp(*sp) ? ip[1].m : ip + 2, sp++); }
Vm(jump) { return Do(ip = ip[1].m); }
Vm(yield) { return Pack(), Ok; }
