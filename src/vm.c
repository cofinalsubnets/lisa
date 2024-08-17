#include "i.h"

#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)
Vm(mbind) {
  Have(2 * Width(struct two));
  two w = ini_two((two) hp, sp[0], sp[1]),
      x = ini_two(w + 1, (word) w, f->macro);
  f->macro = (word) x;
  ip = (thread) sp[2];
  sp[2] = sp[1];
  return ip->ap(f, ip, hp + 2 * Width(struct two), sp + 2); }

Vm(ret) {
  word r = *sp;
  sp += getnum(ip[1].x) + 1,
  ip = (cell) *sp,
  *sp = r;
  return ip->ap(f, ip, hp, sp); }

Vm(ap) {
  if (nump(sp[1])) return
    ip[1].ap(f, ip + 1, hp, sp + 1);
  thread k = (thread) sp[1];
  sp[1] = (word) (ip + 1);
  return k->ap(f, k, hp, sp); }

Vm(apn) {
  size_t n = getnum(ip[1].x);
  word r = (word) (ip + 2); // return address
  ip = (thread) sp[n]; // only used by let form so will not be num
  sp[n] = r; // store return address
  // do this at compile time
  if (ip->ap == cur && getnum(ip[1].x) == n) ip += 2;
  return ip->ap(f, ip, hp, sp); }

Vm(tap) {
  //puts("tap");
  word x = sp[0], j = sp[1];
  sp += getnum(ip[1].x) + 1;
  if (nump(j)) ip = (thread) *++sp, *sp = j;
  else ip = (thread) j, *sp = x;
  return ip->ap(f, ip, hp, sp); }

Vm(tapn) {
  size_t n = getnum(ip[1].x),
         r = getnum(ip[2].x);
  // won't be num since is only emitted for let
  ip = (thread) sp[n];
  // do this at compile time
  if (ip->ap == cur && getnum(ip[1].x) == n) ip += 2;
  // generalize to other cases ...
  stack arg = sp;
  for (sp += r + 1; n--; sp[n] = arg[n]);
  return ip->ap(f, ip, hp, sp); }

static Vm(Kj) {
  Have1();
  sp[-1] = ip[1].x;
  return ip[2].m->ap(f, ip[2].m, hp, sp - 1); }

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
  ip = (cell) *sp, *sp = (word) k;
  return ip->ap(f, ip, hp + S, sp); }

Vm(data) {
  word r = (word) ip;
  ip = (cell) *++sp;
  *sp = r;
  return ip->ap(f, ip, hp, sp); }

Vm(K) { Have1(); return
  sp[-1] = ip[1].x,
  ip[2].ap(f, ip + 2, hp, sp - 1); }

Vm(prc) {
  ip = (thread) sp[1];
  fputc(getnum(sp[1] = sp[0]), stdout);
  return ip->ap(f, ip, hp, sp + 1); }

Vm(print) { return
  ip = (void*) sp[1],
  sp[1] = *sp,
  transmit(f, stdout, *sp),
  puts(""),
  ip->ap(f, ip, hp, sp + 1); }

Vm(cons) {
  Have(Width(struct two));
  two w = ini_two((two) hp, sp[0], sp[1]);
  return ip = (thread) sp[2],
         sp[2] = (word) w,
         ip->ap(f, ip, hp + Width(struct two), sp + 2); }

Vm(car) { return
  ip = (thread) sp[1],
  sp[1] = twop(sp[0]) ? A(sp[0]) : sp[0],
  ip->ap(f, ip, hp, sp + 1); }

Vm(cdr) { return
  ip = (thread) sp[1],
  sp[1] = twop(sp[0]) ? B(sp[0]) : nil,
  ip->ap(f, ip, hp, sp + 1); }

#define binop(n, x) Vm(n) { return\
  ip = (void*) sp[2],\
  sp[2] = x,\
  ip->ap(f, ip, hp, sp + 2); }

binop(add, putnum(getnum(sp[0])+getnum(sp[1])))
binop(sub, putnum(getnum(sp[0])-getnum(sp[1])))
binop(mul, putnum(getnum(sp[0])*getnum(sp[1])))
binop(quot, nilp(sp[1]) ? nil : putnum(getnum(sp[0])/getnum(sp[1])))
binop(rem, nilp(sp[1]) ? nil : putnum(getnum(sp[0])%getnum(sp[1])))
binop(eq, eql(f, sp[0], sp[1]) ? putnum(-1) : nil)
binop(lt, sp[0] < sp[1] ? putnum(-1) : nil)
binop(le, sp[0] <= sp[1] ? putnum(-1) : nil)
binop(gt, sp[0] > sp[1] ? putnum(-1) : nil)
binop(ge, sp[0] >= sp[1] ? putnum(-1) : nil)

Vm(not) { return
  ip = (thread) sp[1],
  sp[1] = ~sp[0] | 1,
  ip->ap(f, ip, hp, sp + 1); }

Vm(trim) {
  thread k = (thread) sp[0];
  ttag(k)->head = k;
  ip = (thread) sp[1];
  sp[1] = (word) k;
  return ip->ap(f, ip, hp, sp + 1); }

Vm(seek) {
  thread k = (thread) sp[1];
  ip = (thread) sp[2];
  sp[2] = (word) (k + getnum(sp[0]));
  return ip->ap(f, ip, hp, sp + 2); }

Vm(peek) {
  thread k = (thread) sp[0];
  ip = (thread) sp[1];
  sp[1] = k[0].x;
  return ip->ap(f, ip, hp, sp + 1); }

Vm(poke) {
  thread k = (thread) sp[1];
  k->x = sp[0];
  ip = (thread) sp[2];
  sp[2] = (word) k;
  return ip->ap(f, ip, hp, sp + 2); }

Vm(thda) {
  size_t n = getnum(sp[0]);
  Have(n + Width(struct tag));
  thread k = mo_ini(memset(hp, -1, n * sizeof(word)), n);
  hp += n + Width(struct tag);
  ip = (thread) sp[1];
  sp[1] = (word) k;
  return ip->ap(f, ip, hp, sp + 1); }

#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)
NoInline Vm(gc, size_t n) {
  return Pack(), !please(f, n) ? Oom :
    f->ip->ap(f, f->ip, f->hp, f->sp); }

Vm(ref) {
  Have1();
  sp[-1] = sp[getnum(ip[1].x)];
  return ip[2].ap(f, ip + 2, hp, sp - 1); }

Vm(cond) { return
  ip = nilp(*sp) ? ip[1].m : ip + 2,
  ip->ap(f, ip, hp, sp + 1); }

Vm(jump) { return ip[1].m->ap(f, ip[1].m, hp, sp); }

Vm(yield) { return Pack(), Ok; }
