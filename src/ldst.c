#include "la.h"

////
/// Load Instructions
//

// constants
Vm(one) { return ApN(1, putnum(1)); }
Vm(zero) { return ApN(1, putnum(0)); }
// immediate value from thread
Vm(imm) { return ApN(2, (ob) GF(ip)); }

// function arguments
Vm(argn) { return ApN(2, fp->argv[getnum(GF(ip))]); }
Vm(arg0) { return ApN(1, fp->argv[0]); }
Vm(arg1) { return ApN(1, fp->argv[1]); }
Vm(arg2) { return ApN(1, fp->argv[2]); }
Vm(arg3) { return ApN(1, fp->argv[3]); }

// the first two stack slots under the current frame
// may hold extra call data.
#define Slot1 ((ob**)fp)[-1]
#define Slot2 ((ob**)fp)[-2]

// local variables
Vm(locn) { return ApN(2, Slot1[getnum(GF(ip))]); }
Vm(loc0) { return ApN(1, Slot1[0]); }
Vm(loc1) { return ApN(1, Slot1[1]); }
Vm(loc2) { return ApN(1, Slot1[2]); }
Vm(loc3) { return ApN(1, Slot1[3]); }

// closure variables
Vm(clon) { return ApN(2, fp->clos[getnum(GF(ip))]); }
Vm(clo0) { return ApN(1, fp->clos[0]); }
Vm(clo1) { return ApN(1, fp->clos[1]); }
Vm(clo2) { return ApN(1, fp->clos[2]); }
Vm(clo3) { return ApN(1, fp->clos[3]); }

////
/// Store Instructions
// // stack push
Vm(push) {
  Have1();
  *--sp = xp;
  return ApN(1, xp); }

// dup top of stack
Vm(dupl) {
  Have1();
  --sp;
  sp[0] = sp[1];
  return ApN(1, xp); }

// set a local variable
Vm(defloc) { return
  Slot1[getnum(GF(ip))] = xp,
  ApN(2, xp); }

// set a module variable
Vm(deftop) {
  ob a = (ob) GF(ip);
  CallOut(a = tbl_set(v, (tbl) A(a), B(a), xp));
  return a ? ApN(2, a) : ApC(xoom, xp); }

// allocate local variable array
Vm(setloc) {
  size_t n = getnum((ob) GF(ip));
  // + 1 for the stack slot
  Have(n + Width(tag) + 1);
  ob *t = setw(hp, nil, n);
  hp += n + Width(tag) + 1;
  t[n] = 0;
  *--sp = t[n+1] = (ob) t;
  return ApN(2, xp); }

static NoInline Vm(nom_err) {
  Pack();
  const char *n = "#sym";
  size_t l = 4;
  str s = ((sym) xp)->nom;
  if (s) n = s->text, l = s->len;
  return nope(v, "free variable : %.*s", l, n); }

// late binding
// TODO dynamic type checking here
Vm(late) {
  ob w = (ob) GF(ip), d = A(w);
  xp = B(w);
  w = tbl_get(v, (tbl) d, xp);
  if (!w) return ApC(nom_err, xp);
  xp = w;
  // omit the arity check if possible
  vm *n = G(FF(ip));
  if ((n == call || n == rec) && // xp will be a hom
      G(xp) == arity &&
      GF(FF(ip)) >= GF(xp))
    xp = (ob) FF(ip);
  G(ip) = imm;
  GF(ip) = (vm*) xp;
  return ApN(2, xp); }

// varargs
static NoInline Vm(varg0) {
  size_t reqd = getnum((ob) GF(ip));
  Have1();
  fp = cpyw((ob*) fp - 1, fp, Width(sf) + fp->argc);
  sp = (ob*) fp;
  fp->argc++;
  fp->argv[reqd] = nil;
  return ApN(2, xp); }

Vm(varg) {
  size_t reqd = getnum((ob) GF(ip)),
         vdic = fp->argc - reqd;
  ArityCheck(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) return ApC(varg0, xp);
  // in this case we just keep the existing slots.
  Have(Width(two) * vdic);
  two t = (two) hp;
  hp += Width(two) * vdic;
  for (size_t i = vdic; i--;
    ini_two(t + i, fp->argv[reqd + i], (ob) (t + i + 1)));
  t[vdic-1].b = nil;
  fp->argv[reqd] = (ob) t;
  return ApN(2, xp); }
