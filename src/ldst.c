#include "la.h"
#include "vm.h"

////
/// Load Instructions
//

// constants
Vm(one) { return ApN(1, putnum(1)); }
Vm(zero) { return ApN(1, putnum(0)); }
// immediate value from thread
Vm(imm) { return
  xp = (ob) GF(ip),
  ApN(2, xp); }

// function arguments
Vm(arg) { return
  xp = fp->argv[getnum(GF(ip))],
  ApN(2, xp); }
Vm(arg0) { return ApN(1, fp->argv[0]); }
Vm(arg1) { return ApN(1, fp->argv[1]); }

// local variables
Vm(loc) { return
  xp = Locs[getnum(GF(ip))],
  ApN(2, xp); }
Vm(loc0) { return ApN(1, Locs[0]); }
Vm(loc1) { return ApN(1, Locs[1]); }

// closure variables
Vm(clo) { return
  xp = ((ob*) fp->clos)[getnum(GF(ip))],
  ApN(2, xp); }
Vm(clo0) { return ApN(1, ((ob*) fp->clos)[0]); }
Vm(clo1) { return ApN(1, ((ob*) fp->clos)[1]); }

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
Vm(loc_) { return
  Locs[getnum(GF(ip))] = xp,
  ApN(2, xp); }

// set a module variable
Vm(tbind) {
  ob a = (ob) GF(ip);
  CallOut(v->xp = tbl_set(v, A(a), B(a), xp));
  return xp ? ApN(2, xp) : ApC(oom_err, xp); }

// allocate local variable array
Vm(locals) {
  ob *t = hp, n = getnum((ob) GF(ip));
  // n + 2 for the vector thread + 1 for the stack slot
  Have(n + 3);
  hp += n + 3;
  setw(t, nil, n);
  t[n] = 0;
  *--sp = t[n+1] = (ob) t;
  return ApN(2, xp); }

static NoInline Vm(nom_err) {
  return Pack(), nope(v,
    "referenced free variable `%s'",
    nilp(xp) ? 0 : ((str) xp)->text); }
// late binding
// TODO dynamic type checking here
Vm(late) {
  ob w = (ob) GF(ip), d = A(w);
  xp = B(w);
  w = tbl_get(v, d, xp);
  if (!w) return ApC(nom_err, ((sym)xp)->nom);
  xp = w;
  // omit the arity check if possible
  vm *n = G(FF(ip));
  if ((n == call || n == rec) && // xp will be a hom
      ((ob*) xp)[0] == (ob) arity &&
      ((ob*) ip)[3] >= ((ob*) xp)[1])
    xp = (ob) ((ob*) xp + 2);
  G(ip) = imm;
  GF(ip) = (vm*) xp;
  return ApN(2, xp); }

// varargs
static NoInline Vm(varg0) {
  size_t reqd = getnum((ob) GF(ip));
  Have1();
  cpyw((ob*) fp - 1, fp, Width(fr) + getnum(fp->argc));
  fp = (fr) ((ob*) fp - 1);
  sp = (ob*) fp;
  fp->argc += 2; // 1 << 1
  fp->argv[reqd] = nil;
  return ApN(2, xp); }

Vm(varg) {
  size_t reqd = getnum((ob) GF(ip)),
         vdic = getnum(fp->argc) - reqd;
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
  t[vdic-1].b = nil,
  fp->argv[reqd] = (ob) t;
  return ApN(2, xp); }
