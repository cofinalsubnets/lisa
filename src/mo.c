#include "la.h"
#include "alloc.h"
#include "mo.h"
#include "two.h"
#include <string.h>

// function functions
//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|?|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
//
// two easy potential optimizations are:
// - add a tail pointer to the start of the function,
//   so GC can find the head quickly (since often we
//   won't have an internal pointer)
// - tag the tail/head pointers instead of using a null
//   sentinel (but then the C compiler would need to
//   align functions)

// allocate a thread
la_fn mkmo(la_carrier v, size_t n) {
  la_fn k = cells(v, n + wsizeof(struct tag));
  return k ? ini_mo(k, n) : k; }

// get the tag at the end of a function
la_fn_tag motag(la_fn k) {
  while (G(k)) k = F(k);
  return (la_fn_tag) k; }

// instructions for the internal compiler
#include "vm.h"
// initialize a function
Vm(hom_f) {
  ArityCheck(1);
  size_t len = getnum(fp->argv[0]);
  Have(len + wsizeof(struct tag));
  mo k = setw(ini_mo(hp, len), nil, len);
  hp += len + wsizeof(struct tag);
  return ApC(ret, (ob) (k + len)); }

// trim a function after writing out code
Vm(hfin_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(homp(xp) && G(xp) != disp);
  motag((mo) xp)->head = (mo) xp;
  return ApC(ret, xp); }

// emit data
Vm(poke_f) {
  ArityCheck(2);
  Check(homp(fp->argv[1]));
  mo k = (mo) fp->argv[1] - 1;
  G(k) = (vm*) fp->argv[0];
  return ApC(ret, (ob) k); }

// frameless
Vm(poke) {
  mo k = (mo) *sp++ - 1;
  G(k) = (vm*) xp;
  return ApN(1, (ob) k); }

// read data from a thread (be sure it's really data!)
Vm(peekx_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(homp(xp));
  return ApC(ret, (ob) G(xp)); }

// thread pointer arithmetic -- not bounds checked!
Vm(seek_f) {
  ArityCheck(2);
  Check(homp(fp->argv[0]));
  ip = (mo) fp->argv[0];
  xp = getnum(fp->argv[1]);
  return ApC(ret, (ob) (ip + xp)); }

// dispatch a data thread
// TODO maybe we could do this with closures instead?
Vm(disp) { return ApC(((mtbl) GF(ip))->does, xp); }

// closure functions
//
// pop some things off the stack into an array.
Vm(take) {
  ob n = getnum((ob) GF(ip));
  Have(n + wsizeof(struct tag));
  mo k = ini_mo(cpyw_r2l(hp, sp, n), n);
  hp += n + wsizeof(struct tag);
  return ApN(2, (ob) k); }

// set the closure for this frame
static Vm(setclo) { return
  fp->clos = (ob*) GF(ip),
  ApY(G(FF(ip)), xp); }

// finalize function instance closure
static Vm(genclo1) { return
  G(ip) = setclo,
  GF(ip) = (vm*) xp,
  ApY(ip, xp); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
static Vm(genclo0) {
  ob *ec = (ob*) GF(ip);
  size_t adic = 0;
  while (ec[3 + adic]) adic++;
  Have(wsizeof(struct sf) + adic + 1);
  ob loc = ec[1];
  sf subd = fp;
  G(ip) = genclo1;
  sp = (ob*) (fp = (sf) (sp - adic) - 1);
  cpyw_r2l(fp->argv, ec + 3, adic);
  fp->retp = ip;
  fp->subd = subd;
  fp->argc = adic;
  fp->clos = (ob*) ec[2];
  if (!nilp(loc)) *--sp = loc;
  return ApY(ec[0], xp); }

// the next few functions create and store
// lexical environments.
static Vm(enclose) {
  size_t
    adic = fp->argc,
    thd_len = 3 + wsizeof(struct tag),
    env_len = 3 + adic + wsizeof(struct tag),
    n =  env_len + thd_len;
  Have(n);
  ob codeXcons = (ob) GF(ip), // pair of the compiled thread & closure constructor
     *block = hp;
  hp += n;

  ob *env = (ob*) ini_mo(block, 3 + adic); // holds the closure environment & constructor
  block += env_len;
  cpyw_r2l(env + 3, fp->argv, adic);

  ob *thd = (ob*) ini_mo(block, 3), // the thread that actually gets returned
     // TODO get closure out of stack frame; configure via xp
     loc = nilp(xp) ? xp : ((ob*)fp)[-1],
     clo = (ob) fp->clos;

  env[0] = B(codeXcons);
  env[1] = loc;
  env[2] = clo;

  thd[0] = (ob) genclo0;
  thd[1] = (ob) env;
  thd[2] = A(codeXcons);

  return ApN(2, (ob) thd); }

// these pass the locals array to encl in xp
// TODO do the same thing with the closure ptr
Vm(encl1) { return ApC(enclose, putnum(1)); }
// FIXME if there are no locals we don't need to defer closure construction!
Vm(encl0) { return ApC(enclose, putnum(0)); }

// try to get the name of a function
ob hnom(la v, mo x) {
  if (!livep(v, (ob) x)) return nil;
  vm *k = G(x);
  if (k == setclo || k == genclo0 || k == genclo1)
    return hnom(v, (mo) G(FF(x)));
  ob n = ((ob*) motag(x))[-1];
  return livep(v, n) ? n : nil; }
