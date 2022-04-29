#include "lips.h"
#include "terp.h"
#include "mem.h"

// " the virtual machine "
// It's a stack machine with one free register that runs on
// top of the C compiler's calling convention. This lets us
// keep the main state variables in CPU registers without
// needing to write any platform specific code, as long as
// the compiler passes enough arguments in registers and
// optimizes tail calls.

// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function

// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.

Vm(ap_u) {
  Arity(2);
  obj x = Argv[0], y = Argv[1];
  TypeCheck(x, Hom);
  u64 adic = llen(y);
  Have(adic);
  obj off = Subr, rp = Retp;
  sp = Argv + N(Argc) - adic;
  for (u64 j = 0; j < adic; y = B(y)) sp[j++] = A(y);
  fp = sp -= Width(frame);
  Retp = rp;
  Argc = _N(adic);
  Subr = off;
  Clos = nil;
  Ap(x, nil); }

// continuations
Vm(ccc_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  // we need space for:
  // the entire stack
  // the frame offset
  // the length (to put it all in a tuple)
  // the continuation thread (4 words)
  u64 depth = v->pool + v->len - sp;
  Have(depth + 6);
  ip = *Argv;
  vec t = (vec) hp;
  hp += depth + 2;
  t->len = depth + 1;
  t->xs[0] = _N(fp - sp);
  cpy64(t->xs+1, sp, depth);
  hom c = (hom) hp;
  hp += 4;
  c[0] = cont;
  c[1] = (terp*) _V(t);
  c[2] = NULL;
  c[3] = (terp*) c;
  *Argv = _H(c);
  Ap(ip, nil); }

// call a continuation
Vm(cont) {
 vec t = V((obj) H(ip)[1]);
 Have(t->len - 1);
 xp = N(Argc) == 0 ? nil : *Argv;
 i64 off = N(t->xs[0]);
 sp = v->pool + v->len - (t->len - 1);
 fp = sp + off;
 cpy64(sp, t->xs+1, t->len-1);
 Jump(ret); }

Vm(vararg) {
  i64 reqd = N((i64) H(ip)[1]),
      vdic = N(Argc) - reqd;
  Arity(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) {
    Have1();
    cpy64(fp-1, fp, Width(frame) + N(Argc));
    sp = --fp;
    Argc += Word;
    Argv[reqd] = nil;
    Next(2); }
  // in this case we just keep the existing slots.
  // the path is knowable at compile time in many cases
  // so maybe vararg should be two or more different
  // functions.
  else {
    Have(2 * vdic);
    two t = (two) hp;
    hp += 2 * vdic;
    for (i64 i = vdic; i--;
      t[i].a = Argv[reqd + i],
      t[i].b = puttwo(t+i+1));
    t[vdic-1].b = nil;
    Argv[reqd] = puttwo(t);
    Next(2); } }

// type predicates
#define Tp(t)\
  Vm(t##pp) { Ap(ip+Word, (t##p(xp)?ok:nil)); }\
  Vm(t##p_u) {\
    for (obj *xs = Argv, *l = xs + N(Argc); xs < l;)\
      if (!t##p(*xs++)) Go(ret, nil);\
    Go(ret, ok); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(vec) Tp(nil)

// stack manipulation
Vm(dupl) { Have1(); --sp; sp[0] = sp[1]; Next(1); }

static terp recne;

////
/// Branch Instructions
//
// unconditional jump
Vm(jump) { Ap((obj) H(ip)[1], xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Vm(nom) {\
  if (test) Ap((obj)a(H(ip)),x);\
  else Ap((obj)b(H(ip)),y); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq,  eql(*sp++, xp), GF, ok, FF, nil)
Br(brne,  eql(*sp++, xp), FF, ok, GF, nil)

Br(brlt,    *sp++ < xp,  GF, xp, FF, nil)
Br(brlt2,   *sp++ < xp,  FF, xp, GF, nil)
Br(brlteq,  *sp++ <= xp, GF, xp, FF, nil)
Br(brlteq2, *sp++ <= xp, FF, xp, GF, nil)
Br(brgt,    *sp++ > xp,  GF, xp, FF, nil)
Br(brgt2,   *sp++ > xp,  FF, xp, GF, nil)
Br(brgteq,  *sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt
#undef Br

// return from a function
Vm(ret) {
 ip = Retp;
 sp = (mem) ((i64) Argv + Argc - Num);
 fp = (mem) ((i64)   sp + Subr - Num);
 Next(0); }

// "inner" function call
Vm(call) {
 Have(Width(frame));
 obj adic = (obj) H(ip)[1];
 i64 off = fp - (mem) ((i64) sp + adic - Num);
 fp = sp -= Width(frame);
 Retp = ip + 2 * word;
 Subr = _N(off);
 Clos = nil;
 Argc = adic;
 Ap(xp, nil); }

// tail call
Vm(rec) {
 if (Argc != (ip = (obj) H(ip)[1])) Jump(recne);
 cpy64(Argv, sp, ip = getnum(ip));
 sp = fp;
 Ap(xp, nil); }

// tail call with different arity
static Vm(recne) {
 v->xp = Subr, v->ip = Retp; // save return info
 fp = Argv + N(Argc - ip);
 cpy64r(fp, sp, N(ip)); // copy from high to low
 sp = fp -= Width(frame);
 Retp = v->ip;
 Argc = ip;
 Subr = v->xp;
 ip = xp;
 Clos = xp = nil;
 Next(0); }
