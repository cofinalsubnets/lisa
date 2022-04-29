#include "lips.h"
#include "terp.h"

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
  ob x = Argv[0], y = Argv[1];
  TypeCheck(x, Hom);
  u64 adic = llen(y);
  Have(adic);
  ob off = Subr, rp = Retp;
  sp = Argv + N(Argc) - adic;
  for (u64 j = 0; j < adic; y = B(y)) sp[j++] = A(y);
  fp = sp -= Width(fr);
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
  yo c = (yo) hp;
  hp += 4;
  c[0].ll = (vm*) cont;
  c[1].ll = (vm*) _V(t);
  c[2].ll = NULL;
  c[3].ll = (vm*) c;
  *Argv = (ob) c;
  Ap(ip, nil); }

// call a continuation
Vm(cont) {
  vec t = V((ob) H(ip)[1].ll);
  Have(t->len - 1);
  xp = N(Argc) == 0 ? nil : *Argv;
  i64 off = N(t->xs[0]);
  sp = v->pool + v->len - (t->len - 1);
  fp = sp + off;
  cpy64(sp, t->xs+1, t->len-1);
  Jump(ret); }

Vm(vararg) {
  i64 reqd = N((i64) H(ip)[1].ll),
      vdic = N(Argc) - reqd;
  Arity(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) {
    Have1();
    cpy64(fp-1, fp, Width(fr) + N(Argc));
    sp = --fp;
    Argc += Word;
    Argv[reqd] = nil;
    Next(2); }
  // in this case we just keep the existing slots.
  // the path is knowable at compile time in many cases
  // so maybe vararg should be two or more different
  // functions.
  Have(2 * vdic);
  two t = (two) hp;
  hp += 2 * vdic;
  for (i64 i = vdic; i--;
    t[i].a = Argv[reqd + i],
    t[i].b = puttwo(t+i+1));
  t[vdic-1].b = nil;
  Argv[reqd] = puttwo(t);
  Next(2); }

// type predicates
#define Tp(t)\
  Vm(t##pp) { Ap(ip+Word, (t##p(xp)?ok:nil)); }\
  Vm(t##p_u) {\
    for (ob *xs = Argv, *l = xs + N(Argc); xs < l;)\
      if (!t##p(*xs++)) Go(ret, nil);\
    Go(ret, ok); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(vec) Tp(nil)

// stack manipulation
Vm(dupl) { Have1(); --sp; sp[0] = sp[1]; Next(1); }

static vm recne;

////
/// Branch Instructions
//
// unconditional jump
Vm(jump) { Ap((ob) H(ip)[1].ll, xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Vm(nom) {\
  if (test) Ap((ob)a(H(ip)),x);\
  else Ap((ob)b(H(ip)),y); }
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
  sp = (ob*) ((i64) Argv + Argc - Num);
  fp = (ob*) ((i64)   sp + Subr - Num);
  Next(0); }

// "inner" function call
Vm(call) {
  Have(Width(fr));
  ob adic = (ob) H(ip)[1].ll;
  i64 off = fp - (ob*) ((i64) sp + adic - Num);
  fp = sp -= Width(fr);
  Retp = ip + 2 * word;
  Subr = _N(off);
  Clos = nil;
  Argc = adic;
  Ap(xp, nil); }

// tail call
Vm(rec) {
  if (Argc != (ip = (ob) H(ip)[1].ll)) Jump(recne);
  cpy64(Argv, sp, ip = getnum(ip));
  sp = fp;
  Ap(xp, nil); }

// tail call with different arity
static Vm(recne) {
 v->xp = Subr, v->ip = Retp; // save return info
 fp = Argv + N(Argc - ip);
 cpy64r(fp, sp, N(ip)); // copy from high to low
 sp = fp -= Width(fr);
 Retp = v->ip;
 Argc = ip;
 Subr = v->xp;
 ip = xp;
 Clos = xp = nil;
 Next(0); }

// errors
Vm(fail) { return Pack(), err(v, "fail"); }

#define type_err_msg "wrong type : %s for %s"
Vm(type_error) {
  class exp = v->xp, act = kind(xp);
  return Pack(), err(v, type_err_msg, tnom(act), tnom(exp)); }

Vm(oob_error) {
  i64 a = v->xp, b = v->ip;
  return Pack(), err(v, "oob : %d >= %d", a, b); }

#define arity_err_msg "wrong arity : %d of %d"
Vm(ary_error) {
  i64 a = N(Argc), b = v->xp;
  return Pack(), err(v, arity_err_msg, a, b); }

Vm(div_error) { return Pack(), err(v, "/ 0"); }

// type/arity checking
#define DTc(n, t) Vm(n) {\
  if (kind(xp-t)==0) Next(1);\
  v->xp = t; Jump(type_error); }
DTc(idZ, Num) DTc(idH, Hom) DTc(idT, Tbl) DTc(id2, Two)
Vm(arity) {
  ob reqd = (ob) H(ip)[1].ll;
  if (reqd <= Argc) Next(2);
  else Jump((v->xp = N(reqd), ary_error)); }

SI u0 show_call(en v, ob ip, ob* fp) {
  fputc('(', stderr);
  emit(v, ip, stderr);
  for (i64 i = 0, argc = N(Argc); i < argc;)
    fputc(' ', stderr), emit(v, Argv[i++], stderr);
  fputc(')', stderr); }

NoInline ob err(en v, const char *msg, ...) {
  ob ip = v->ip, *fp = v->fp,
     *top = v->pool + v->len;
  fputs("# ", stderr);
  if (fp < top) show_call(v, ip, fp), fputs(" : ", stderr);
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  fputc('\n', stderr);
  // print backtrace
  if (fp < top) for (;;) {
    ip = Retp, fp += Width(fr) + N(Argc) + N(Subr);
    if (fp == top) break;
    fputs("# in ", stderr);
    show_call(v, ip, fp);
    fputc('\n', stderr); }
  v->fp = v->sp = v->pool + v->len;
  v->xp = v->ip = nil;
  return 0; }
