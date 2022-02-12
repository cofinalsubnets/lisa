#include "lips.h"
#include "terp.h"
#include "mem.h"
#include "write.h"
#include "hom.h"
#include "tbl.h"
#include "cmp.h"
#include "sym.h"
#include "two.h"
#include "str.h"
#include "vec.h"
// " the virtual machine "
// it's a stack machine with one free register that's
// implemented on top of the C compiler's calling convention.
// this allows us to keep the most important state variables
// in CPU registers at all times while the interpreter is
// running, without any platform-specific code.

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

// jump to nope() when an error happens.

// " virtual machine instructions "
//
// load instructions
OP2(imm, (obj) H(ip)[1])
OP1(unit, nil)
OP1(one, _N(1))
OP1(zero, _N(0))

// indexed load instructions
// this pointer arithmetic works because fixnums are
// premultiplied by W
#define REF(b) (*(i64*)((i64)(b)+(i64)H(ip)[1]-Num))

OP2(arg, REF(Argv))
OP1(arg0, Argv[0])
OP1(arg1, Argv[1])
OP2(loc, REF(V(Locs)->xs))
OP1(loc0, V(Locs)->xs[0])
OP1(loc1, V(Locs)->xs[1])
OP2(clo, REF(V(Clos)->xs))
OP1(clo0, V(Clos)->xs[0])
OP1(clo1, V(Clos)->xs[1])

// store instructions
Vm(push) { Have1(); *--sp = xp; Next(1); } // stack push
Vm(loc_) { REF(V(Locs)->xs) = xp; Next(2); } // set a local variable

Vm(tbind) { CallC(tbl_set(v, Top, (obj) H(ip)[1], xp)); Next(2); }

// initialize local variable slots
Vm(locals) {
 i64 n = N((i64) H(ip)[1]);
 Have(n + 2);
 vec t = (vec) hp;
 set64(t->xs, nil, t->len = n);
 hp += n + 1;
 *--sp = _V(t);
 Next(2); }

// late bind
// this function is a lil complicated, because it incorporates
// the "static" type and arity checking that would have been
// done by the compiler if the function had been bound early.
Vm(lbind) {
 obj w = (obj) H(ip)[1], d = AB(w), y = A(w);
 if (!(w = tbl_get(v, d, xp = BB(w)))) {
  char *nom = nilp(Y(xp)->nom) ? "()" : S(Y(xp)->nom)->text;
  Pack();
  errp(v, "free variable: %s", nom);
  return restart(v); }
 xp = w;
 if (getnum(y) != 8) Tc(xp, getnum(y)); // do the type check
 terp *q = H(ip)[2]; // omit the arity check if possible
 if (q == call || q == rec) {
  obj aa = (obj) H(ip)[3];
  if (H(xp)[0] == arity && aa >= (obj) H(xp)[1]) xp += W2; }
 H(ip)[0] = imm;
 H(ip)[1] = (terp*) xp;
 Next(2); }

// return to C
Vm(yield) { Pack(); return xp; }

#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))

// branches
Vm(jump) { Ap((obj) H(ip)[1], xp); }
// test, yes addr, yes val, no addr, no val
#define Br(test, a, x, b, y) {\
  if (test) Ap((obj)a(H(ip)),x);\
  else Ap((obj)b(H(ip)),y); }

Vm(branch)  Br(xp != nil, GF, xp, FF, xp)
Vm(barnch)  Br(xp == nil, GF, xp, FF, xp)

Vm(breq)    Br(eql(*sp++, xp), GF, ok, FF, nil)
Vm(brne)    Br(eql(*sp++, xp), FF, ok, GF, nil)

Vm(brlt)    Br(*sp++ < xp,  GF, xp,  FF, nil)
Vm(brlt2)   Br(*sp++ < xp,  FF, xp,  GF, nil)

Vm(brlteq)  Br(*sp++ <= xp, GF, xp,  FF, nil)
Vm(brlteq2) Br(*sp++ <= xp, FF, xp,  GF, nil)

Vm(brgt)    Br(*sp++ > xp, GF, xp, FF, nil)
Vm(brgt2)   Br(*sp++ > xp, FF, xp, GF, nil)

Vm(brgteq)  Br(*sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt
#undef Br

// return from a function
Vm(ret) {
 ip = Retp;
 sp = (mem) ((i64) Argv + Argc - Num);
 fp = (mem) ((i64)   sp + Subr - Num);
 Next(0); }

// regular function call
Vm(call) {
 Have(Width(frame));
 obj adic = (obj) H(ip)[1];
 i64 off = fp - (mem) ((i64) sp + adic - Num);
 fp = sp -= Width(frame);
 Retp = ip + W2;
 Subr = _N(off);
 Clos = nil;
 Argc = adic;
 Ap(xp, nil); }

Vm(ap_u) {
 Ary(2);
 obj x = Argv[0],
     y = Argv[1];
 Tc(x, Hom);
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

static Vm(recne) {
 // overwrite current frame with new frame
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

// tail call
Vm(rec) {
 if (Argc != (ip = (obj) H(ip)[1])) Jump(recne);
 cpy64(Argv, sp, ip = getnum(ip));
 sp = fp;
 Ap(xp, nil); }

// continuations
Vm(ccc_u) {
 Ary(1);
 Tc(*Argv, Hom);
 // we need space for:
 // the entire stack
 // the frame offset
 // the length (to put it all in a tuple)
 // the continuation thread (4 words)
 i64 depth = v->pool + v->len - sp;
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
 Argv[0] = _H(c);
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
 Ary(reqd);
 // in this case we need to add another argument
 // slot to hold the nil.
 if (!vdic) {
  Have1();
  cpy64(fp-1, fp, Width(frame) + N(Argc));
  sp = --fp;
  Argc += W;
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
#define Tp(t) \
    Vm(t##pp) { Ap(ip+W, (t##p(xp)?ok:nil)); }\
    Vm(t##p_u) {\
      for (obj *xs = Argv, *l = xs + N(Argc); xs < l;)\
        if (!t##p(*xs++)) Go(ret, nil);\
      Go(ret, ok); }
Tp(num) Tp(hom) Tp(two) Tp(sym)
Tp(str) Tp(tbl) Tp(vec) Tp(nil)

// stack manipulation
Vm(dupl) { Have1(); --sp; sp[0] = sp[1]; Next(1); }

obj restart(lips v) {
  v->fp = v->sp = v->pool + v->len;
  v->xp = v->ip = nil;
  v->root = NULL;
  longjmp(v->restart, 1); }

u0 errp(lips v, const char *msg, ...) {
  obj ip = v->ip;
  mem fp = v->fp;
  // print current call as (function arg1 arg2 ...)
  fputs("# (", stderr);
  emit(v, stderr, ip);
  mem top = v->pool + v->len;
  i64 i = 0, argc = fp == top ? 0 : N(Argc);
  if (argc) for (fputc(' ', stderr);; fputc(' ', stderr)) {
    obj x = Argv[i++];
    emit(v, stderr, x);
    if (i == argc) break; }
  fputc(')', stderr);

  // print error message
  if (msg) {
    fputs(" : ", stderr);
    va_list xs;
    va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs); }

  fputc('\n', stderr);

  // print backtrace
  for (mem top = v->pool + v->len; fp < top;) {
    ip = Retp, fp += Width(frame) + N(Argc) + N(Subr);
    if (button(H(ip))[-1] == yield) break;
    fputs("# in ", stderr), emsep(v, ip, stderr, '\n'); } }

Vm(gc) {
  u64 req = v->xp;
  CallC(req = cycle(v, req));
  if (req) Next(0);
  errp(v, oom_err_msg, v->len, req);
  return restart(v); }

// errors
Vm(fail) {
  if (homp(Re)) Ap(Re, xp);
  Pack();
  errp(v, NULL);
  return restart(v); }

Vm(type_error) {
  if (homp(Re)) Ap(Re, xp);
  enum tag exp = v->xp, act = kind(xp);
  Pack();
  errp(v, "wrong type : %s for %s", tnom(act), tnom(exp));
  return restart(v); }

Vm(oob_error) {
  if (homp(Re)) Ap(Re, xp);
  i64 a = v->xp, b = v->ip;
  Pack();
  errp(v, "oob : %d >= %d", a, b);
  return restart(v); }

Vm(ary_error) {
  if (homp(Re)) Ap(Re, xp);
  i64 a = N(Argc), b = v->xp;
  Pack();
  errp(v, arity_err_msg, a, b);
  return restart(v); }

Vm(div_error) {
  if (homp(Re)) Ap(Re, xp);
  Pack();
  errp(v, "/ 0");
  return restart(v); }

// type/arity checking
#define DTc(n, t) Vm(n) {\
  if (kind(xp-t)==0) Next(1);\
  v->xp = t; Jump(type_error); }
DTc(idZ, Num) DTc(idH, Hom) DTc(idT, Tbl) DTc(id2, Two)
Vm(arity) {
  obj reqd = (obj) H(ip)[1];
  if (reqd <= Argc) Next(2);
  else Jump((v->xp = N(reqd), ary_error)); }
