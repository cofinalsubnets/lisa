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
VM(push) { Have1(); *--sp = xp; NEXT(1); } // stack push
VM(loc_) { REF(V(Locs)->xs) = xp; NEXT(2); } // set a local variable

VM(tbind) { CallC(tbl_set(v, Top, (obj) H(ip)[1], xp)); NEXT(2); }

// initialize local variable slots
VM(locals) {
 i64 n = N((i64) H(ip)[1]);
 Have(n + 2);
 vec t = (vec) hp;
 set64(t->xs, nil, t->len = n);
 hp += n + 1;
 *--sp = _V(t);
 NEXT(2); }

// late bind
// this function is a lil complicated, because it incorporates
// the "static" type and arity checking that would have been
// done by the compiler if the function had been bound early.
VM(lbind) {
 obj w = (obj) H(ip)[1], d = AB(w), y = A(w);
 if (!(w = tbl_get(v, d, xp = BB(w)))) {
  char *nom = nilp(getsym(xp)->nom) ? "()" : symnom(xp);
  Jump(nope, "free variable : %s", nom); }
 xp = w;
 if (getnum(y) != 8) TC(xp, getnum(y)); // do the type check
 terp *q = H(ip)[2]; // omit the arity check if possible
 if (q == call || q == rec) {
  obj aa = (obj) H(ip)[3];
  if (H(xp)[0] == arity && aa >= (obj) H(xp)[1]) xp += W2; }
 H(ip)[0] = imm;
 H(ip)[1] = (terp*) xp;
 NEXT(2); }

// return to C
VM(yield) { Pack(); return xp; }

#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))

// conditional jumps
#define Br(test, a, x, b, y) {\
  if (test) AP((obj)a(H(ip)),x);\
  else AP((obj)b(H(ip)),y); }
#define BR(op, a, x, b, y)\
  Br(*sp++ op xp, a, x, b, y)

VM(branch)  Br(xp == nil, FF, xp, GF, xp)
VM(barnch)  Br(xp == nil, GF, xp, FF, xp)
VM(breq)    Br(eql(*sp++, xp), GF, ok, FF, nil)
VM(brne)    Br(eql(*sp++, xp), FF, ok, GF, nil)

// ordinary jumps

#define BR1(a, b, c)\
 VM(brlt)    BR(a,  GF, b,  FF, c)\
 VM(brgteq2) BR(a,  GF, b,  FF, c)\
 VM(brgteq)  BR(a,  FF, c, GF, b)\
 VM(brlt2)   BR(a,  FF, b,  GF, c)\
 VM(brgt2)   BR(a##=, GF, c, FF, b)\
 VM(brlteq)  BR(a##=, GF, b,  FF, c)\
 VM(brgt)    BR(a##=, FF, c, GF, b)\
 VM(brlteq2) BR(a##=, FF, b,  GF, c)\

BR1(<, xp, nil)
#undef Br

// unconditional jumps
VM(jump) { AP((obj) H(ip)[1], xp); }

// return from a function
VM(ret) {
 ip = Retp;
 sp = (mem) ((i64) Argv + Argc - Num);
 fp = (mem) ((i64)   sp + Subr - Num);
 NEXT(0); }

// regular function call
VM(call) {
 Have(Width(frame));
 obj adic = (obj) H(ip)[1];
 i64 off = fp - (mem) ((i64) sp + adic - Num);
 fp = sp -= Width(frame);
 Retp = ip + W2;
 Subr = _N(off);
 Clos = nil;
 Argc = adic;
 AP(xp, nil); }
 
VM(ap_u) {
 ARY(2);
 obj x = Argv[0],
     y = Argv[1];
 TC(x, Hom);
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
 AP(x, nil); }

static VM(recne) {
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
 NEXT(0); }

// tail call
VM(rec) {
 if (Argc != (ip = (obj) H(ip)[1])) Jump(recne);
 cpy64(Argv, sp, ip = getnum(ip));
 sp = fp;
 AP(xp, nil); }

// continuations
//
// this is a simple but expensive way of doing continuations.
// it would be more memory efficient to do a copy-on-write
// kind of thing where the stack is only copied if the function
// returns. a spaghetti stack would be another option but it
// would be slower. faster continuations at the cost of slower
// function calls seems like a bad deal given the relative
// frequency of the two.
VM(ccc_u) {
 ARY(1);
 TC(*Argv, Hom);
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
 AP(ip, nil); }

// call a continuation
VM(cont) {
 vec t = V((obj) H(ip)[1]);
 Have(t->len - 1);
 xp = N(Argc) == 0 ? nil : *Argv;
 i64 off = N(t->xs[0]);
 sp = v->pool + v->len - (t->len - 1);
 fp = sp + off;
 cpy64(sp, t->xs+1, t->len-1);
 Jump(ret); }

VM(vararg) {
 i64 reqd = N((i64) H(ip)[1]),
     vdic = N(Argc) - reqd;
 ARY(reqd);
 // in this case we need to add another argument
 // slot to hold the nil.
 if (!vdic) {
  Have1();
  cpy64(fp-1, fp, Width(frame) + N(Argc));
  sp = --fp;
  Argc += W;
  Argv[reqd] = nil;
  NEXT(2); }
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
  NEXT(2); } }

// type predicates
#define TP(t) \
    VM(t##pp) { AP(ip+W, (t##p(xp)?ok:nil)); }\
    VM(t##p_u) {\
      for (obj *xs = Argv, *l = xs + N(Argc); xs < l;)\
        if (!t##p(*xs++)) GO(ret, nil);\
      GO(ret, ok); }
TP(num)
TP(hom)
TP(two)
TP(sym)
TP(str)
TP(tbl)
TP(vec)
TP(nil)

// stack manipulation
VM(dupl) { Have1(); --sp; sp[0] = sp[1]; NEXT(1); }

u0 errp(lips v, char *msg, ...) {
  va_list xs;
  fputs("# ", stderr);
  va_start(xs, msg);
  vfprintf(stderr, msg, xs);
  va_end(xs);
  fputc('\n', stderr); }

obj restart(lips v) {
  v->fp = v->sp = v->pool + v->len;
  v->xp = v->ip = nil;
  v->root = NULL;
  longjmp(v->restart, 1); }

VM(nope, const char *msg, ...) {
  // print current call as (function arg1 arg2 ...)
  fputs("# (", stderr);
  emit(v, ip, stderr);
  mem top = v->pool + v->len;
  i64 i = 0, argc = fp == top ? 0 : N(Argc);
  if (argc) for (fputc(' ', stderr);; fputc(' ', stderr)) {
    obj x = Argv[i++];
    emit(v, x, stderr);
    if (i == argc) break; }
  fputc(')', stderr);

  // print error message
  fputs(" : ", stderr);
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  fputc('\n', stderr);

  // print backtrace
  for (;;) {
    ip = Retp, fp += Width(frame) + N(Argc) + N(Subr);
    if (button(H(ip))[-1] == yield) break;
    fputs("# in ", stderr), emsep(v, ip, stderr, '\n'); }

  v->hp = hp;
  return restart(v); }

// errors
VM(fail) { Jump(nope, "fail"); }

VM(type_error) {
  enum tag exp = v->xp, act = kind(xp);
  Jump(nope, "wrong type : %s for %s", tnom(act), tnom(exp)); }

VM(oob_error) {
  Jump(nope, "oob : %d >= %d", v->xp, v->ip); }

VM(ary_error) {
  Jump(nope, arity_err_msg, N(Argc), v->xp); }

// type/arity checking
#define DTC(n, t) VM(n) {\
  if (kind(xp-t)==0) NEXT(1);\
  v->xp = t; Jump(type_error); }
DTC(idZ, Num) DTC(idH, Hom) DTC(idT, Tbl) DTC(id2, Two)
VM(arity) {
 obj reqd = (obj) H(ip)[1];
 if (reqd <= Argc) NEXT(2);
 else Jump((v->xp = N(reqd), ary_error)); }
