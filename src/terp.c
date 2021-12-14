#include "lips.h"
#include "terp.h"
#include "mem.h"
#include "io.h"
#include "hom.h"
#include "tbl.h"
#include "err.h"
#include "cmp.h"
#include "sym.h"
#include "two.h"
#include "str.h"
// " the virtual machine "
// it's a stack machine with one free register that's
// implemented on top of the C compiler's calling convention.
// this allows us to keep the most important state variables
// in CPU registers at all times while the interpreter is
// running, without any platform-specific code.

#define ok _N(1)
// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function
VM(gc) { u64 n = v->xp; CALLC(reqsp(v, n)); NEXT(0); }
// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.

// jump to nope() when an error happens.

// " virtual machine instructions "
//
// load instructions
OP2(imm, (obj)GF(ip)) OP1(unit, nil) OP1(one, N_(1)) OP1(zero, N_(0))

// indexed load instructions
// this pointer arithmetic works because fixnums are
// premultiplied by W
#define REF(b) (*(i64*)((i64)(b)+(i64)GF(ip)-Num))

OP2(arg, REF(ARGV)) OP1(arg0, ARGV[0]) OP1(arg1, ARGV[1])
OP2(loc, REF(V(LOCS)->xs)) OP1(loc0, V(LOCS)->xs[0]) OP1(loc1, V(LOCS)->xs[1])
OP2(clo, REF(V(CLOS)->xs)) OP1(clo0, V(CLOS)->xs[0]) OP1(clo1, V(CLOS)->xs[1])

// store instructions
VM(push) { Have1(); *--sp = xp; NEXT(1); } // stack push
VM(loc_) { REF(V(LOCS)->xs) = xp; NEXT(2); } // set a local variable

VM(tbind) { CALLC(tbl_set(v, Top, (obj) GF(ip), xp)); NEXT(2); }

// initialize local variable slots
VM(locals) {
 u64 n = getnum(GF(ip));
 Have(n + 2);
 vec t = (vec) hp;
 set64(t->xs, nil, t->len = n);
 hp += n + 1;
 *--sp = putvec(t);
 NEXT(2); }

// late bind
// this function is a lil complicated, because it incorporates
// the "static" type and arity checking that would have been
// done by the compiler if the function had been bound early.
VM(lbind) {
 obj w = (obj) GF(ip), d = X(Y(w)), y = X(w);
 if (!(w = tbl_get(v, d, xp = Y(Y(w))))) {
  char *nom = nilp(getsym(xp)->nom) ? "<anon>" : symnom(xp);
  Jump(nope, "free variable : %s", nom); }
 xp = w;
 if (getnum(y) != 8) TC(xp, getnum(y)); // do the type check
 terp *q = G(FF(ip)); // omit the arity check if possible
 if (q == call || q == rec) {
  obj aa = (obj) GF(FF(ip));
  if (G(xp) == arity && aa >= (obj) GF(xp)) xp += W2; }
 G(ip) = imm, GF(ip) = (terp*) xp;
 NEXT(2); }

// control flow instructions
// return to C
VM(yield) { PACK(); return xp; }

// conditional jumps
#define Br(_x_, a, x, b, y) {if(_x_)AP((obj)a(ip),x);AP((obj)b(ip),y);}
#define BR(op, a, x, b, y) Br(*sp++ op xp, a, x, b, y)

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
#undef B

// unconditional jumps
VM(jump) { AP((obj) GF(ip), xp); }
VM(clos) { CLOS = (obj) GF(ip); AP((obj) G(FF(ip)), xp); }

// return from a function
VM(ret) {
 ip = RETP;
 sp = (mem) ((i64) ARGV + ARGC - Num);
 fp = (mem) ((i64)   sp + SUBR - Num);
 NEXT(0); }

// regular function call
VM(call) {
 Have(Size(frame));
 obj adic = (obj) GF(ip);
 i64 off = fp - (mem) ((i64) sp + adic - Num);
 fp = sp -= Size(frame);
 RETP = Ph(ip+W2);
 SUBR = Pn(off);
 CLOS = nil;
 ARGC = adic;
 AP(xp, nil); }
 
VM(ap_u) {
 ARY(2);
 obj x = ARGV[0], y = ARGV[1];
 TC(x, Hom);
 u64 adic = llen(y);
 Have(adic);
 obj off = SUBR, rp = RETP;
 sp = ARGV + Gn(ARGC) - adic;
 for (u64 j = 0; j < adic; y = Y(y)) sp[j++] = X(y);
 fp = sp -= Size(frame);
 RETP = rp;
 ARGC = N_(adic);
 SUBR = off;
 CLOS = nil;
 AP(x, nil); }


static VM(recne) {
 // overwrite current frame with new frame
 v->xp = SUBR, v->ip = RETP; // save return info
 fp = ARGV + N(ARGC - ip);
 cpy64r(fp, sp, N(ip)); // copy from high to low
 sp = fp -= Size(frame);
 RETP = v->ip;
 ARGC = ip;
 SUBR = v->xp;
 ip = xp;
 CLOS = xp = nil;
 NEXT(0); }

// tail call
VM(rec) {
 if (ARGC!=(ip = (obj) GF(ip))) Jump(recne);
 cpy64(ARGV, sp, ip = getnum(ip));
 sp = fp;
 AP(xp, nil); }

// type/arity checking
#define TDCN(t) if(!kind(xp-t)){ NEXT(1);}\
 Jump(nope,type_err_msg,tnom(kind(xp)),tnom(t))
#define DTC(n, t) VM(n) { TDCN(t); }
DTC(idZ, Num) DTC(idH, Hom) DTC(idT, Tbl) DTC(id2, Two)
VM(arity) {
 obj reqd = (obj) GF(ip);
 if (reqd <= ARGC) { NEXT(2); }
 Jump(nope, arity_err_msg, N(ARGC), N(reqd)); }

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
 ARY(1); TC(*ARGV, Hom); ip = *ARGV;
 // we need space for:
 // the entire stack
 // the frame offset
 // the length (to put it all in a tuple)
 // the continuation thread (4 words)
 i64 ht = v->pool + v->len - sp;
 Have(ht + 6);
 vec t = (vec) hp;
 hp += ht + 2;
 t->len = ht + 1;
 t->xs[0] = Pn(fp - sp);
 cpy64(t->xs+1, sp, ht);
 hom c = (hom) hp;
 hp += 4;
 c[0] = cont;
 c[1] = (terp*) putvec(t);
 c[2] = NULL;
 c[3] = (terp*) c;
 ARGV[0] = Ph(c);
 AP(ip, nil); }

// call a continuation
VM(cont) {
 vec t = getvec(GF(ip));
 Have(t->len - 1);
 xp = N(ARGC) == 0 ? nil : *ARGV;
 i64 off = N(t->xs[0]);
 sp = v->pool + v->len - (t->len - 1);
 fp = sp + off;
 cpy64(sp, t->xs+1, t->len-1);
 Jump(ret); }

VM(vararg) {
 i64 reqd = Gn(GF(ip)),
     vdic = Gn(ARGC) - reqd;
 ARY(reqd);
 // in this case we need to add another argument
 // slot to hold the nil.
 if (!vdic) {
  Have1();
  cpy64(fp-1, fp, Size(frame) + Gn(ARGC));
  sp = --fp;
  ARGC += W;
  ARGV[reqd] = nil; }
 // in this case we just keep the existing slots.
 // the path is knowable at compile time in many cases
 // so maybe vararg should be two or more different
 // functions.
 else {
  Have(2 * vdic);
  two t = (two) hp;
  hp += 2 * vdic;
  for (i64 i = vdic; i--;
   t[i].a = ARGV[reqd + i],
   t[i].b = puttwo(t+i+1));
  t[vdic-1].b = nil;
  ARGV[reqd] = puttwo(t); }
 NEXT(2); }

// the next few functions create and store
// lexical environments.
static VM(encl) {
 i64 n = v->xp;
 obj x = (obj) GF(ip), arg = nil;
 mem block = hp;
 hp += n;
 if (n > 11) {
  n -= 12;
  vec t = (vec) block;
  block += 1 + n;
  t->len = n;
  while (n--) t->xs[n] = ARGV[n];
  arg = putvec(t); }

 vec t = (vec) block; // compiler thread closure array (1 length 5 elements)
 hom at = (hom) (block+6); // compiler thread (1 instruction 2 data 2 tag)

 t->len = 5; // initialize alpha closure
 t->xs[0] = arg;
 t->xs[1] = xp; // LOCS or nil
 t->xs[2] = CLOS;
 t->xs[3] = Y(x);
 t->xs[4] = Ph(at);

 at[0] = pc0;
 at[1] = (terp*) putvec(t);
 at[2] = (terp*) X(x);
 at[3] = 0;
 at[4] = (terp*) at;

 AP(ip+W2, Ph(at)); }

VM(prencl) {
 u64 n = Gn(ARGC);
 n += n ? 12 : 11;
 Have(n);
 v->xp = n;
 Jump(encl); }

VM(encll) { GO(prencl, LOCS); }
VM(encln) { GO(prencl, nil); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
VM(pc0) {
 obj ec  = (obj) GF(ip),
     arg = V(ec)->xs[0],
     loc = V(ec)->xs[1];
 u64 adic = nilp(arg) ? 0 : V(arg)->len;
 Have(Size(frame) + adic + 1);
 i64 off = (mem) fp - sp;
 G(ip) = pc1;
 sp -= adic;
 cpy64(sp, V(arg)->xs, adic);
 ec = (obj) GF(ip);
 fp = sp -= Size(frame);
 RETP = ip;
 SUBR = _N(off);
 ARGC = _N(adic);
 CLOS = V(ec)->xs[2];
 if (!nilp(loc)) *--sp = loc;
 ip = V(ec)->xs[3];
 NEXT(0); }

// finalize function instance closure
VM(pc1) { G(ip) = clos; GF(ip) = (terp*) xp; NEXT(0); }

// type predicates
#define TP(ty) VM(ty##pp) { AP(ip+W, (ty##p(xp)?ok:nil)); }
TP(num) TP(hom) TP(two) TP(sym) TP(str) TP(tbl) TP(nil) TP(vec)

static VM(typ) {
 for (obj *xs = ARGV, *l = xs + N(ARGC); xs < l;)
  if (kind(*xs++) != xp) GO(ret, nil);
 GO(ret, ok); }
#define typp(t, i) VM(t##p_u) { GO(typ, i); }
typp(num, Num) typp(hom, Hom) typp(str, Str) typp(tbl, Tbl)
typp(two, Two) typp(sym, Sym) typp(nil, Nil) typp(vec, Vec)

// stack manipulation
VM(dupl) { Have1(); --sp; sp[0] = sp[1]; NEXT(1); }

VM(rnd_u) { GO(ret, Pn(lcprng(&v->seed))); }
