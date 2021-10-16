#include "lips.h"
#include "terp.h"
static obj compile(lips, obj);
// " the virtual machine "
// it's a stack machine with one free register that's
// implemented on top of the C compiler's calling convention.
// this allows us to keep the most important state variables
// in CPU registers at all times while the interpreter is
// running, without any platform-specific code.

// " the interpreter "
// is all the functions of type terp:
#define interp(n,...) NoInline obj \
 n(lips v, obj ip, mem fp, mem sp, mem hp, obj xp, ##__VA_ARGS__)
// the arguments to a terp function collectively represent the
// runtime state, and the  return value is the result of the
// program. there are six arguments because that's the number
// that the prevalent unix calling convention on AMD64 (System
// V ABI) says should be passed in registers; that's the only
// reason why there aren't more. but it's not too bad; the six
// arguments are:
// - v  : vm instance pointer ; most lips functions take this as the first argument
// - ip : instruction pointer ; the current vm instruction ; function pointer pointer
// - fp : frame pointer       ; current function context
// - sp : stack pointer       ; data/call stack
// - hp : heap pointer        ; the next free heap location
// - xp : return value        ; general-purpose register

// when the interpreter isn't running, the state variables that
// would normally be in registers are stored in slots on the
// vm structure. however while the interpreter is running it
// uses these struct slots to pass and return extra values
// without using the stack. so the interpreter has to restore
// the current values in the vm struct before it makes any
// "external" function calls.
#define PACK() (v->ip=ip,Sp=sp,Hp=hp,Fp=fp,v->xp=xp)
#define UNPACK() (fp=Fp,hp=Hp,sp=Sp,ip=v->ip,xp=v->xp)
#define CALLC(...)(PACK(),(__VA_ARGS__),UNPACK())
#define RETC(...){CALLC(__VA_ARGS__);Jump(ret);}

// the frame structure holds the current function context.
typedef struct frame { obj clos, retp, subd, argc, argv[]; } *frame;
#define CLOS ((frame)fp)->clos
#define RETP ((frame)fp)->retp
#define SUBR ((frame)fp)->subd
#define ARGC ((frame)fp)->argc
#define ARGV ((frame)fp)->argv
// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer.
#define LOCS fp[-1]
// if a function has locals, this will have been initialized by the
// by the time they are referred to. the wrinkle in the representation
// gives a small but significant benefit to general function call
// performance and should be extended to the closure pointer, which is often
// nil.

// the return value of a terp function is usually a call
// to another terp function.
#define STATE v,ip,fp,sp,hp,xp
#define Jump(f,...) return (f)(STATE,##__VA_ARGS__)
#define AP(f,x) return (ip=f,xp=x,G(ip)(STATE))
#define GO(f,x) return (xp=x,f(STATE))
#define NEXT(n) AP(ip+w2b(n),xp)
#define ok _N(1)
// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function
static interp(gc) { u64 n = v->xp; CALLC(reqsp(v, n)); NEXT(0); }
// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.
#define avail (sp-hp)
#define Have(n) if (avail < n) Jump((v->xp=n,gc))
#define Have1() if (hp == sp) Jump((v->xp=1,gc)) // common case, faster comparison

#define arity_err_msg "wrong arity : %d of %d"
#define type_err_msg "wrong type : %s for %s"
#define div0_err_msg "%d / 0"
#define TERP(n, m, ...) interp(n) m(__VA_ARGS__)

// jump to nope() when an error happens.
static interp(nope, const char *, ...);

// type check
#define TC(x,t) if(kind((x))-(t))\
 Jump(nope, type_err_msg, tnom(kind(x)), tnom(t))
// arity check
#define ARY(n) if(_N(n)>ARGC)\
 Jump(nope,arity_err_msg,getnum(ARGC),n)

#define OP(nom, x, n) interp(nom) { xp = (x); NEXT(n); }
#define OP0(nom, x) OP(nom, x, 0)
#define OP1(nom, x) OP(nom, x, 1)
#define OP2(nom, x) OP(nom, x, 2)
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
interp(push) { Have1(); *--sp = xp; NEXT(1); } // stack push
interp(loc_) { REF(V(LOCS)->xs) = xp; NEXT(2); } // set a local variable
interp(tbind) { CALLC(tblset(v, Top, (obj) GF(ip), xp)); NEXT(2); }

// initialize local variable slots
interp(locals) {
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
interp(lbind) {
 obj w = (obj) GF(ip), d = X(Y(w)), y = X(w);
 if (!(w = tblget(v, d, xp = Y(Y(w))))) {
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
interp(yield) { PACK(); return xp; }

// conditional jumps
#define Br(_x_, a, x, b, y) {if(_x_)AP((obj)a(ip),x);AP((obj)b(ip),y);}
#define BR(op, a, x, b, y) Br(*sp++ op xp, a, x, b, y)

interp(branch)  Br(xp == nil, FF, xp, GF, xp)
interp(barnch)  Br(xp == nil, GF, xp, FF, xp)
interp(breq)    Br(eql(*sp++, xp), GF, ok, FF, nil)
interp(brne)    Br(eql(*sp++, xp), FF, ok, GF, nil)

// ordinary jumps

#define BR1(a, b, c)\
 interp(brlt)    BR(a,  GF, b,  FF, c)\
 interp(brgteq2) BR(a,  GF, b,  FF, c)\
 interp(brgteq)  BR(a,  FF, c, GF, b)\
 interp(brlt2)   BR(a,  FF, b,  GF, c)\
 interp(brgt2)   BR(a##=, GF, c, FF, b)\
 interp(brlteq)  BR(a##=, GF, b,  FF, c)\
 interp(brgt)    BR(a##=, FF, c, GF, b)\
 interp(brlteq2) BR(a##=, FF, b,  GF, c)\

BR1(<, xp, nil)
#undef Br
#undef B

// unconditional jumps
interp(jump) { AP((obj) GF(ip), xp); }
interp(clos) { CLOS = (obj) GF(ip); AP((obj) G(FF(ip)), xp); }

// return from a function
interp(ret) {
 ip = RETP;
 sp = (mem) ((i64) ARGV + ARGC - Num);
 fp = (mem) ((i64)   sp + SUBR - Num);
 NEXT(0); }

// regular function call
interp(call) {
 Have(Size(frame));
 obj adic = (obj) GF(ip);
 i64 off = fp - (mem) ((i64) sp + adic - Num);
 fp = sp -= Size(frame);
 RETP = Ph(ip+W2);
 SUBR = Pn(off);
 CLOS = nil;
 ARGC = adic;
 AP(xp, nil); }

static interp(recne) {
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
interp(rec) {
 if (ARGC!=(ip = (obj) GF(ip))) Jump(recne);
 cpy64(ARGV, sp, ip = getnum(ip));
 sp = fp;
 AP(xp, nil); }

// type/arity checking
#define TDCN(t) if(!kind(xp-t)){ NEXT(1);}\
 Jump(nope,type_err_msg,tnom(kind(xp)),tnom(t))
#define DTC(n, t) interp(n) { TDCN(t); }
DTC(idZ, Num) DTC(idH, Hom) DTC(idT, Tbl) DTC(id2, Two)
interp(arity) {
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
interp(ccc_u) {
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
interp(cont) {
 vec t = getvec(GF(ip));
 Have(t->len - 1);
 xp = N(ARGC) == 0 ? nil : *ARGV;
 i64 off = N(t->xs[0]);
 sp = v->pool + v->len - (t->len - 1);
 fp = sp + off;
 cpy64(sp, t->xs+1, t->len-1);
 Jump(ret); }

interp(ap_u) {
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

// instructions used by the compiler
interp(hom_u) {
 obj x;
 ARY(1);
 TC(x = *ARGV, Num);
 i64 len = Gn(x) + 2;
 Have(len);
 hom h = (hom) hp;
 hp += len;
 set64((mem) h, nil, len);
 h[len-1] = (terp*) h;
 h[len-2] = NULL;
 GO(ret, Ph(h+len-2)); }

interp(vset_u) {
 ARY(3);
 TC(ARGV[0], Vec);
 TC(ARGV[1], Num);
 num idx = getnum(ARGV[1]);
 vec ary = getvec(ARGV[0]);
 if (idx < 0 || idx >= ary->len) Jump(nope, "oob : %d#%d");
 GO(ret, ary->xs[idx] = ARGV[2]); }

interp(vget_u) {
 ARY(2);
 TC(ARGV[0], Vec);
 TC(ARGV[1], Num);
 num idx = getnum(ARGV[1]);
 vec ary = getvec(ARGV[0]);
 if (idx < 0 || idx >= ary->len) Jump(nope, "oob : %d#%d");
 GO(ret, ary->xs[idx]); }

interp(vec_u) {
 obj n = N(ARGC);
 Have(n + 1);
 vec t = (vec) hp;
 hp += 1 + n;
 cpy64(t->xs, ARGV, t->len = n);
 GO(ret, putvec(t)); }

interp(tset) {
 obj x = *sp++, y = *sp++;
 CALLC(v->xp = tblset(v, xp, x, y));
 NEXT(1); }

interp(emx) { obj h = *sp++ - W; G(h) = (terp*) xp;    AP(ip+W, h); }
interp(emi) { obj h = *sp++ - W; G(h) = (terp*) N(xp); AP(ip+W, h); }

interp(emx_u) {
 ARY(2);
 obj h = ARGV[1];
 TC(h, Hom);
 h -= W;
 G(h) = (terp*) ARGV[0];
 GO(ret, h); }

interp(emi_u) {
 ARY(2);
 TC(ARGV[0], Num);
 obj h = ARGV[1];
 TC(h, Hom);
 h -= W;
 G(h) = (terp*) Gn(ARGV[0]);
 GO(ret, h); }

interp(hgeti_u) { ARY(1); TC(ARGV[0], Hom); GO(ret,   N_(G(ARGV[0]))); }
interp(hgetx_u) { ARY(1); TC(ARGV[0], Hom); GO(ret, (obj)G(ARGV[0])); }

interp(hseek_u) {
 ARY(2); TC(ARGV[0], Hom); TC(ARGV[1], Num);
 GO(ret, H_(H(ARGV[0])+N(ARGV[1]))); }

// hash tables
interp(tblg) {
 ARY(2);
 TC(ARGV[0], Tbl);
 xp = tblget(v, ARGV[0], ARGV[1]);
 GO(ret, xp ? xp : nil); }

OP1(tget, (xp = tblget(v, xp, *sp++)) ? xp : nil)

static obj tblkeys_j(lips v, ent e, obj l) {
 obj x;
 return !e ? l :
  (x = e->key,
   with(x, l = tblkeys_j(v, e->next, l)),
   pair(v, x, l)); }

static obj tblkeys_i(lips v, obj t, i64 i) {
 obj k;
 return i == gettbl(t)->cap ? nil :
  (with(t, k = tblkeys_i(v, t, i+1)),
   tblkeys_j(v, gettbl(t)->tab[i], k)); }

static Inline obj tblkeys(lips v, obj t) {
 return tblkeys_i(v, t, 0); }

OP1(thas, tblget(v, xp, *sp++) ? ok : nil)
OP1(tlen, N_(gettbl(xp)->len))
interp(tkeys) { CALLC(v->xp = tblkeys(v, xp)); NEXT(1); }

interp(tblc) {
 ARY(2);
 TC(ARGV[0], Tbl);
 xp = tblget(v, ARGV[0], ARGV[1]);
 GO(ret, xp ? ok : nil); }

static obj tblss(lips v, i64 i, i64 l) {
 mem fp = Fp;
 return i > l-2 ? ARGV[i-1] :
  (tblset(v, v->xp, ARGV[i], ARGV[i+1]),
   tblss(v, i+2, l)); }

interp(tbls) {
 ARY(1);
 xp = *ARGV;
 TC(xp, Tbl);
 RETC(v->xp = tblss(v, 1, N(ARGC))); }

interp(tblmk) { RETC(v->xp = table(v), tblss(v, 0, N(ARGC))); }

interp(tbld) {
 ARY(2); TC(ARGV[0], Tbl);
 RETC(v->xp = tbldel(v, ARGV[0], ARGV[1])); }

interp(tblks) {
 ARY(1); TC(*ARGV, Tbl);
 RETC(v->xp = tblkeys(v, *ARGV)); }

interp(tbll) {
 ARY(1); TC(*ARGV, Tbl);
 GO(ret, N_(gettbl(*ARGV)->len)); }

// string instructions
interp(strl) {
 ARY(1); TC(*ARGV, Str);
 GO(ret, N_(getstr(*ARGV)->len-1)); }

interp(strg) {
 ARY(2); TC(ARGV[0], Str); TC(ARGV[1], Num);
 GO(ret, N(ARGV[1]) < getstr(ARGV[0])->len-1 ?
  N_(getstr(ARGV[0])->text[N(ARGV[1])]) :
  nil); }

interp(strconc) {
 i64 l = Gn(ARGC), sum = 0, i = 0;
 while (i < l) {
  obj x = ARGV[i++];
  TC(x, Str);
  sum += getstr(x)->len - 1; }
 i64 words = b2w(sum+1) + 1;
 Have(words);
 str d = (str) hp;
 hp += words;
 d->len = sum + 1;
 d->text[sum] = 0;
 while (i) {
  str x = getstr(ARGV[--i]);
  sum -= x->len - 1;
  cpy8(d->text+sum, x->text, x->len - 1); }
 GO(ret, putstr(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
interp(strs) {
 ARY(3);
 TC(ARGV[0], Str); TC(ARGV[1], Num); TC(ARGV[2], Num);
 str src = getstr(ARGV[0]);
 i64 lb = Gn(ARGV[1]), ub = Gn(ARGV[2]);
 lb = max(lb, 0);
 ub = min(ub, src->len-1);
 ub = max(ub, lb);
 i64 words = 1 + b2w(ub - lb + 1);
 Have(words);
 str dst = (str) hp;
 hp += words;
 dst->len = ub - lb + 1;
 dst->text[ub - lb] = 0;
 cpy8(dst->text, src->text + lb, ub - lb);
 GO(ret, putstr(dst)); }

interp(strmk) {
 i64 i = 0, l = Gn(ARGC)+1, size = 1 + b2w(l);
 Have(size);
 str s = (str) hp;
 hp += size;
 for (obj x; i < l-1; s->text[i++] = Gn(x)) {
  x = ARGV[i];
  TC(x, Num);
  if (x == Pn(0)) break; }
 s->text[i] = 0;
 s->len = i+1;
 GO(ret, putstr(s)); }

interp(vararg) {
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
static interp(encl) {
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

interp(prencl) {
 u64 n = Gn(ARGC);
 n += n ? 12 : 11;
 Have(n);
 v->xp = n;
 Jump(encl); }

interp(encll) { GO(prencl, LOCS); }
interp(encln) { GO(prencl, nil); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
interp(pc0) {
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
interp(pc1) { G(ip) = clos; GF(ip) = (terp*) xp; NEXT(0); }

// this is used to create closures.
interp(take) {
 u64 n = Gn((obj) GF(ip));
 Have(n + 1);
 vec t = (vec) hp;
 hp += n + 1;
 t->len = n;
 cpy64(t->xs, sp, n);
 sp += n;
 GO(ret, putvec(t)); }

// print to console
interp(em_u) {
 u64 l = Gn(ARGC), i;
 if (l) {
  for (i = 0; i < l - 1; i++)
   emsep(v, ARGV[i], stdout, ' ');
  emit(v, xp = ARGV[i], stdout); }
 fputc('\n', stdout);
 Jump(ret); }

interp(putc_u) { ARY(1); fputc(N(*ARGV), stdout); Jump(ret); }
interp(getc_u) { GO(ret, feof(stdin) ? nil : N_(getc(stdin))); }

// pairs
OP1(car, X(xp)) OP1(cdr, Y(xp))
interp(cons) {
 Have1(); hp[0] = xp, hp[1] = *sp++;
 xp = puttwo(hp); hp += 2; NEXT(1); }

interp(car_u) { ARY(1); TC(*ARGV, Two); GO(ret, X(*ARGV)); }
interp(cdr_u) { ARY(1); TC(*ARGV, Two); GO(ret, Y(*ARGV)); }
interp(cons_u) {
 ARY(2); Have(2);
 hp[0] = ARGV[0], hp[1] = ARGV[1];
 xp = puttwo(hp), hp += 2; Jump(ret); }

// arithmetic
#define BINOP(nom, xpn) interp(nom) { xp = (xpn); NEXT(1); }
BINOP(add,  xp + *sp++ - Num)
BINOP(sub,  *sp++ - xp + Num)
BINOP(mul,  N_(N(*sp++)  * N(xp)))
BINOP(sar,  N_(N(*sp++) >> N(xp)))
BINOP(sal,  N_(N(*sp++) << N(xp)))
BINOP(band, xp & *sp++)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
OP1(neg, N_(-N(xp)))
interp(dqv) {
 if (xp == Pn(0)) Jump(nope, div0_err_msg, Gn(*sp));
 xp = Pn(Gn(*sp++) / Gn(xp));
 NEXT(1); }
interp(mod) {
 if (xp == Pn(0)) Jump(nope, div0_err_msg, Gn(*sp));
 xp = Pn(Gn(*sp++) % Gn(xp));
 NEXT(1); }


#define mm_u(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op Gn(x)){\
  x = *xs++; TC(x, Num);}\
 GO(ret, Pn(xp));}
#define mm_u0(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op Gn(x)){\
  x = *xs++; TC(x, Num);\
  if (x == Pn(0)) Jump(nope, div0_err_msg, xp);}\
 GO(ret, Pn(xp));}

#define UBINOP(nom, dflt, op)\
 interp(nom##_u) { mm_u(Gn(ARGC), ARGV, dflt, op); }
UBINOP(add, 0, +) UBINOP(mul, 1, *)
UBINOP(band, -1, &) UBINOP(bor, 0, |) UBINOP(bxor, 0, ^)

interp(sub_u) {
 if (!(xp = Gn(ARGC))) GO(ret, Pn(0));
 TC(*ARGV, Num);
 if (xp == 1) GO(ret, Pn(-Gn(*ARGV)));
 mm_u(xp-1,ARGV+1,Gn(*ARGV),-); }

interp(div_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TC(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),/); }
interp(mod_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TC(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),%); }

interp(sar_u) {
 if (ARGC == N_(0)) GO(ret, Pn(0));
 TC(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), >>); }
interp(sal_u) {
 if (ARGC == N_(0)) GO(ret, N_(0));
 TC(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), <<); }

// type predicates
#define TP(ty) interp(ty##pp) { AP(ip+W, (ty##p(xp)?ok:nil)); }
TP(num) TP(hom) TP(two) TP(sym) TP(str) TP(tbl) TP(nil) TP(vec)

bool eql(obj a, obj b) {
 if (a == b)             return true;
 if (kind(a) != kind(b)) return false;
 switch (kind(a)) {
  case Two:
   // pairs are immutable, so we can deduplicate their contents.
   if (!eql(X(a), X(b))) return false;
   X(a) = X(b);
   if (!eql(Y(a), Y(b))) return false;
   Y(a) = Y(b);
   return true;
  case Str: {
   str o = getstr(a), m = getstr(b);
   return o->len == m->len && scmp(o->text, m->text) == 0; }
  default:  return false; } }

#define cmp_(n, op) BINOP(n, *sp++ op xp ? xp : nil)
cmp_(lt, <) cmp_(lteq, <=) cmp_(gteq, >=) cmp_(gt, >)
// there should be a separate instruction for simple equality?
BINOP(eq, eql(xp, *sp++) ? ok : nil)

static interp(ord_) {
 bool (*r)(obj, obj) = (void*)v->xp;
 obj n=Gn(ARGC),*xs=ARGV,m,*l;
 switch(n){
  case 0:no:GO(ret, nil);
  default:for(l=xs+n-1,m=*xs;xs<l;m=*++xs)if(!(r(m,xs[1])))goto no;
  case 1:break;}
 GO(ret, ok);}

#define ord_w(r)v->xp=(obj)r;Jump(ord_)
#define cmp(op, n)\
 static bool cmp_##n(obj a, obj b) { return a op b; }\
 interp(n##_u) { v->xp=(obj)cmp_##n;Jump(ord_); }
cmp(<, lt) cmp(<=, lteq) cmp(>=, gteq) cmp(>, gt)
interp(eq_u) { ord_w(eql); }

static interp(typ) {
 for (obj *xs = ARGV, *l = xs + Gn(ARGC); xs < l;)
  if (kind(*xs++) != xp) GO(ret, nil);
 GO(ret, ok); }
#define typp(t, i) interp(t##p_u) { GO(typ, i); }
typp(num, Num) typp(hom, Hom) typp(str, Str) typp(tbl, Tbl)
typp(two, Two) typp(sym, Sym) typp(nil, Nil) typp(vec, Vec)

// stack manipulation
interp(dupl) { Have1(); --sp; sp[0] = sp[1]; NEXT(1); }

interp(ystr_u) {
 ARY(1);
 xp = *ARGV;
 TC(xp, Sym);
 GO(ret, getsym(xp)->nom); }

// errors
interp(fail) { Jump(nope, "fail"); }
interp(gsym_u) {
 if (ARGC > Pn(0) && strp(*ARGV))
  RETC(v->xp = intern(v, *ARGV));
 Have(Size(sym));
 sym y = (sym) hp;
 hp += Size(sym);
 y->nom = y->l = y->r = nil;
 y->code = v->count++ * mix;
 GO(ret, putsym(y)); }

interp(hfin_u) {
 ARY(1);
 obj a = *ARGV;
 TC(a, Hom);
 GF(button(Gh(a))) = (terp*) a;
 GO(ret, a); }

interp(ev_u) {
 ARY(1);
 RETC(xp = compile(v, *ARGV),
      v->xp = G(xp)(v, xp, Fp, Sp, Hp, nil)); }

interp(rnd_u) { GO(ret, Pn(lcprng(&v->seed))); }

interp(slurp) {
  ARY(1);
  xp = *ARGV;
  TC(xp, Str);
  RETC(v->xp = slurp_file(v, getstr(xp)->text)); }
interp(dump) {
 ARY(2);
 TC(ARGV[0], Str); TC(ARGV[1], Str);
 char *p = getstr(ARGV[0])->text,
      *d = getstr(ARGV[1])->text;
 GO(ret, dump_file(v, p, d)); }

// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
static Inline u0 perrarg(lips v, mem fp) {
 i64 i = 0, argc = fp == v->pool + v->len ? 0 : getnum(ARGC);
 if (argc) for (fputc(' ', stderr);;fputc(' ', stderr)) {
  obj x = ARGV[i++];
  emit(v, x, stderr);
  if (i == argc) break; }
 fputc(')', stderr); }

static interp(nope, const char *msg, ...) {
 fputs("# (", stderr);
 emit(v, Ph(ip), stderr);
 perrarg(v, fp);
 va_list xs;
 fputs(" : ", stderr);
 va_start(xs, msg); vfprintf(stderr, msg, xs);
 fputc('\n', stderr);
 for (;;) {
  ip = RETP, fp += Size(frame) + getnum(ARGC) + getnum(SUBR);
  if (button(Gh(ip))[-1] == yield) break;
  fputs("# in ", stderr), emsep(v, Ph(ip), stderr, '\n'); }
 Hp = hp;
 return restart(v); }

obj restart(lips v) {
 v->fp = v->sp = v->pool + v->len;
 v->xp = v->ip = nil;
 v->root = NULL;
 if (v->restart) longjmp(*v->restart, 1);
 abort(); }
////
/// bootstrap thread compiler
//
// functions construct their continuations by pushing function
// pointers onto the main stack with Push
#define Push(...) pushs(v,__VA_ARGS__,non)
// and then calling them with Ccc
#define Ccc ((c1*)Gn(*Sp++))
// there's a natural correspondence between the push/Ccc pattern
// in this file and normal continuation passing style in lisp
// (cf. the stage 2 compiler).

// in addition to the main stack, the compiler uses Xp and Ip
// as stacks for storing code entry points when generating
// conditionals, which is admittedly kind of sus.
//
// this compiler emits runtime type checks for safety but does
// (almost) no optimizations or static typing since all it has
// to do is bootstrap the main compiler.

// " compilation environments "
// the current lexical environment is passed to compiler
// functions as a pointer to an object, either a tuple with a
// structure specified below, or nil for toplevel. it's a
// pointer to an object, instead of just an object, so it can
// be gc-protected once instead of separately by every function.
// in the other compiler it's just a regular object.
#define arg(x)  V(x)->xs[0] // argument variables : a list
#define loc(x)  V(x)->xs[1] // local variables : a list
#define clo(x)  V(x)->xs[2] // closure variables : a list
#define par(x)  V(x)->xs[3] // surrounding scope : tuple or nil
#define name(x) V(x)->xs[4] // function name : a symbol or nil
#define asig(x) V(x)->xs[5] // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.

static u0 scan(lips, mem, obj);
static obj hfin(lips, obj), hini(lips, u64), c_la_clo(lips, mem, obj, obj), ltu(lips, mem, obj, obj);
typedef obj c1(lips, mem, u64), c2(lips, mem, u64, obj);
static c1 c_ev, c_d_bind, inst, insx, c_ini;
static c2 c_eval, c_sy, c_2, c_imm, c_ap;

enum { Here, Loc, Arg, Clo, Wait };
#define c1(nom,...) static obj nom(lips v, mem e, u64 m, ##__VA_ARGS__)
#define c2(nom,...) static obj nom(lips v, mem e, u64 m, obj x, ##__VA_ARGS__)

// helper functions for lists
static i64 lidx(obj l, obj x) {
 for (i64 i = 0; twop(l); l = Y(l), i++)
  if (x == X(l)) return i;
 return -1; }

static obj linitp(lips v, obj x, mem d) {
 obj y; return !twop(Y(x)) ? (*d = x, nil) :
  (with(x, y = linitp(v, Y(x), d)), pair(v, X(x), y)); }

static obj snoc(lips v, obj l, obj x) {
 return !twop(l) ? pair(v, x, l) :
  (with(l, x = snoc(v, Y(l), x)), pair(v, X(l), x)); }

static u0 pushss(lips v, i64 i, va_list xs) {
 obj x = va_arg(xs, obj);
 x ? (with(x, pushss(v, i, xs)), *--Sp = x) : reqsp(v, i); }

static u0 pushs(lips v, ...) {
 i64 i = 0;
 va_list xs; va_start(xs, v);
 while (va_arg(xs, obj)) i++;
 va_end(xs), va_start(xs, v);
 if (Avail < i) pushss(v, i, xs);
 else for (mem sp = Sp -= i; i--; *sp++ = va_arg(xs, obj));
 va_end(xs); }

// emit code backwards like cons
static obj em1(terp *i, obj k) {
 return k -= W, G(k) = i, k; }
static obj em2(terp *i, obj j, obj k) {
 return em1(i, em1((terp*)j, k)); }

static obj imx(lips v, mem e, i64 m, terp *i, obj x) {
 return Push(Pn(i), x), insx(v, e, m); }

static NoInline obj apply(lips v, obj f, obj x) {
 Push(f, x);
 hom h = cells(v, 5);
 h[0] = call;
 h[1] = (terp*) Pn(2);
 h[2] = yield;
 h[3] = NULL;
 h[4] = (terp*) h;
 return call(v, Ph(h), Fp, Sp, Hp, tblget(v, Top, App)); }

static NoInline obj rwlade(lips v, obj x) {
 mm(&x);
 for (obj y; twop(X(x));
  y = snoc(v, Y(X(x)), X(Y(x))),
  y = pair(v, v->glob[Lamb], y),
  y = pair(v, y, Y(Y(x))),
  x = pair(v, X(X(x)), y));
 return um, x; }

static int scan_def(lips v, mem e, obj x) {
 if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
 if (!twop(Y(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
 mm(&x);
 int r = scan_def(v, e, Y(Y(x)));
 if (r) {
  x = rwlade(v, x);
  obj y = pair(v, X(x), loc(*e));
  loc(*e) = y;
  scan(v, e, X(Y(x))); }
 return um, r; }

static u0 scan(lips v, mem e, obj x) {
 if (!twop(x) || X(x) == La || X(x) == Qt) return;
 if (X(x) == De) return (u0) scan_def(v, e, Y(x));
 for (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
 um; }

static obj asign(lips v, obj a, i64 i, mem m) {
 obj x;
 if (!twop(a)) return *m = i, a;
 if (twop(Y(a)) && X(Y(a)) == Va)
  return *m = -(i+1), pair(v, X(a), nil);
 with(a, x = asign(v, Y(a), i+1, m));
 return pair(v, X(a), x); }

static vec tuplr(lips v, i64 i, va_list xs) {
 vec t; obj x;
 return (x = va_arg(xs, obj)) ?
  (with(x, t = tuplr(v, i+1, xs)), t->xs[i] = x, t) :
  ((t = cells(v, Size(tup) + i))->len = i, t); }

static obj tupl(lips v, ...) { vec t; va_list xs; return
 va_start(xs, v), t = tuplr(v, 0, xs), va_end(xs), putvec(t); }

static Inline obj scope(lips v, mem e, obj a, obj n) {
 i64 s = 0;
 return with(n, a = asign(v, a, 0, &s)),
        tupl(v, a, nil, nil, e ? *e : nil, n, Pn(s), non); }

static Inline obj compose(lips v, mem e, obj x) {
 Push(Pn(c_ev), x, Pn(inst), Pn(ret), Pn(c_ini));
 scan(v, e, Sp[1]);
 x = Ccc(v, e, 4); // 4 = 2 + 2
 i64 i = llen(loc(*e));
 if (i) x = em2(locals,  Pn(i), x);
 i = Gn(asig(*e));
 if (i > 0) x = em2(arity, Pn(i), x);
 else if (i < 0) x = em2(vararg, Pn(-i-1), x);
 x = hfin(v, x);
 return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expression, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static obj ltu(lips v, mem e, obj n, obj l) {
 obj y;
 l = Y(l);
 with(n,
  l = twop(l) ? l : pair(v, l, nil),
  with(y, l = linitp(v, l, &y),
          with(l, n = pair(v, n, e ? name(*e) : nil)),
          n = scope(v, e, l, n)),
  l = compose(v, &n, X(y)));
 return l; }

c2(c_la) {
 terp* j = imm;
 obj k, nom = *Sp == Pn(c_d_bind) ? Sp[1] : nil;
 with(nom, with(x, k = Ccc(v, e, m+2)));
 with(k,
  x = homp(x = ltu(v, e, nom, x)) ? x :
  (j = e && twop(loc(*e)) ? encll : encln,
   c_la_clo(v, e, X(x), Y(x))));
 return em2(j, x, k); }

c2(c_imm) { return Push(Pn(imm), x), insx(v, e, m); }

static obj c_la_clo(lips v, mem e, obj arg, obj seq) {
 i64 i = llen(arg);
 mm(&arg), mm(&seq);
 for (Push(Pn(insx), Pn(take), Pn(i), Pn(c_ini));
      twop(arg);
      Push(Pn(c_ev), X(arg), Pn(inst), Pn(push)), arg = Y(arg));
 return arg = Ccc(v, e, 0), um, um, pair(v, seq, arg); }

c1(c_d_bind) { obj y = *Sp++; return
 e ? imx(v, e, m, loc_, Pn(lidx(loc(*e), y))) :
     imx(v, e, m, tbind, y); }

static u0 c_de_r(lips v, mem e, obj x) {
 if (twop(x))
  x = rwlade(v, x),
  with(x, c_de_r(v, e, Y(Y(x)))),
  Push(Pn(c_ev), X(Y(x)), Pn(c_d_bind), X(x)); }

// syntactic sugar for define
static obj def_sug(lips v, obj x) {
 obj y = nil; return
  with(y, x = linitp(v, x, &y)),
  x = pair(v, x, y),   x = pair(v, Se, x),
  x = pair(v, x, nil), x = pair(v, La, x),
  pair(v, x, nil); }

c2(c_de) { return
 !twop(Y(x))    ? c_imm(v, e, m, nil) :
 llen(Y(x)) % 2 ? c_eval(v, e, m, def_sug(v, x)) :
                  (c_de_r(v, e, Y(x)), Ccc(v, e, m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 v->xp
#define S2 v->ip

// before generating anything, store the
// exit address in stack 2
c1(c_co_pre) {
 obj x = Ccc(v, e, m);
 return X(S2 = pair(v, x, S2)); }

// before generating a branch emit a jump to
// the top of stack 2
c1(c_co_pre_con) {
 obj x = Ccc(v, e, m+2), k = X(S2);
 return G(k) == ret ? em1(ret, x) : em2(jump, k, x); }

// after generating a branch store its address
// in stack 1
c1(c_co_post_con) {
 obj x = Ccc(v, e, m);
 return X(S1 = pair(v, x, S1)); }

// before generating an antecedent emit a branch to
// the top of stack 1
c1(c_co_pre_ant) {
 obj x = Ccc(v, e, m+2);
 return x = em2(branch, X(S1), x), S1 = Y(S1), x; }

static u0 c_co_r(lips v, mem e, obj x) {
 if (!twop(x)) x = pair(v, nil, nil);
 if (!twop(Y(x))) return Push(Pn(c_ev), X(x), Pn(c_co_pre_con));
 with(x,
  Push(_N(c_co_post_con), N_(c_ev), X(Y(x)), _N(c_co_pre_con)),
  c_co_r(v, e, Y(Y(x))));
 Push(Pn(c_ev), X(x), Pn(c_co_pre_ant)); }

c2(c_co) { return
 with(x, Push(Pn(c_co_pre))),
 c_co_r(v, e, Y(x)),
 x = Ccc(v, e, m),
 S2 = Y(S2),
 x; }

static u0 c_se_r(lips v, mem e, obj x) {
 if (twop(x))
  with(x, c_se_r(v, e, Y(x))),
  Push(Pn(c_ev), X(x)); }

c2(c_se) {
 if (!twop(x = Y(x))) x = pair(v, nil, nil);
 return c_se_r(v, e, x), Ccc(v, e, m); }

c1(c_call) {
 obj a = *Sp++, k = Ccc(v, e, m + 2);
 return em2(G(k) == ret ? rec : call, a, k); }

#define L(n,x) pair(v, Pn(n), x)
static obj look(lips v, obj e, obj y) {
 obj q; return
  nilp(e) ?
   ((q = tblget(v, Top, y)) ?  L(Here, q) : L(Wait, Top)) :
  ((q = lidx(loc(e), y)) != -1) ? L(Loc, e) :
  ((q = lidx(arg(e), y)) != -1) ? L(Arg, e) :
  ((q = lidx(clo(e), y)) != -1) ? L(Clo, e) :
  look(v, par(e), y); }
#undef L

c2(c_sy) {
 obj y, q;
 with(x, y = X(q = look(v, e ? *e:nil, x)));
 switch (Gn(y)) {
  case Here: return c_imm(v, e, m, Y(q));
  case Wait:
   x = pair(v, Y(q), x);
   with(x, y = Ccc(v, e, m+2));
   with(y, x = pair(v, Pn(8), x));
   return em2(lbind, x, y);
  default:
   if (Y(q) == *e) switch (Gn(y)) {
    case Loc: return imx(v, e, m, loc, Pn(lidx(loc(*e), x)));
    case Arg: return imx(v, e, m, arg, Pn(lidx(arg(*e), x)));
    default:  return imx(v, e, m, clo, Pn(lidx(clo(*e), x))); }
   return y = llen(clo(*e)),
          with(x, q = snoc(v, clo(*e), x)),
          clo(*e) = q,
          imx(v, e, m, clo, Pn(y)); } }

c1(c_ev) { return c_eval(v, e, m, *Sp++); }
c2(c_eval) { return (symp(x) ? c_sy : twop(x) ? c_2 : c_imm)(v, e, m, x); }

c2(c_qt) { return c_imm(v, e, m, twop(x = Y(x)) ? X(x) : x); }
c2(c_2) { obj z = X(x); return
 (z == Qt ? c_qt : z == If ? c_co : z == De ? c_de :
  z == La ? c_la : z == Se ? c_se : c_ap)(v, e, m, x); }

c2(c_ap) {
 obj mac = tblget(v, Mac, X(x));
 if (mac) {
  obj s1 = S1, s2 = S2;
  with(s1, with(s2, x = apply(v, mac, Y(x))));
  S1 = s1, S2 = s2;
  return c_eval(v, e, m, x); }
 for (mm(&x),
      Push(Pn(c_ev), X(x), Pn(inst), Pn(idH), Pn(c_call), Pn(llen(Y(x))));
      twop(x = Y(x));
      Push(Pn(c_ev), X(x), Pn(inst), Pn(push)));
 return um, Ccc(v, e, m); }

c1(inst) {
 terp* i = (terp*) Gn(*Sp++);
 return em1(i, Ccc(v, e, m+1)); }

c1(insx) {
 terp* i = (terp*) Gn(*Sp++);
 obj x = *Sp++, k;
 return with(x, k = Ccc(v, e, m+2)), em2(i, x, k); }

c1(c_ini) {
 obj k = hini(v, m+1);
 return em1((terp*)(e ? name(*e) : Eva), k); }

static NoInline obj hini(lips v, u64 n) {
 hom a = cells(v, n + 2);
 return G(a+n) = NULL,
        GF(a+n) = (terp*) a,
        set64((mem) a, nil, n),
        Ph(a+n); }

static obj hfin(lips v, obj a) {
 return (obj) (GF(button(Gh(a))) = (terp*) a); }

NoInline obj homnom(lips v, obj x) {
 terp *k = G(x);
 if (k == clos || k == pc0 || k == pc1)
  return homnom(v, (obj) G(FF(x)));
 mem h = (mem) Gh(x);
 while (*h) h++;
 x = h[-1];
 int inb = (mem) x >= v->pool && (mem) x < v->pool+v->len;
 return inb ? x : x == (obj) yield ? Eva : nil; }

NoInline u0 defprim(lips v, const char *a, terp *b) {
 obj z = pair(v, interns(v, a), nil);
 if (!Avail) with(z, reqsp(v, 1));
 *--Sp = z;
 obj x = hini(v, 2);
 x = em2(b, z = *Sp++, x);
 tblset(v, Top, X(z), x); }

static obj compile(lips v, obj x) { return
 Push(Pn(c_ev), x, Pn(inst), Pn(yield), Pn(c_ini)),
 Ccc(v, NULL, 0); }

obj eval(lips v, obj x) { return
 x = pair(v, x, nil),
 apply(v, tblget(v, Top, Eva), x); }
