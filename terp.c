#include "lips.h"
#include "terp.h"
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
#define PACK() (Ip=ip,Sp=sp,Hp=hp,Fp=fp,Xp=xp)
#define UNPACK() (fp=Fp,hp=Hp,sp=Sp,ip=Ip,xp=Xp)
#define CALLC(...)(PACK(),(__VA_ARGS__),UNPACK())

// the frame structure holds the current function context.
typedef struct fr { obj clos, retp, subd, argc, argv[]; } *fr;
#define ff(x)((fr)(x))
#define CLOS ff(fp)->clos
#define RETP ff(fp)->retp
#define SUBR ff(fp)->subd
#define ARGC ff(fp)->argc
#define ARGV ff(fp)->argv
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
#define terp_arg v,ip,fp,sp,hp,xp
#define Jump(f,...) return (f)(terp_arg,##__VA_ARGS__)
#define Cont(n, x) return ip+=w2b(n), xp=x, G(ip)(terp_arg)
#define AP(f,x) return ip=f,xp=x,G(ip)(terp_arg)
#define GO(f,x) return xp=x,f(terp_arg)
#define NEXT(n) ip+=w2b(n);AP(ip, xp)
#define ok Pn(1)
// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function
static interp(gc) { u64 n = Xp; CALLC(reqsp(v, n)); NEXT(0); }
// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.
#define avail (sp-hp)
#define Have(n) if (avail < n) Jump((Xp=n,gc))
#define Have1() if (hp == sp) Jump((Xp=1,gc)) // common case, faster comparison

#define arity_err_msg "wrong arity : %d of %d"
#define type_err_msg "wrong type : %s for %s"
#define div0_err_msg "%d / 0"
#define TERP(n, m, ...) interp(n) m(__VA_ARGS__)

// jump to nope() when an error happens.
static interp(nope, const char *, ...);

// type check
#define TYP(x,t) if(kind((x))-(t))\
 Jump(nope, type_err_msg, tnom(kind(x)), tnom(t))
// arity check
#define ARY(n) if(Pn(n)>ARGC)\
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
OP2(loc, REF(AR(LOCS))) OP1(loc0, AR(LOCS)[0]) OP1(loc1, AR(LOCS)[1])
OP2(clo, REF(AR(CLOS))) OP1(clo0, AR(CLOS)[0]) OP1(clo1, AR(CLOS)[1])

// store instructions
interp(push) { Have1(); *--sp = xp; NEXT(1); } // stack push
interp(loc_) { REF(AR(LOCS)) = xp; NEXT(2); } // set a local variable
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
 obj w = (obj) GF(ip), d = XY(w), y = X(w);
 if (!(w = tblget(v, d, xp = YY(w)))) {
  char *nom = nilp(getsym(xp)->nom) ? "<anon>" : symnom(xp);
  Jump(nope, "free variable : %s", nom); }// free variable

 xp = w;
 if (getnum(y) != 8) TYP(xp, getnum(y)); // do the type check
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
#define B(_x_, a, x, b, y) {if(_x_)AP((obj)a(ip),x);AP((obj)b(ip),y);}
#define BR(op, a, x, b, y) B(*sp++ op xp, a, x, b, y)

interp(branch)  B(xp == nil, FF, xp, GF, xp)
interp(barnch)  B(xp == nil, GF, xp, FF, xp)
interp(breq)    B(eql(*sp++, xp), GF, ok, FF, nil)
interp(brne)    B(eql(*sp++, xp), FF, ok, GF, nil)

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
 Have(Size(fr));
 obj adic = (obj) GF(ip);
 i64 off = fp - (mem) ((i64) sp + adic - Num);
 fp = sp -= Size(fr);
 RETP = Ph(ip+W2);
 SUBR = Pn(off);
 CLOS = nil;
 ARGC = adic;
 AP(xp, nil); }

static interp(recne) {
 // overwrite current frame with new frame
 Xp = SUBR, Ip = RETP; // save return info
 fp = ARGV + Gn(ARGC - ip);
 cpy64r(fp, sp, Gn(ip)); // copy from high to low
 sp = fp -= Size(fr);
 RETP = Ip;
 ARGC = ip;
 SUBR = Xp;
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
 obj x;
 ARY(1);
 TYP(x=ARGV[0], Hom);
 // we need space for:
 // the entire stack
 // the frame offset
 // the length (to put it all in a tuple)
 // the continuation thread (4 words)
 i64 ht = Pool + Len - sp;
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
 AP(x, nil); }

// call a continuation
interp(cont) {
 vec t = getvec(GF(ip));
 Have(t->len - 1);
 xp = N(ARGC) == 0 ? nil : *ARGV;
 i64 off = N(t->xs[0]);
 sp = Pool + Len - (t->len - 1);
 fp = sp + off;
 cpy64(sp, t->xs+1, t->len-1);
 Jump(ret); }

interp(ap_u) {
 ARY(2);
 obj x = ARGV[0], y = ARGV[1];
 TYP(x, Hom);
 u64 adic = llen(y);
 Have(adic);
 obj off = SUBR, rp = RETP;
 sp = ARGV + Gn(ARGC) - adic;
 for (u64 j = 0; j < adic; y = Y(y)) sp[j++] = X(y);
 fp = sp -= Size(fr);
 RETP = rp;
 ARGC = N_(adic);
 SUBR = off;
 CLOS = nil;
 AP(x, nil); }

// instructions used by the compiler
interp(hom_u) {
 obj x;
 ARY(1);
 TYP(x = *ARGV, Num);
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
 TYP(ARGV[0], Vec);
 TYP(ARGV[1], Num);
 num idx = getnum(ARGV[1]);
 vec ary = getvec(ARGV[0]);
 if (idx < 0 || idx >= ary->len) Jump(nope, "oob : %d#%d");
 xp = ary->xs[idx] = ARGV[2];
 Jump(ret); }

interp(vget_u) {
 ARY(2);
 TYP(ARGV[0], Vec);
 TYP(ARGV[1], Num);
 num idx = getnum(ARGV[1]);
 vec ary = getvec(ARGV[0]);
 if (idx < 0 || idx >= ary->len) Jump(nope, "oob : %d#%d");
 xp = ary->xs[idx];
 Jump(ret); }

interp(vec_u) {
 obj n = N(ARGC);
 Have(n + 1);
 vec t = (vec) hp;
 hp += 1 + n;
 cpy64(t->xs, ARGV, t->len = n);
 GO(ret, putvec(t)); }

interp(tset) {
 obj x = *sp++, y = *sp++;
 CALLC(x = tblset(v, xp, x, y));
 AP(ip+W, x); }

interp(emx) { obj h = *sp++ - W; G(h) = (terp*) xp;    AP(ip+W, h); }
interp(emi) { obj h = *sp++ - W; G(h) = (terp*) N(xp); AP(ip+W, h); }

interp(emx_u) {
 ARY(2);
 obj h = ARGV[1];
 TYP(h, Hom);
 h -= W;
 G(h) = (terp*) ARGV[0];
 GO(ret, h); }

interp(emi_u) {
 ARY(2);
 TYP(ARGV[0], Num);
 obj h = ARGV[1];
 TYP(h, Hom);
 h -= W;
 G(h) = (terp*) Gn(ARGV[0]);
 GO(ret, h); }

interp(hgeti_u) { ARY(1); TYP(ARGV[0], Hom); GO(ret,   N_(G(ARGV[0]))); }
interp(hgetx_u) { ARY(1); TYP(ARGV[0], Hom); GO(ret, (obj)G(ARGV[0])); }

interp(hseek_u) {
 ARY(2);
 TYP(ARGV[0], Hom);
 TYP(ARGV[1], Num);
 GO(ret, H_(H(ARGV[0])+N(ARGV[1]))); }

// hash tables
interp(tblg) {
 ARY(2);
 TYP(ARGV[0], Tbl);
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
interp(tkeys) { CALLC(Xp = tblkeys(v, xp)); NEXT(1); }

interp(tblc) {
 ARY(2);
 TYP(ARGV[0], Tbl);
 xp = tblget(v, ARGV[0], ARGV[1]);
 GO(ret, xp ? ok : nil); }

static obj tblss(lips v, i64 i, i64 l) {
 mem fp = Fp;
 return i > l-2 ? ARGV[i-1] :
  (tblset(v, Xp, ARGV[i], ARGV[i+1]),
   tblss(v, i+2, l)); }

interp(tbls) {
 ARY(1);
 xp = *ARGV;
 TYP(xp, Tbl);
 CALLC(Xp = tblss(v, 1, N(ARGC)));
 Jump(ret); }

interp(tblmk) {
 CALLC(Xp = table(v), tblss(v, 0, N(ARGC)));
 Jump(ret); }

interp(tbld) {
 ARY(2);
 TYP(ARGV[0], Tbl);
 CALLC(Xp = tbldel(v, ARGV[0], ARGV[1]));
 Jump(ret); }

interp(tblks) {
 ARY(1);
 TYP(*ARGV, Tbl);
 CALLC(Xp = tblkeys(v, *ARGV));
 Jump(ret); }

interp(tbll) {
 ARY(1);
 TYP(*ARGV, Tbl);
 GO(ret, N_(gettbl(*ARGV)->len)); }

// string instructions
interp(strl) {
 ARY(1);
 TYP(*ARGV, Str);
 GO(ret, N_(getstr(*ARGV)->len-1)); }

interp(strg) {
 ARY(2);
 TYP(ARGV[0], Str);
 TYP(ARGV[1], Num);
 GO(ret, N(ARGV[1]) < getstr(ARGV[0])->len-1 ?
  N_(getstr(ARGV[0])->text[N(ARGV[1])]) :
  nil); }

interp(strconc) {
 i64 l = Gn(ARGC), sum = 0, i = 0;
 while (i < l) {
  obj x = ARGV[i++];
  TYP(x, Str);
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
 TYP(ARGV[0], Str);
 TYP(ARGV[1], Num);
 TYP(ARGV[2], Num);
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
  TYP(x, Num);
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
  cpy64(fp-1, fp, Size(fr) + Gn(ARGC));
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
   t[i].x = ARGV[reqd + i],
   t[i].y = puttwo(t+i+1));
  t[vdic-1].y = nil;
  ARGV[reqd] = puttwo(t); }
 NEXT(2); }

// the next few functions create and store
// lexical environments.
static interp(encl) {
 i64 n = Xp;
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
 Xp = n;
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
     arg = AR(ec)[0],
     loc = AR(ec)[1];
 u64 adic = nilp(arg) ? 0 : AL(arg);
 Have(Size(fr) + adic + 1);
 i64 off = (mem) fp - sp;
 G(ip) = pc1;
 sp -= adic;
 cpy64(sp, AR(arg), adic);
 ec = (obj) GF(ip);
 fp = sp -= Size(fr);
 RETP = ip;
 SUBR = Pn(off);
 ARGC = Pn(adic);
 CLOS = AR(ec)[2];
 if (!nilp(loc)) *--sp = loc;
 ip = AR(ec)[3];
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

interp(car_u) { ARY(1); TYP(*ARGV, Two); GO(ret, X(*ARGV)); }
interp(cdr_u) { ARY(1); TYP(*ARGV, Two); GO(ret, Y(*ARGV)); }
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
  x = *xs++; TYP(x, Num);}\
 GO(ret, Pn(xp));}
#define mm_u0(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op Gn(x)){\
  x = *xs++; TYP(x, Num);\
  if (x == Pn(0)) Jump(nope, div0_err_msg, xp);}\
 GO(ret, Pn(xp));}

#define UBINOP(nom, dflt, op)\
 interp(nom##_u) { mm_u(Gn(ARGC), ARGV, dflt, op); }
UBINOP(add, 0, +) UBINOP(mul, 1, *)
UBINOP(band, -1, &) UBINOP(bor, 0, |) UBINOP(bxor, 0, ^)

interp(sub_u) {
 if (!(xp = Gn(ARGC))) GO(ret, Pn(0));
 TYP(*ARGV, Num);
 if (xp == 1) GO(ret, Pn(-Gn(*ARGV)));
 mm_u(xp-1,ARGV+1,Gn(*ARGV),-); }

interp(div_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TYP(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),/); }
interp(mod_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TYP(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),%); }

interp(sar_u) {
 if (ARGC == N_(0)) GO(ret, Pn(0));
 TYP(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), >>); }
interp(sal_u) {
 if (ARGC == N_(0)) GO(ret, N_(0));
 TYP(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), <<); }

// type predicates
#define TP(ty) interp(ty##pp) { AP(ip+W, (ty##p(xp)?ok:nil)); }
TP(num) TP(hom) TP(two) TP(sym) TP(str) TP(tbl) TP(nil) TP(vec)

bool eql(obj a, obj b) {
 if (a == b)             return true;
 if (kind(a) != kind(b)) return false;
 switch (kind(a)) {
  case Two:
   // pairs are immutable, so we can take this opportunity to
   // deduplicate them.
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
// there should be a separate instruction for simple equality.
BINOP(eq, eql(xp, *sp++) ? ok : nil)

static interp(ord_) {
 bool (*r)(obj, obj) = (void*)Xp;
 obj n=Gn(ARGC),*xs=ARGV,m,*l;
 switch(n){
  case 0:no:GO(ret, nil);
  default:for(l=xs+n-1,m=*xs;xs<l;m=*++xs)if(!(r(m,xs[1])))goto no;
  case 1:break;}
 GO(ret, ok);}

#define ord_w(r)Xp=(obj)r;Jump(ord_)
#define cmp(op, n)\
 static bool cmp_##n(obj a, obj b) { return a op b; }\
 interp(n##_u) { Xp=(obj)cmp_##n;Jump(ord_); }
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
 TYP(xp, Sym);
 GO(ret, getsym(xp)->nom); }

// errors
interp(fail) { Jump(nope, "fail"); }
interp(gsym_u) {
 if (ARGC > Pn(0) && strp(*ARGV)) {
  xp = *ARGV;
  CALLC(Xp = intern(v, xp));
  Jump(ret); }
 Have(Size(sym));
 sym y = (sym) hp;
 hp += Size(sym);
 y->nom = y->l = y->r = nil;
 y->code = v->count++ * mix;
 GO(ret, putsym(y)); }

interp(hfin_u) {
 ARY(1);
 obj a = *ARGV;
 TYP(ARGV[0], Hom);
 GF(button(Gh(a))) = (terp*) a;
 GO(ret, a); }

interp(ev_u) {
 ARY(1);
 CALLC(xp = compile(v, *ARGV),
       Xp = G(xp)(v, xp, Fp, Sp, Hp, nil));
 Jump(ret); }

interp(rnd_u) { GO(ret, Pn(lcprng(&v->seed))); }

// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
static Inline u0 perrarg(lips v, mem fp) {
 i64 i = 0, argc = fp == Pool + Len ? 0 : getnum(ARGC);
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
  ip = RETP, fp += Size(fr) + getnum(ARGC) + getnum(SUBR);
  if (button(Gh(ip))[-1] == yield) break;
  fputs("# in ", stderr), emsep(v, Ph(ip), stderr, '\n'); }
 Hp = hp;
 return restart(v); }

noreturn obj restart(lips v) {
 v->fp = v->sp = v->mem_pool + v->mem_len;
 v->xp = v->ip = nil;
 v->mem_root = NULL;
 longjmp(*v->restart, 1); }
