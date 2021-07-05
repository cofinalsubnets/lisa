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
#define Pack() (Ip=ip,Sp=sp,Hp=hp,Fp=fp,Xp=xp)
#define Unpack() (fp=Fp,hp=Hp,sp=Sp,ip=Ip,xp=Xp)
#define CallC(...)(Pack(),(__VA_ARGS__),Unpack())

// the frame structure holds the current function context.
typedef struct fr { obj clos, retp, subd, argc, argv[]; } *fr;
#define ff(x)((fr)(x))
#define Clos ff(fp)->clos
#define Retp ff(fp)->retp
#define Subd ff(fp)->subd
#define Argc ff(fp)->argc
#define Argv ff(fp)->argv
// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer.
#define Locs fp[-1]
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
#define Ap(f,x) return ip=f,xp=x,G(ip)(terp_arg)
#define Go(f,x) return xp=x,f(terp_arg)
#define N(n) ip+=w2b(n);Ap(ip, xp)
// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function
static interp(gc) { u64 n = Xp; CallC(reqsp(v, n)); N(0); }
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

// jump to nope() when an error happens.
static interp(nope, const char *, ...);

#define TyCh(x,t) if(kind((x))-(t))\
 Jump(nope, type_err_msg,\
  tnom(kind(x)), tnom(t)) // type check
#define TyChN(x,t,n) if(kind((x))-(t))\
 Jump(nope, type_err_msg,\
  tnom(kind(x)), n) // type check
#define TyNum(x) TyChN(x,Num,"num")
#define TyTwo(x) TyChN(x,Two,"two")
#define TyHom(x) TyChN(x,Hom,"hom")
#define TyTbl(x) TyChN(x,Tbl,"tbl")
#define TyStr(x) TyChN(x,Str,"str")
#define ArCh(n) if(Pn(n)>Argc)\
 Jump(nope, arity_err_msg,\
  Gn(Argc), n)

// " virtual machine instructions "
//
// load instructions
interp(imm) { xp = (obj) GF(ip); N(2); }
// common constants
interp(unit) { xp = nil;   N(1); }
interp(one)  { xp = Pn(1); N(1); }
interp(zero) { xp = Pn(0); N(1); }

// indexed load instructions
// this pointer arithmetic works because fixnums are
// premultiplied by W
#define fast_idx(b) (*(i64*)((i64)(b)+(i64)GF(ip)-Num))
interp(arg)  { xp = fast_idx(Argv);     N(2); }
interp(arg0) { xp = Argv[0];            N(1); }
interp(arg1) { xp = Argv[1];            N(1); }
interp(loc)  { xp = fast_idx(AR(Locs)); N(2); }
interp(loc0) { xp = AR(Locs)[0];        N(1); }
interp(loc1) { xp = AR(Locs)[1];        N(1); }
interp(clo)  { xp = fast_idx(AR(Clos)); N(2); }
interp(clo0) { xp = AR(Clos)[0];        N(1); }
interp(clo1) { xp = AR(Clos)[1];        N(1); }

// store instructions
interp(push) { Have1(); *--sp = xp; N(1); } // stack push
interp(loc_) { fast_idx(AR(Locs)) = xp; N(2); } // set a local variable
interp(tbind) { CallC(tblset(v, Dict, (obj) GF(ip), xp)); N(2); }

// initialize local variable slots
interp(locals) {
 u64 n = Gn(GF(ip));
 Have(n + 2);
 vec t = (vec) hp;
 set64(t->xs, nil, t->len = n);
 hp += n + 1;
 *--sp = puttup(t);
 N(2); }

// late bind
// this function is a lil complicated, because it incorporates
// the "static" type and arity checking that would have been
// done by the compiler if the function had been bound early.
interp(lbind) {
 obj w = (obj) GF(ip),
     d = XY(w), y = X(w);
 if (!(w = tblget(v, d, xp = YY(w))))
  Jump(nope, "free variable : %s", symnom(xp)); // free variable
 xp = w;
 if (Gn(y) != 8) TyCh(xp, Gn(y)); // do the type check
 terp *q = G(FF(ip)); // omit the arity check if possible
 if (q == call || q == rec) {
  obj aa = (obj) GF(FF(ip));
  if (G(xp) == arity && aa >= (obj) GF(xp))
   xp += W2; }
 G(ip) = imm;
 GF(ip) = (terp*) xp;
 N(2); }

// control flow instructions
// return to C
interp(yield) { Pack(); return xp; }

// conditional jumps
interp(branch) { Ap(xp == nil ? (obj) FF(ip) : (obj) GF(ip), xp); }
interp(barnch) { Ap(xp == nil ? (obj) GF(ip) : (obj) FF(ip), xp); }
// relational jumps
interp(brlt)   { Ap(*sp++ <  xp    ? (obj) GF(ip) : (obj) FF(ip), xp); }
interp(brgteq) { Ap(*sp++ <  xp    ? (obj) FF(ip) : (obj) GF(ip), xp); }
interp(brlteq) { Ap(*sp++ <= xp    ? (obj) GF(ip) : (obj) FF(ip), xp); }
interp(brgt)   { Ap(*sp++ <= xp    ? (obj) FF(ip) : (obj) GF(ip), xp); }
interp(breq)   { Ap(eql(*sp++, xp) ? (obj) GF(ip) : (obj) FF(ip), xp); }
interp(brne)   { Ap(eql(*sp++, xp) ? (obj) FF(ip) : (obj) GF(ip), xp); }

// unconditional jumps
interp(jump) { Ap((obj) GF(ip), xp); }
interp(clos) { Clos = (obj) GF(ip); Ap((obj) G(FF(ip)), xp); }

// return from a function
interp(ret) {
 ip = Retp;
 sp = (mem) ((i64) Argv + Argc - Num);
 fp = (mem) ((i64)   sp + Subd - Num);
 N(0); }

// regular function call
interp(call) {
#define fwds (sizeof(struct fr)/W)
 Have(fwds);
 obj adic = (obj) GF(ip);
 i64 off = fp - (mem) ((i64) sp + adic - Num);
 fp = sp -= fwds;
 Retp = Ph(ip+W2);
 Subd = Pn(off);
 Clos = nil;
 Argc = adic;
 Ap(xp, nil); }

static interp(recne) {
 // overwrite current frame with new frame
 Xp = Subd, Ip = Retp; // save return info
 fp = Argv + Gn(Argc - ip);
 cpy64r(fp, sp, Gn(ip)); // copy from high to low
 sp = fp -= Size(fr);
 Retp = Ip;
 Argc = ip;
 Subd = Xp;
 ip = xp;
 Clos = xp = nil;
 N(0); }

// tail call
interp(rec) {
 ip = (obj) GF(ip);
 if (Argc!=ip) Jump(recne);
 cpy64(Argv, sp, ip = Gn(ip));
 sp = fp;
 Ap(xp, nil); }

// type/arity checking
#define tcn(k, n) if(!kind(xp-k)){N(1);}\
 Jump(nope, type_err_msg, tnom(kind(xp)), n)
interp(idZ) { tcn(Num, "num"); }
interp(id2) { tcn(Two, "two"); }
interp(idH) { tcn(Hom, "hom"); }
interp(idT) { tcn(Tbl, "tbl"); }
interp(arity) {
 obj reqd = (obj) GF(ip);
 if (reqd <= Argc) { N(2); }
 Jump(nope, arity_err_msg, Gn(Argc), Gn(reqd)); }

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
 ArCh(1);
 TyHom(x=Argv[0]);
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
 c[1] = (terp*) puttup(t);
 c[2] = NULL;
 c[3] = (terp*) c;
 Argv[0] = Ph(c);
 Ap(x, nil); }

// call a continuation
interp(cont) {
 vec t = gettup(GF(ip));
 Have(t->len - 1);
 xp = Gn(Argc) == 0 ? nil : *Argv;
 i64 off = Gn(t->xs[0]);
 sp = Pool + Len - (t->len - 1);
 fp = sp + off;
 cpy64(sp, t->xs+1, t->len-1);
 Jump(ret); }

interp(ap_u) {
 ArCh(2);
 obj x = Argv[0], y = Argv[1];
 TyHom(x);
 u64 adic = llen(y);
 Have(adic);
 obj off = Subd, rp = Retp;
 sp = Argv + Gn(Argc) - adic;
 for (u64 j = 0; j < adic; y = Y(y)) sp[j++] = X(y);
 fp = sp -= Size(fr);
 Retp = rp;
 Argc = Pn(adic);
 Subd = off;
 Clos = nil;
 Ap(x, nil); }

// instructions used by the compiler
interp(hom_u) {
 obj x;
 ArCh(1);
 TyNum(x = *Argv);
 i64 len = Gn(x) + 2;
 Have(len);
 hom h = (hom) hp;
 hp += len;
 set64((mem) h, nil, len);
 h[len-1] = (terp*) h;
 h[len-2] = NULL;
 Go(ret, Ph(h+len-2)); }

interp(tset) {
 obj x = *sp++, y = *sp++;
 CallC(x = tblset(v, xp, x, y));
 Ap(ip+W, x); }

interp(emx) {
 obj h = *sp++ - W;
 G(h) = (terp*) xp;
 Ap(ip+W, h); }

interp(emi) {
 obj h = *sp++ - W;
 G(h) = (terp*) Gn(xp);
 Ap(ip+W, h); }

interp(emx_u) {
 ArCh(2);
 obj h = Argv[1];
 TyHom(h);
 h -= W;
 G(h) = (terp*) Argv[0];
 Go(ret, h); }

interp(emi_u) {
 ArCh(2);
 TyNum(Argv[0]);
 obj h = Argv[1];
 TyHom(h);
 h -= W;
 G(h) = (terp*) Gn(Argv[0]);
 Go(ret, h); }

interp(hom_geti_u) {
 ArCh(1);
 TyHom(Argv[0]);
 Go(ret, Pn(G(Argv[0]))); }

interp(hom_getx_u) {
 ArCh(1);
 TyHom(Argv[0]);
 Go(ret, (obj) G(Argv[0])); }

interp(hom_seek_u) {
 ArCh(2);
 TyHom(Argv[0]);
 TyNum(Argv[1]);
 Go(ret, Ph(Gh(Argv[0])+Gn(Argv[1]))); }

// hash tables
interp(tblg) {
 ArCh(2);
 TyTbl(Argv[0]);
 xp = tblget(v, Argv[0], Argv[1]);
 Go(ret, xp ? xp : nil); }

interp(tget) {
 xp = tblget(v, xp, *sp++);
 Ap(ip+W, xp ? xp : nil); }

static obj tblkeys_j(lips v, tble e, obj l) {
 obj x;
 if (!e) return l;
 x = e->key;
 with(x, l = tblkeys_j(v, e->next, l));
 return pair(v, x, l); }

static obj tblkeys_i(lips v, obj t, i64 i) {
 obj k;
 if (i == gettbl(t)->cap) return nil;
 with(t, k = tblkeys_i(v, t, i+1));
 return tblkeys_j(v, gettbl(t)->tab[i], k); }

static Inline obj tblkeys(lips v, obj t) {
 return tblkeys_i(v, t, 0); }

#define ok Pn(1)
interp(thas)  { xp = tblget(v, xp, *sp++) ? ok : nil; N(1); }
interp(tlen)  { xp = putnum(gettbl(xp)->len); N(1); }
interp(tkeys) { obj x; CallC(x = tblkeys(v, xp)); xp = x; N(1); }

interp(tblc) {
 ArCh(2);
 TyTbl(Argv[0]);
 xp = tblget(v, Argv[0], Argv[1]);
 Go(ret, xp ? Pn(0) : nil); }

static obj tblss(lips v, i64 i, i64 l) {
 mem fp = Fp;
 return i > l-2 ? Argv[i-1] :
  (tblset(v, Xp, Argv[i], Argv[i+1]),
   tblss(v, i+2, l)); }

interp(tbls) {
 obj x = nil;
 ArCh(1);
 xp = *Argv;
 TyTbl(xp);
 CallC(x = tblss(v, 1, Gn(Argc)));
 Go(ret, x); }

interp(tblmk) {
 CallC(Xp = table(v), tblss(v, 0, Gn(Argc)));
 Go(ret, Xp); }

interp(tbld) {
 obj x = nil;
 ArCh(2);
 TyTbl(Argv[0]);
 CallC(x = tbldel(v, Argv[0], Argv[1]));
 Go(ret, x); }

interp(tblks) {
 ArCh(1);
 TyTbl(*Argv);
 obj x;
 CallC(x = tblkeys(v, *Argv));
 Go(ret, x); }

interp(tbll) {
 ArCh(1);
 TyTbl(*Argv);
 Go(ret, Pn(gettbl(*Argv)->len)); }

// string instructions
interp(strl) {
 ArCh(1);
 TyStr(*Argv);
 Go(ret, Pn(getoct(*Argv)->len-1)); }

interp(strg) {
 ArCh(2);
 TyStr(Argv[0]);
 TyNum(Argv[1]);
 Go(ret, Gn(Argv[1]) < getoct(Argv[0])->len-1 ?
  Pn(getoct(Argv[0])->text[Gn(Argv[1])]) :
  nil); }

interp(strconc) {
 i64 l = Gn(Argc), sum = 0, i = 0;
 while (i < l) {
  obj x = Argv[i++];
  TyStr(x);
  sum += getoct(x)->len - 1; }
 i64 words = b2w(sum+1) + 1;
 Have(words);
 str d = (str) hp;
 hp += words;
 d->len = sum + 1;
 d->text[sum] = 0;
 while (i) {
  str x = getoct(Argv[--i]);
  sum -= x->len - 1;
  cpy8(d->text+sum, x->text, x->len - 1); }
 Go(ret, putoct(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
interp(strs) {
 ArCh(3);
 TyStr(Argv[0]);
 TyNum(Argv[1]);
 TyNum(Argv[2]);
 str src = getoct(Argv[0]);
 i64 lb = Gn(Argv[1]), ub = Gn(Argv[2]);
 lb = max(lb, 0);
 ub = max(min(ub, src->len-1), lb);
 i64 words = 1 + b2w(ub - lb + 1);
 Have(words);
 str dst = (str) hp;
 hp += words;
 dst->len = ub - lb + 1;
 dst->text[ub - lb] = 0;
 cpy8(dst->text, src->text + lb, ub - lb);
 Go(ret, putoct(dst)); }

interp(strmk) {
 i64 i = 0, l = Gn(Argc)+1, size = 1 + b2w(l);
 Have(size);
 str s = (oct) hp;
 hp += size;
 for (obj x; i < l-1; s->text[i++] = Gn(x)) {
  x = Argv[i];
  TyNum(x);
  if (x == Pn(0)) break; }
 s->text[i] = 0;
 s->len = i+1;
 Go(ret, putoct(s)); }

interp(vararg) {
 i64 reqd = Gn(GF(ip)),
     vdic = Gn(Argc) - reqd;
 ArCh(reqd);
 // in this case we need to add another argument
 // slot to hold the nil.
 if (!vdic) {
  Have1();
  sp = --fp;
  cpy64(fp, fp+1, Size(fr));
  Argc += W;
  Argv[reqd] = nil; }
 // in this case we just keep the existing slots.
 // the path is knowable at compile time in many cases
 // so maybe vararg should be two or more different
 // functions.
 else {
  Have(2 * vdic);
  two t = (two) hp;
  hp += 2 * vdic;
  for (i64 i = vdic; i--;
   t[i].x = Argv[reqd + i],
   t[i].y = puttwo(t+i+1));
  t[vdic-1].y = nil;
  Argv[reqd] = puttwo(t); }
 N(2); }

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
  while (n--) t->xs[n] = Argv[n];
  arg = puttup(t); }

 vec t = (vec) block; // compiler thread closure array (1 length 5 elements)
 hom at = (hom) (block+6); // compiler thread (1 instruction 2 data 2 tag)

 t->len = 5; // initialize alpha closure
 t->xs[0] = arg;
 t->xs[1] = xp; // Locs or nil
 t->xs[2] = Clos;
 t->xs[3] = Y(x);
 t->xs[4] = Ph(at);

 at[0] = pc0;
 at[1] = (terp*) puttup(t);
 at[2] = (terp*) X(x);
 at[3] = 0;
 at[4] = (terp*) at;

 Ap(ip+W2, Ph(at)); }

interp(prencl) {
 u64 n = Gn(Argc);
 n += n ? 12 : 11;
 Have(n);
 Xp = n;
 Jump(encl); }

interp(encll) { Go(prencl, Locs); }
interp(encln) { Go(prencl, nil); }

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
 Retp = ip;
 Subd = Pn(off);
 Argc = Pn(adic);
 Clos = AR(ec)[2];
 if (!nilp(loc)) *--sp = loc;
 ip = AR(ec)[3];
 N(0); }

// finalize function instance closure
interp(pc1) {
 G(ip) = clos;
 GF(ip) = (terp*) xp;
 N(0); }

// this is used to create closures.
interp(take) {
 u64 n = Gn((obj) GF(ip));
 Have(n + 1);
 vec t = (vec) hp;
 hp += n + 1;
 t->len = n;
 cpy64(t->xs, sp, n);
 sp += n;
 Go(ret, puttup(t)); }

// print to console
interp(em_u) {
 u64 l = Gn(Argc), i;
 if (l) {
  for (i = 0; i < l - 1; i++)
   emsep(v, Argv[i], stdout, ' ');
  emit(v, xp = Argv[i], stdout); }
 fputc('\n', stdout);
 Jump(ret); }

// pairs
interp(cons) {
 Have1(); hp[0] = xp, hp[1] = *sp++;
 xp = puttwo(hp); hp += 2; N(1); }
interp(car) { Ap(ip+W, X(xp)); }
interp(cdr) { Ap(ip+W, Y(xp)); }

interp(cons_u) {
 ArCh(2); Have(2);
 hp[0] = Argv[0], hp[1] = Argv[1];
 xp = puttwo(hp), hp += 2; Jump(ret); }
interp(car_u) { ArCh(1); TyTwo(*Argv); Go(ret, X(*Argv)); }
interp(cdr_u) { ArCh(1); TyTwo(*Argv); Go(ret, Y(*Argv)); }

// arithmetic
interp(neg) { Ap(ip+W, Pn(-Gn(xp))); }
interp(add) { xp = xp + *sp++ - Num; N(1); }
interp(sub) { xp = *sp++ - xp + Num; N(1); }
interp(mul) { xp = Pn(Gn(xp) * Gn(*sp++)); N(1); }
interp(dqv) {
 if (xp == Pn(0)) Jump(nope, div0_err_msg, Gn(*sp));
 xp = Pn(Gn(*sp++) / Gn(xp));
 N(1); }
interp(mod) {
 if (xp == Pn(0)) Jump(nope, div0_err_msg, Gn(*sp));
 xp = Pn(Gn(*sp++) % Gn(xp));
 N(1); }

interp(sar) { xp = Pn(Gn(*sp++) >> Gn(xp)); N(1); }
interp(sal) { xp = Pn(Gn(*sp++) << Gn(xp)); N(1); }
interp(band) { xp &= *sp++; N(1); }
interp(bor) { xp |= *sp++; N(1); }
interp(bxor) { xp ^= *sp++; xp |= Num; N(1); }

#define mm_u(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op Gn(x)){\
  x = *xs++; TyNum(x);}\
 Go(ret, Pn(xp));}
#define mm_u0(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op Gn(x)){\
  x = *xs++; TyNum(x);\
  if (x == Pn(0)) Jump(nope, div0_err_msg, xp);}\
 Go(ret, Pn(xp));}

interp(add_u) {
 mm_u(Gn(Argc), Argv, 0, +); }
interp(mul_u) {
 mm_u(Gn(Argc), Argv, 1, *); }
interp(sub_u) {
 if (!(xp = Gn(Argc))) Go(ret, Pn(0));
 TyNum(*Argv);
 if (xp == 1) Go(ret, Pn(-Gn(*Argv)));
 mm_u(xp-1,Argv+1,Gn(*Argv),-); }

interp(div_u) {
 if (!(xp = Gn(Argc))) Go(ret, Pn(1));
 TyNum(*Argv);
 mm_u0(xp-1,Argv+1,Gn(*Argv),/); }
interp(mod_u) {
 if (!(xp = Gn(Argc))) Go(ret, Pn(1));
 TyNum(*Argv);
 mm_u0(xp-1,Argv+1,Gn(*Argv),%); }

interp(sar_u) {
 if (Argc == Pn(0)) Go(ret, Pn(0));
 TyNum(*Argv);
 mm_u(Gn(Argc)-1, Argv+1, Gn(*Argv), >>); }
interp(sal_u) {
 if (Argc == Pn(0)) Go(ret, Pn(0));
 TyNum(*Argv);
 mm_u(Gn(Argc)-1, Argv+1, Gn(*Argv), <<); }
interp(band_u) {
 mm_u(Gn(Argc), Argv, (~0), &); }
interp(bor_u) {
 mm_u(Gn(Argc), Argv, 0, |); }
interp(bxor_u) {
 mm_u(Gn(Argc), Argv, 0, ^); }

#define Tf(x) ((x)?ok:nil)
// type predicates
interp(numpp) { Ap(ip+W, Tf(nump(xp))); }
interp(hompp) { Ap(ip+W, Tf(homp(xp))); }
interp(twopp) { Ap(ip+W, Tf(twop(xp))); }
interp(sympp) { Ap(ip+W, Tf(symp(xp))); }
interp(strpp) { Ap(ip+W, Tf(octp(xp))); }
interp(tblpp) { Ap(ip+W, Tf(tblp(xp))); }
interp(nilpp) { Ap(ip+W, Tf(nilp(xp))); }
interp(vecpp) { Ap(ip+W, Tf(tupp(xp))); }


// pairs are immutable, so we can take this opportunity to
// deduplicate them.
static u64 twoeq(obj a, obj b) {
 if (!eql(X(a), X(b))) return false;
 X(a) = X(b);
 if (!eql(Y(a), Y(b))) return false;
 Y(a) = Y(b);
 return true; }

static u64 streq(obj a, obj b) {
 str o = getoct(a), m = getoct(b);
 if (o->len != m->len) return false;
 for (i64 i = 0; i < o->len; i++)
  if (o->text[i] != m->text[i]) return false;
 return true; }

u64 eql(obj a, obj b) {
 if (a == b)             return true;
 if (kind(a) != kind(b)) return false;
 switch (kind(a)) {
  case Two: return twoeq(a, b);
  case Str: return streq(a, b);
  default:  return false; } }

interp(lt)    { xp = *sp++ <  xp ? xp : nil; N(1); }
interp(lteq)  { xp = *sp++ <= xp ? xp : nil; N(1); }
interp(gteq)  { xp = *sp++ >= xp ? xp : nil; N(1); }
interp(gt)    { xp = *sp++ >  xp ? xp : nil; N(1); }
// there should be a separate instruction for simple equality.
interp(eq)    { xp = eql(xp, *sp++) ? ok : nil; N(1); }

#define ord_w(r){\
 obj n=Gn(Argc),*xs=Argv,m,*l;\
 switch (n){\
  case 0: no: Go(ret, nil);\
  case 1: break;\
  default: for (l=xs+n-1,m=*xs;xs<l;m=*++xs)\
           if(!(m r xs[1])) goto no;}\
 Go(ret, ok);}

#define ord_wv(r){\
 obj n=Gn(Argc),*xs=Argv,m,*l;\
 switch(n) {\
  case 0: Go(ret, nil);\
  case 1: break;\
  default: for (l=xs+n-1,m=*xs;xs<l;m=*++xs)\
           if (!(r(m,xs[1]))) Go(ret, nil);}\
 Go(ret, ok);}

#define ord_v(r) Go(ret, ord_u(Gn(Argc), Argv, r))

interp(lt_u)   { ord_w(<); }
interp(lteq_u) { ord_w(<=); }
interp(eq_u)   { ord_wv(eql); }
interp(gteq_u) { ord_w(>=); }
interp(gt_u)   { ord_w(>); }

#define typpp(t) {\
 for (obj *xs = Argv, *l=xs+Gn(Argc);xs<l;)\
  if (kind(*xs++)!=t) Go(ret, nil);\
 Go(ret, ok); }
interp(nump_u) { typpp(Num); }
interp(homp_u) { typpp(Hom); }
interp(strp_u) { typpp(Str); }
interp(tblp_u) { typpp(Tbl); }
interp(twop_u) { typpp(Two); }
interp(symp_u) { typpp(Sym); }
interp(nilp_u) { typpp(Nil); }
interp(vecp_u) { typpp(Vec); }

// stack manipulation
interp(tuck) { Have1(); sp--, sp[0] = sp[1], sp[1] = xp; N(1); }
interp(drop) { sp++; N(1); }
interp(dupl) { Have1(); --sp; sp[0] = sp[1]; N(1); }

// errors
interp(fail) { Jump(nope, "fail"); }
interp(gsym_u) {
 Have(Size(sym));
 sym y = (sym) hp;
 hp += Size(sym);
 y->nom = y->l = y->r = nil;
 y->code = v->count++ * mix;
 Go(ret, putsym(y)); }

interp(hfin_u) {
 ArCh(1);
 obj a = *Argv;
 TyHom(a);
 GF(button(Gh(a))) = (terp*) a;
 Go(ret, a); }

interp(ev_u) {
 ArCh(1);
 obj x;
 CallC(x = compile(v, *Argv),
       x = G(x)(v, x, Fp, Sp, Hp, nil));
 Go(ret, x); }

interp(rnd_u) { Go(ret, Pn(lcprng(&v->seed))); }

// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
static Inline u0 perrarg(lips v, mem fp) {
 i64 i = 0, argc = fp == Pool + Len ? 0 : Gn(Argc);
 if (argc) for (fputc(' ', stderr);;fputc(' ', stderr)) {
  obj x = Argv[i++];
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
  ip = Retp, fp += Size(fr) + Gn(Argc) + Gn(Subd);
  if (button(Gh(ip))[-1] == yield) break;
  fputs("# in ", stderr), emsep(v, Ph(ip), stderr, '\n'); }
 Hp = hp;
 return restart(v); }

noreturn obj restart(lips v) {
 Fp = Sp = Pool + Len;
 Xp = Ip = nil;
 v->mem_root = NULL;
 longjmp(v->restart, 1); }
