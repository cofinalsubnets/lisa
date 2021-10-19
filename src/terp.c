#include "lips.h"
#include "terp.h"
#include "hom.h"
#include "table.h"
#include "err.h"
#include "eql.h"
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
static VM(gc) { u64 n = v->xp; CALLC(reqsp(v, n)); NEXT(0); }
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
#define TERP(n, m, ...) VM(n) m(__VA_ARGS__)

// jump to nope() when an error happens.
static VM(nope, const char *, ...);

// type check
#define TC(x,t) if(kind((x))-(t))\
 Jump(nope, type_err_msg, tnom(kind(x)), tnom(t))
// arity check
#define ARY(n) if(_N(n)>ARGC)\
 Jump(nope,arity_err_msg,getnum(ARGC),n)

#define OP(nom, x, n) VM(nom) { xp = (x); NEXT(n); }
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
VM(push) { Have1(); *--sp = xp; NEXT(1); } // stack push
VM(loc_) { REF(V(LOCS)->xs) = xp; NEXT(2); } // set a local variable
VM(tbind) { CALLC(tblset(v, Top, (obj) GF(ip), xp)); NEXT(2); }

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

// instructions used by the compiler
VM(hom_u) {
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

VM(vset_u) {
 ARY(3);
 TC(ARGV[0], Vec);
 TC(ARGV[1], Num);
 num idx = getnum(ARGV[1]);
 vec ary = getvec(ARGV[0]);
 if (idx < 0 || idx >= ary->len) Jump(nope, "oob : %d#%d");
 GO(ret, ary->xs[idx] = ARGV[2]); }

VM(vget_u) {
 ARY(2);
 TC(ARGV[0], Vec);
 TC(ARGV[1], Num);
 num idx = getnum(ARGV[1]);
 vec ary = getvec(ARGV[0]);
 if (idx < 0 || idx >= ary->len) Jump(nope, "oob : %d#%d");
 GO(ret, ary->xs[idx]); }

VM(vec_u) {
 obj n = N(ARGC);
 Have(n + 1);
 vec t = (vec) hp;
 hp += 1 + n;
 cpy64(t->xs, ARGV, t->len = n);
 GO(ret, putvec(t)); }

VM(tset) {
 obj x = *sp++, y = *sp++;
 CALLC(v->xp = tblset(v, xp, x, y));
 NEXT(1); }

VM(emx) { obj h = *sp++ - W; G(h) = (terp*) xp;    AP(ip+W, h); }
VM(emi) { obj h = *sp++ - W; G(h) = (terp*) N(xp); AP(ip+W, h); }

VM(emx_u) {
 ARY(2);
 obj h = ARGV[1];
 TC(h, Hom);
 h -= W;
 G(h) = (terp*) ARGV[0];
 GO(ret, h); }

VM(emi_u) {
 ARY(2);
 TC(ARGV[0], Num);
 obj h = ARGV[1];
 TC(h, Hom);
 h -= W;
 G(h) = (terp*) Gn(ARGV[0]);
 GO(ret, h); }

VM(hgeti_u) { ARY(1); TC(ARGV[0], Hom); GO(ret,   N_(G(ARGV[0]))); }
VM(hgetx_u) { ARY(1); TC(ARGV[0], Hom); GO(ret, (obj)G(ARGV[0])); }

VM(hseek_u) {
 ARY(2); TC(ARGV[0], Hom); TC(ARGV[1], Num);
 GO(ret, H_(H(ARGV[0])+N(ARGV[1]))); }

// hash tables
VM(tblg) {
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
VM(tkeys) { CALLC(v->xp = tblkeys(v, xp)); NEXT(1); }

VM(tblc) {
 ARY(2);
 TC(ARGV[0], Tbl);
 xp = tblget(v, ARGV[0], ARGV[1]);
 GO(ret, xp ? ok : nil); }

static obj tblss(lips v, i64 i, i64 l) {
 mem fp = Fp;
 return i > l-2 ? ARGV[i-1] :
  (tblset(v, v->xp, ARGV[i], ARGV[i+1]),
   tblss(v, i+2, l)); }

VM(tbls) {
 ARY(1);
 xp = *ARGV;
 TC(xp, Tbl);
 RETC(v->xp = tblss(v, 1, N(ARGC))); }

VM(tblmk) { RETC(v->xp = table(v), tblss(v, 0, N(ARGC))); }

VM(tbld) {
 ARY(2); TC(ARGV[0], Tbl);
 RETC(v->xp = tbldel(v, ARGV[0], ARGV[1])); }

VM(tblks) {
 ARY(1); TC(*ARGV, Tbl);
 RETC(v->xp = tblkeys(v, *ARGV)); }

VM(tbll) {
 ARY(1); TC(*ARGV, Tbl);
 GO(ret, N_(gettbl(*ARGV)->len)); }

// string instructions
VM(strl) {
 ARY(1); TC(*ARGV, Str);
 GO(ret, N_(getstr(*ARGV)->len-1)); }

VM(strg) {
 ARY(2); TC(ARGV[0], Str); TC(ARGV[1], Num);
 GO(ret, N(ARGV[1]) < getstr(ARGV[0])->len-1 ?
  N_(getstr(ARGV[0])->text[N(ARGV[1])]) :
  nil); }

VM(strconc) {
 i64 l = Gn(ARGC), sum = 0, i = 0;
 while (i < l) {
  obj x = ARGV[i++];
  TC(x, Str);
  sum += S(x)->len - 1; }
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
VM(strs) {
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

VM(strmk) {
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

// this is used to create closures.
VM(take) {
 u64 n = Gn((obj) GF(ip));
 Have(n + 1);
 vec t = (vec) hp;
 hp += n + 1;
 t->len = n;
 cpy64(t->xs, sp, n);
 sp += n;
 GO(ret, putvec(t)); }

// print to console
VM(em_u) {
 u64 l = Gn(ARGC), i;
 if (l) {
  for (i = 0; i < l - 1; i++)
   emsep(v, ARGV[i], stdout, ' ');
  emit(v, xp = ARGV[i], stdout); }
 fputc('\n', stdout);
 Jump(ret); }

VM(putc_u) { ARY(1); fputc(N(*ARGV), stdout); Jump(ret); }
VM(getc_u) { GO(ret, feof(stdin) ? nil : N_(getc(stdin))); }

// pairs
OP1(car, X(xp)) OP1(cdr, Y(xp))
VM(cons) {
 Have1(); hp[0] = xp, hp[1] = *sp++;
 xp = puttwo(hp); hp += 2; NEXT(1); }

VM(car_u) { ARY(1); TC(*ARGV, Two); GO(ret, X(*ARGV)); }
VM(cdr_u) { ARY(1); TC(*ARGV, Two); GO(ret, Y(*ARGV)); }
VM(cons_u) {
 ARY(2); Have(2);
 hp[0] = ARGV[0], hp[1] = ARGV[1];
 xp = puttwo(hp), hp += 2; Jump(ret); }

// arithmetic
#define BINOP(nom, xpn) VM(nom) { xp = (xpn); NEXT(1); }
BINOP(add,  xp + *sp++ - Num)
BINOP(sub,  *sp++ - xp + Num)
BINOP(mul,  N_(N(*sp++)  * N(xp)))
BINOP(sar,  N_(N(*sp++) >> N(xp)))
BINOP(sal,  N_(N(*sp++) << N(xp)))
BINOP(band, xp & *sp++)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
OP1(neg, N_(-N(xp)))
VM(dqv) {
 if (xp == Pn(0)) Jump(nope, div0_err_msg, Gn(*sp));
 xp = Pn(Gn(*sp++) / Gn(xp));
 NEXT(1); }
VM(mod) {
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
 VM(nom##_u) { mm_u(Gn(ARGC), ARGV, dflt, op); }
UBINOP(add, 0, +) UBINOP(mul, 1, *)
UBINOP(band, -1, &) UBINOP(bor, 0, |) UBINOP(bxor, 0, ^)

VM(sub_u) {
 if (!(xp = Gn(ARGC))) GO(ret, Pn(0));
 TC(*ARGV, Num);
 if (xp == 1) GO(ret, Pn(-Gn(*ARGV)));
 mm_u(xp-1,ARGV+1,Gn(*ARGV),-); }

VM(div_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TC(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),/); }
VM(mod_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TC(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),%); }

VM(sar_u) {
 if (ARGC == N_(0)) GO(ret, Pn(0));
 TC(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), >>); }
VM(sal_u) {
 if (ARGC == N_(0)) GO(ret, N_(0));
 TC(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), <<); }

// type predicates
#define TP(ty) VM(ty##pp) { AP(ip+W, (ty##p(xp)?ok:nil)); }
TP(num) TP(hom) TP(two) TP(sym) TP(str) TP(tbl) TP(nil) TP(vec)

#define cmp_(n, op) BINOP(n, *sp++ op xp ? xp : nil)
cmp_(lt, <) cmp_(lteq, <=) cmp_(gteq, >=) cmp_(gt, >)
// there should be a separate instruction for simple equality?
BINOP(eq, eql(xp, *sp++) ? ok : nil)

static VM(ord_) {
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
 VM(n##_u) { v->xp=(obj)cmp_##n;Jump(ord_); }
cmp(<, lt) cmp(<=, lteq) cmp(>=, gteq) cmp(>, gt)
VM(eq_u) { ord_w(eql); }

static VM(typ) {
 for (obj *xs = ARGV, *l = xs + N(ARGC); xs < l;)
  if (kind(*xs++) != xp) GO(ret, nil);
 GO(ret, ok); }
#define typp(t, i) VM(t##p_u) { GO(typ, i); }
typp(num, Num) typp(hom, Hom) typp(str, Str) typp(tbl, Tbl)
typp(two, Two) typp(sym, Sym) typp(nil, Nil) typp(vec, Vec)

// stack manipulation
VM(dupl) { Have1(); --sp; sp[0] = sp[1]; NEXT(1); }

VM(ystr_u) {
 ARY(1);
 xp = *ARGV;
 TC(xp, Sym);
 GO(ret, getsym(xp)->nom); }

// errors
VM(fail) { Jump(nope, "fail"); }
VM(gsym_u) {
 if (ARGC > Pn(0) && strp(*ARGV))
  RETC(v->xp = intern(v, *ARGV));
 Have(Size(sym));
 sym y = (sym) hp;
 hp += Size(sym);
 y->nom = y->l = y->r = nil;
 y->code = v->count++ * mix;
 GO(ret, putsym(y)); }

VM(hfin_u) {
 ARY(1);
 obj a = *ARGV;
 TC(a, Hom);
 GF(button(Gh(a))) = (terp*) a;
 GO(ret, a); }

VM(ev_u) {
 ARY(1);
 RETC(xp = compile(v, *ARGV),
      v->xp = G(xp)(v, xp, Fp, Sp, Hp, nil)); }

VM(rnd_u) { GO(ret, Pn(lcprng(&v->seed))); }

VM(slurp) {
  ARY(1);
  xp = *ARGV;
  TC(xp, Str);
  RETC(v->xp = read_file(v, getstr(xp)->text)); }
VM(dump) {
 ARY(2);
 TC(ARGV[0], Str); TC(ARGV[1], Str);
 char *p = S(ARGV[0])->text,
      *d = S(ARGV[1])->text;
 GO(ret, write_file(v, p, d)); }

// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
static Inline u0 perrarg(lips v, mem fp) {
 i64 i = 0, argc = fp == v->pool + v->len ? 0 : getnum(ARGC);
 if (argc) for (fputc(' ', stderr);;fputc(' ', stderr)) {
  obj x = ARGV[i++];
  emit(v, x, stderr);
  if (i == argc) break; }
 fputc(')', stderr); }

static VM(nope, const char *msg, ...) {
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

NoInline obj homnom(lips v, obj x) {
 terp *k = G(x);
 if (k == clos || k == pc0 || k == pc1)
  return homnom(v, (obj) G(FF(x)));
 mem h = (mem) Gh(x);
 while (*h) h++;
 x = h[-1];
 int inb = (mem) x >= v->pool && (mem) x < v->pool+v->len;
 return inb ? x : x == (obj) yield ? Eva : nil; }

obj eval(lips v, obj x) { return
 x = pair(v, x, nil),
 apply(v, tblget(v, Top, Eva), x); }
