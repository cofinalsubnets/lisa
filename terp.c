#include "lips.h"
#include "terp.h"

// "the interpreter"
// it's a stack machine with one free register (xp)
// that's implemented on top of the C compiler's calling
// convention. this allows us to keep all the most important
// vm state variables in CPU registers at all times while the
// interpreter is running, without any platform-specific code.
// practically the interpreter amounts to a set of functions
// with a shared type that observe certain restrictions
// concerning memory access, function calls, etc. the C
// compiler has to tail call optimize these functions,
// otherwise the call stack will grow every time a
// nonoptimized instruction is executed.

// the System V AMD64 ABI calling convention passes the
// first six pointer or integer function arguments in CPU
// registers; further arguments are passed on the stack
// which affects the compiler's ability to optimize tail
// calls. interpreter functions have six arguments for
// that reason. the six arguments are:
// - v  : vm instance pointer
// - ip : instruction pointer
// - fp : frame pointer
// - sp : stack pointer
// - hp : heap pointer
// - xp : return value
// variables ip-xp all have corresponding slots in the vm
// structure that store their state when the interpreter
// is inactive. these slots are also used by the interpreter
// in certain cases to pass arguments in excess of 6 without
// spilling them to the stack.
//
// here's some macros for interpreter functions
#define v_op(n,...) NoInline obj n(vm v,hom ip,mem fp,mem sp,mem hp,obj xp,##__VA_ARGS__)
#define Pack() (Ip=puthom(ip),Sp=sp,Hp=hp,Fp=fp,Xp=xp)
#define Unpack() (fp=Fp,hp=Hp,sp=Sp,ip=gethom(Ip),xp=Xp)
#define Jump(f,...) return (f)(v,ip,fp,sp,hp,xp,##__VA_ARGS__)
#define Have(n) if (avail < n) Jump((Xp=n,gc))
#define CallC(...)(Pack(),(__VA_ARGS__),Unpack())
#define Cont(n, x) return ip+=n,xp=x,G(ip)(v,ip,fp,sp,hp,xp)
#define Ap(f,x) return G(f)(v,f,fp,sp,hp,x)
#define Go(f,x) return f(v,ip,fp,sp,hp,x)
#define Next(n) Ap(ip+n,xp)

// these are for accessing the current stack frame
typedef struct fr { obj clos, retp, subd, argc, argv[]; } *fr;
#define ff(x)((fr)(x))
#define Locs fp[-1]
#define Clos ff(fp)->clos
#define Retp ff(fp)->retp
#define Subd ff(fp)->subd
#define Argc ff(fp)->argc
#define Argv ff(fp)->argv

// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
static obj interpret_error(vm v, obj xp, obj ip, mem fp, const char *msg, ...) {
  va_list xs;
  va_start(xs, msg);
  vferrp(v, stderr, "interpret", xp, msg, xs);
  for (;fp < Pool + Len;
       ip = Retp, fp += Size(fr) + getnum(Argc) + getnum(Subd))
    fputs("  in ", stderr), emsep(v, ip, stderr, '\n');
  return restart(v); }

obj restart(vm v) {
  v->fp = v->sp = v->mem_pool + v->mem_len;
  v->xp = v->ip = nil;
  v->mem_root = NULL;
  longjmp(v->restart, 1); }

// vm instructions for different errors. the compiler will
// never emit these. 
static v_op(eetc) {
  return interpret_error(v, xp, puthom(ip), fp, "wrong type : %s for %s", tnom(kind(xp)), tnom(Xp)); }
static v_op(eear) {
  return interpret_error(v, 0, puthom(ip), fp, "wrong arity : %ld of %ld", Xp, Ip); }
static v_op(ee_0) {
  return interpret_error(v, 0, puthom(ip), fp, "%ld/0", getnum(xp)); }
#define type_error(x,t){xp=x,Xp=t;Jump(eetc);}
#define arity_error(h,w){Xp=h,Ip=w;Jump(eear);}
#define zero_error(x){xp=x;Jump(ee_0);}
#define TypeCheck(x, t) if(kind(x)!=t)type_error(x,t)
#define Arity(n) if(n>Argc)arity_error(getnum(Argc),getnum(n))
#define ArityCheck(n) Arity(putnum(n))

// this is the garbage collector interface used
// by the interpreter. interpreter functions that
// allocate memory check the available space and
// trigger garbage collection when more space is
// needed than is available.
//
// the amount of space required is passed in a
// register slot on the vm structure in order not
// to spill function arguments on AMD64, sorry.
v_op(gc) { num n = Xp; CallC(reqsp(v, n)); Next(0); }
#define avail (sp-hp)


// load instructions
#define fast_ref(b) (*(num*)((num)(b)+(num)GF(ip)-Num))
// this pointer arithmetic works because fixnums are
// premultiplied by sizeof(obj)
//
// load immediate value
v_op(immv) { xp = (obj) GF(ip); Next(2); }
// simple variable reference
v_op(argn) { xp = fast_ref(Argv);     Next(2); }
v_op(locn) { xp = fast_ref(AR(Locs)); Next(2); }
v_op(clon) { xp = fast_ref(AR(Clos)); Next(2); }
// special functions for eg. the first 4
// arguments in each location, and special values
// like 0 and nil, and functions that combine
// a reference with a stack push should all
// markedly improve general performance.

// environment operations
//
// push a value onto the stack
v_op(push)  { Have(1); *--sp = xp; Next(1); }

// create a local variable environment
v_op(prel) {
  num n = getnum(GF(ip));
  Have(n + 2);
  tup t = (tup) hp;
  hp += n + 1;
  t->len = n;
  while (n--) t->xs[n] = nil;
  *--sp = puttup(t);
  Next(2); }

// set a local variable
v_op(setl) { fast_ref(AR(Locs)) = xp; Next(2); }

// resolve a lazy binding
v_op(lbind) {
  obj w = (obj) GF(ip),
      d = XY(w), y = X(w);
  w = tbl_get(v, d, xp = YY(w));
  if (!w) return
    interpret_error(v, xp, puthom(ip), fp, "free variable");
  xp = w;
  if (getnum(y) != 8) TypeCheck(xp, getnum(y));
  G(ip) = immv;
  GF(ip) = (terp*) xp;
  Next(2); }

// set a global variable
v_op(tbind) {
  CallC(tbl_set(v, Dict, (obj) GF(ip), xp));
  Next(2); }

// control flow functions
// return to C
v_op(yield) { return Pack(), xp; }

// return from a function
v_op(ret) {
  //printf("ret "), emsep(v, (obj) ip, stdout, ' '); emsep(v, Retp, stdout, '\n');
  ip = gethom(Retp);
  sp = (mem) ((num) Argv + Argc - Num);
  fp = (mem) ((num)   sp + Subd - Num);
  Next(0); }

// jumps
// unconditional
v_op(jump) { Ap(gethom(GF(ip)), xp); }
// conditional
v_op(branch) {
  ip = gethom(xp == nil ? FF(ip) : gethom(GF(ip)));
  Next(0); }
// opposite conditional
v_op(barnch) {
  ip = gethom(xp != nil ? FF(ip) : gethom(GF(ip)));
  Next(0); }
// unconditional with closure
v_op(clos) {
  Clos = (obj) GF(ip);
  ip = gethom(G(FF(ip)));
  Next(0); }


// regular function call
v_op(call) {
  Have(Size(fr));
  //printf("call "), emsep(v, (obj) ip, stdout, ' '); emsep(v, xp, stdout, '\n');
  obj adic = (obj) GF(ip);
  num off = fp - (mem) ((num) sp + adic - Num);
  fp = sp -= Size(fr);
  Retp = puthom(ip+2);
  Subd = putnum(off);
  Clos = nil;
  Argc = adic;
  Ap(gethom(xp), nil); }

// tail call: special case where the caller and callee
// have the same arity, so we can keep the frame
v_op(loop) {
  //printf("jmp "), emsep(v, (obj)ip, stdout, ' '); emsep(v, xp, stdout, '\n');
  num adic = getnum(Argc);
  for (mem p = Argv; adic--; *p++ = *sp++);
  sp = fp;
  Ap(gethom(xp), nil); }

// general tail call
v_op(rec) {
  //printf("rec "), emsep(v, (obj)ip, stdout, ' '); emsep(v, xp, stdout, '\n');
  num adic = getnum(GF(ip));

  obj off = Subd, rp = Retp; // save return info
  mem src = sp + adic;
  // overwrite current frame with new frame
  sp = Argv + getnum(Argc);
  // important to copy in reverse order since they
  // may overlap
  for (num i = adic; i--; *--sp = *--src);
  fp = sp -= Size(fr);
  Retp = rp;
  Argc = putnum(adic);
  Subd = off;
  Clos = nil;
  Ap(gethom(xp), nil); }

// type/arity checking
v_op(tcnum) { TypeCheck(xp, Num); Next(1); }
v_op(tctwo) { TypeCheck(xp, Two); Next(1); }
v_op(tchom) { TypeCheck(xp, Hom); Next(1); }
v_op(arity) { Arity((obj)GF(ip)); Next(2); }

// continuations
//
// this is a simple but expensive way of doing continuations.
// it would be more memory efficient to do a copy-on-write
// kind of thing where the stack is only copied if the function
// returns. a spaghetti stack would be another option but it
// would be slower. faster continuations at the cost of slower
// function calls seems like a bad deal given the relative
// frequency of the two.
v_op(ccc_u) {
  ArityCheck(1);
  obj x = Argv[0];
  TypeCheck(x, Hom);
  // we need space for:
  // the entire stack
  // the frame offset
  // the length (to put it all in a tuple)
  // the continuation thread (4 words)
  num ht = Pool + Len - sp;
  Have(ht + 6);
  tup t = (tup) hp;
  hp += ht + 2;
  t->len = ht + 1;
  t->xs[0] = putnum(fp - sp);
  memcpy(t->xs+1, sp, w2b(ht));
  hom c = (hom) hp;
  hp += 4;
  c[0].g = cont;
  c[1].g = (terp*) puttup(t);
  c[2].g = NULL;
  c[3].g = (terp*) c;
  Argv[0] = puthom(c);
  Ap(gethom(x), nil); }

// call a continuation
v_op(cont) {
  tup t = gettup(GF(ip));
  Have(t->len - 1);
  xp = getnum(Argc) == 0 ? nil : *Argv;
  num off = getnum(t->xs[0]);
  sp = Pool + Len - (t->len - 1);
  fp = sp + off;
  memcpy(sp, t->xs+1, w2b(t->len-1));
  Jump(ret); }

v_op(rd_u) {
  obj x; CallC(x = parse(v, stdin), x = x ? pair(v, x, nil) : nil);
  Go(ret, x); }

// eval
v_op(ev_u) {
  ArityCheck(1);
  obj x; CallC(x = eval(v, *Argv));
  Go(ret, x); }

// apply
v_op(ap_u) {
  ArityCheck(2);
  obj x = Argv[0];
  TypeCheck(x, Hom);
  obj y = Argv[1];
  num adic = llen(y);
  Have(adic);
  obj off = Subd, rp = Retp;
  sp = Argv + getnum(Argc) - adic;
  for (num j = 0; j < adic; sp[j++] = X(y), y = Y(y));
  fp = sp -= Size(fr);
  Retp = rp;
  Argc = putnum(adic);
  Subd = off;
  Clos = nil;
  Ap(gethom(x), nil); }


// instructions used by the compiler
v_op(hom_u) {
  ArityCheck(1);
  TypeCheck(*Argv, Num);
  num len = getnum(*Argv) + 2;
  Have(len);
  hom h = (hom) hp;
  hp += len;
  memset(h, -1, w2b(len));
  h[len-1].g = (terp*) h;
  h[len-2].g = NULL;
  Go(ret, puthom(h+len-2)); }
v_op(hom_fin_u) {
  ArityCheck(1);
  TypeCheck(*Argv, Hom);
  obj x; CallC(x = hom_fin(v, *Argv));
  Go(ret, x); }
v_op(hom_setx_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Hom);
  G(gethom(Argv[0])) = (terp*) (xp = Argv[1]);
  Jump(ret); }
v_op(hom_seti_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Hom);
  TypeCheck(Argv[1], Num);
  G(Argv[0]) = (terp*) (xp = getnum(Argv[1]));
  Jump(ret); }
v_op(hom_geti_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Hom);
  Go(ret, putnum(G(Argv[0]))); }
v_op(hom_getx_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Hom);
  Go(ret, (obj)G(Argv[0])); }
v_op(hom_seek_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Hom);
  TypeCheck(Argv[1], Num);
  Go(ret, puthom(gethom(Argv[0])+getnum(Argv[1]))); }

// hash tables
v_op(tblg) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  Go(ret, xp ? xp : nil); }
v_op(tblc) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  Go(ret, xp ? putnum(0) : nil); }

static obj tblss(vm v, num i, num l) {
  mem fp = Fp;
  return i > l-2 ? Argv[i-1] :
    (tbl_set(v, *Argv, Argv[i], Argv[i+1]),
     tblss(v, i+2, l)); }

v_op(tbls) {
  obj x = nil;
  ArityCheck(1);
  xp = *Argv;
  TypeCheck(xp, Tbl);
  CallC(x = tblss(v, 1, getnum(Argc)));
  Go(ret, x); }
v_op(tbld) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  obj x = nil;
  CallC(x = tbl_del(v, Argv[0], Argv[1]));
  Go(ret, x); }
v_op(tblks) {
  ArityCheck(1);
  TypeCheck(Argv[0], Tbl);
  obj x;
  CallC(x = tbl_keys(v, Argv[0]));
  Go(ret, x); }
v_op(tbll) {
  ArityCheck(1);
  TypeCheck(Argv[0], Tbl);
  Go(ret, putnum(gettbl(*Argv)->len)); }
v_op(tblmk) {
  obj x;
  CallC(x = table(v));
  Go(ret, x); }

// string instructions
v_op(strl) {
  ArityCheck(1);
  TypeCheck(*Argv, Oct);
  Go(ret, putnum(getoct(*Argv)->len-1)); }
v_op(strg) {
  ArityCheck(2);
  TypeCheck(Argv[0], Oct);
  TypeCheck(Argv[1], Num);
  Go(ret, getnum(Argv[1]) < getoct(Argv[0])->len-1 ?
    putnum(getoct(Argv[0])->text[getnum(Argv[1])]) :
    nil); }
v_op(strmk) {
  num i, l = getnum(Argc)+1, size = 1 + b2w(l);
  Have(size);
  oct s = (oct) hp;
  hp += size;
  for (i = 0; i < l-1; i++) {
    obj x = Argv[i];
    TypeCheck(x, Num);
    if (x == putnum(0)) break;
    s->text[i] = getnum(x); }
  s->text[i] = 0;
  s->len = i+1;
  Go(ret, putoct(s)); }

v_op(vararg) {
  num reqd = getnum(GF(ip)),
      vdicity = getnum(Argc) - reqd;
  ArityCheck(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdicity) {
    Have(1);
    sp = --fp;
    for (num i = 0; i < Size(fr) + reqd; i++)
      fp[i] = fp[i+1];
    Argc += Word;
    Argv[reqd] = nil; }
  // in this case we just keep the existing slots.
  // the path is knowable at compile time in many cases
  // so maybe vararg should be two or more different
  // functions.
  else {
    Have(2 * vdicity);
    two t = (two) hp;
    hp += 2 * vdicity;
    for (num i = vdicity; i--;)
      t[i].x = Argv[reqd + i],
      t[i].y = puttwo(t+i+1);
    t[vdicity-1].y = nil;
    Argv[reqd] = puttwo(t); }
  Next(2); }

// the next few functions create and store
// lexical environments.
static v_op(encl) {
  num len = getnum(Argc);
  num n = len ? len + 12 : 11;
  Have(n);
  obj x = (obj) GF(ip);
  mem block = hp;
  hp += n;
  obj arg = nil; // optional argument array
  if (len) {
    tup t = (tup) block;
    block += 1 + len;
    t->len = len;
    memcpy(t->xs, Argv, w2b(len));
    arg = puttup(t); }
  tup t = (tup) block; // compiler thread closure array (len=5)
  t->len = 5; // initialize alpha closure
  t->xs[0] = arg;
  t->xs[1] = xp; // Locs or nil
  t->xs[2] = Clos;
  t->xs[3] = Y(x);

  block += 6;
  hom at = (hom) block; // compiler thread
  t->xs[4] = puthom(at);

  at[0].g = pc0;
  at[1].g = (terp*) puttup(t);
  at[2].g = (terp*) X(x);
  at[3].g = 0;
  at[4].g = (terp*) at;

  Ap(ip+2, puthom(at)); }

v_op(encll) {
  Go(encl, Locs); }

v_op(encln) {
  Go(encl, nil); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
v_op(pc0) {
  obj ec = (obj) GF(ip),
      arg = AR(ec)[0],
      loc = AR(ec)[1];
  num adic = nilp(arg) ? 0 : AL(arg);
  Have(Size(fr) + adic + 1);
  num off = (mem) fp - sp;
  G(ip) = pc1;
  sp -= adic;
  memcpy(sp, AR(arg), w2b(adic));
  ec = (obj) GF(ip);
  fp = sp -= Size(fr);
  Retp = puthom(ip);
  Subd = putnum(off);
  Argc = putnum(adic);
  Clos = AR(ec)[2];
  if (!nilp(loc)) *--sp = loc;
  //printf("pc0 "), emsep(v, (obj)ip, stdout, ' ');
  ip = gethom(AR(ec)[3]);
  //emsep(v, (obj)ip, stdout, '\n');
  Next(0); }

// finalize function instance closure
v_op(pc1) {
  G(ip) = clos;
  GF(ip) = (terp*) xp;
  Next(0); }

// this is used to create closures.
v_op(take) {
  num n = getnum((obj)GF(ip));
  Have(n + 1);
  tup t = (tup) hp;
  hp += n + 1;
  t->len = n;
  memcpy(t->xs, sp, w2b(n));
  sp += n;
  Go(ret, puttup(t)); }

// print to console
v_op(em_u) {
  num l = getnum(Argc), i;
  if (l) {
    for (i = 0; i < l - 1; i++)
      emsep(v, Argv[i], stdout, ' ');
    emit(v, xp = Argv[i], stdout); }
  fputc('\n', stdout);
  Jump(ret); }
v_op(pc_u) {
  ArityCheck(1);
  xp = *Argv;
  TypeCheck(xp, Num);
  fputc(getnum(xp), stdout);
  Jump(ret); }

v_op(emse) {
  emsep(v, xp, stdout, getnum(GF(ip)));
  Next(2); }

// pairs
v_op(cons) {
  Have(1); hp[0] = *sp++, hp[1] = xp;
  xp = puttwo(hp); hp += 2; Next(1); }
v_op(car) { Ap(ip+1, X(xp)); }
v_op(cdr) { Ap(ip+1, Y(xp)); }
v_op(setcar) { obj x = *sp++; X(xp) = x; xp = x; Next(1); }
v_op(setcdr) { obj x = *sp++; Y(xp) = x; xp = x; Next(1); }

v_op(cons_u) {
  ArityCheck(2);
  Have(2); hp[0] = Argv[0], hp[1] = Argv[1];
  xp = puttwo(hp), hp += 2; Jump(ret); }
v_op(car_u) {
  ArityCheck(1); TypeCheck(*Argv, Two);
  Go(ret, X(*Argv)); }
v_op(cdr_u) {
  ArityCheck(1); TypeCheck(*Argv, Two);
  Go(ret, Y(*Argv)); }
v_op(setcar_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Two);
  Go(ret, X(Argv[0]) = Argv[1]); }
v_op(setcdr_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Two);
  Go(ret, Y(Argv[0]) = Argv[1]); }

// arithmetic
#define ok putnum(0)
v_op(neg) { Ap(ip+1, putnum(-getnum(xp))); }
v_op(add) {
  xp = xp + *sp++ - Num;
  Next(1); }
v_op(sub) {
  xp = *sp++ - xp + Num;
  Next(1); }
v_op(mul) {
  xp = putnum(getnum(xp) * getnum(*sp++));
  Next(1); }
v_op(dqv) {
  if (xp == putnum(0)) zero_error(*sp);
  xp = putnum(getnum(*sp++) / getnum(xp));
  Next(1); }
v_op(mod) {
  if (xp == putnum(0)) zero_error(*sp);
  xp = putnum(getnum(*sp++) % getnum(xp));
  Next(1); }

// as tempting as it is to put shared logic into helper
// functions, there's so much register pressure in the vm
// that calling a function with an incompatible signature
// can generate as much code bloat as just inlining everything
// with a macro.
#define mm_u(_c,_v,_z,op){\
  obj x,m=_z,*xs=_v,*l=xs+_c;\
  if (_c) for(;xs<l;m=m op getnum(x)){\
    x = *xs++; TypeCheck(x, Num);}\
  Go(ret, putnum(m));}
#define mm_u0(_c,_v,_z,op){\
  obj x,m=_z,*xs=_v,*l=xs+_c;\
  if (_c) for(;xs<l;m=m op getnum(x)){\
    x = *xs++; TypeCheck(x, Num);\
    if (x == putnum(0)) zero_error(putnum(m));}\
  Go(ret, putnum(m));}

v_op(add_u) {
  mm_u(getnum(Argc), Argv, 0, +); }
v_op(mul_u) {
  mm_u(getnum(Argc), Argv, 1, *); }
v_op(sub_u) {
  num i = getnum(Argc);
  if (i == 0) Go(ret, putnum(0));
  TypeCheck(*Argv, Num);
  if (i == 1) Go(ret, putnum(-getnum(*Argv)));
  mm_u(i-1,Argv+1,getnum(Argv[0]),-); }

v_op(div_u) {
  num i = getnum(Argc);
  if (i == 0) Go(ret, putnum(1));
  TypeCheck(*Argv, Num);
  mm_u0(i-1,Argv+1,getnum(*Argv),/); }
v_op(mod_u) {
  num i = getnum(Argc);
  if (i == 0) Go(ret, putnum(1));
  TypeCheck(*Argv, Num);
  mm_u0(i-1,Argv+1,getnum(*Argv),%); }

// type predicates
v_op(numpp) { xp = nump(xp) ? ok : nil; Next(1); }
v_op(hompp) { xp = homp(xp) ? ok : nil; Next(1); }
v_op(twopp) { xp = twop(xp) ? ok : nil; Next(1); }
v_op(sympp) { xp = symp(xp) ? ok : nil; Next(1); }
v_op(strpp) { xp = octp(xp) ? ok : nil; Next(1); }
v_op(tblpp) { xp = tblp(xp) ? ok : nil; Next(1); }
v_op(nilpp) { xp = nilp(xp) ? ok : nil; Next(1); }

// comparison
static int eql(obj, obj);
static int twoeq(obj a, obj b) {
  return eql(X(a), X(b)) && eql(Y(a), Y(b)); }

static int tupeq(obj a, obj b) {
  tup t = gettup(a), u = gettup(b);
  if (t->len != u->len) return 0;
  for (num i = 0; i < t->len; i++)
    if (!eql(t->xs[i], u->xs[i])) return 0;
  return 1; }
static int streq(obj a, obj b) {
  oct o = getoct(a), m = getoct(b);
  if (o->len != m->len) return 0;
  for (num i = 0; i < o->len; i++)
    if (o->text[i] != m->text[i]) return 0;
  return 1; }
static int eql(obj a, obj b) {
  if (a == b) return 1;
  if (kind(a) != kind(b)) return 0;
  switch (kind(a)) {
    case Tup: return tupeq(a, b);
    case Two: return twoeq(a, b);
    case Oct: return streq(a, b);
    default: return 0; } }

v_op(lt)    { xp = *sp++ < xp  ? ok : nil; Next(1); }
v_op(lteq)  { xp = *sp++ <= xp ? ok : nil; Next(1); }
v_op(gteq)  { xp = *sp++ >= xp ? ok : nil; Next(1); }
v_op(gt)    { xp = *sp++ >  xp ? ok : nil; Next(1); }
// there should be a separate instruction for simple equality.
v_op(eq)   {
  obj y = *sp++;
  xp = eql(xp, y) ? ok : nil; 
  Next(1); }

#define ord_w(r){\
  obj n=getnum(Argc),*xs=Argv,m,*l;\
  switch(n){\
    case 0: no: Go(ret, nil);\
    case 1: break;\
    default: for(l=xs+n-1,m=*xs;xs<l;m=*++xs)\
               if(!(m r xs[1])) goto no;}\
  Go(ret, ok);}

#define ord_wv(r){\
  obj n=getnum(Argc),*xs=Argv,m,*l;\
  switch(n){\
    case 0: no: Go(ret, nil);\
    case 1: break;\
    default: for(l=xs+n-1,m=*xs;xs<l;m=*++xs)\
               if(!(r(m,xs[1]))) goto no;}\
  Go(ret, ok);}

v_op(and_u) {
  num i, l = getnum(Argc);
  switch (l) {
    case 0: Go(ret, nil);
    default:
      for (i = 0; i < l-1; i++) if (nilp(Argv[i])) break;
      Go(ret, Argv[i]); } }

v_op(or_u) {
  num i, l = getnum(Argc);
  switch (l) {
    case 0: Go(ret, nil);
    default:
      for (i = 0; i < l-1; i++) if (!nilp(Argv[i])) break;
      Go(ret, Argv[i]); } }

#define ord_v(r) Go(ret, ord_u(getnum(Argc), Argv, r))

v_op(lt_u)   { ord_w(<); }
v_op(lteq_u) { ord_w(<=); }
v_op(eq_u)   { ord_wv(eql); }
v_op(gteq_u) { ord_w(>=); }
v_op(gt_u)   { ord_w(>); }

#define typpp(t) {\
  for (obj *xs = Argv, *l=xs+getnum(Argc);xs<l;)\
    if (kind(*xs++)!=t) Go(ret, nil);\
  Go(ret, ok); }
v_op(nump_u) { typpp(Num); }
v_op(homp_u) { typpp(Hom); }
v_op(strp_u) { typpp(Oct); }
v_op(tblp_u) { typpp(Tbl); }
v_op(twop_u) { typpp(Two); }
v_op(symp_u) { typpp(Sym); }
v_op(nilp_u) { typpp(Nil); }

// stack manipulation
v_op(tuck) { Have(1); sp--, sp[0] = sp[1], sp[1] = xp; Next(1); }
v_op(drop) { sp++; Next(1); }

// errors
v_op(fail) { return interpret_error(v, xp, puthom(ip), fp, "fail"); }
v_op(fail_u) { Go(fail, getnum(Argc) ? *Argv : nil); }
v_op(zzz) { exit(EXIT_SUCCESS); }
v_op(globs) { Go(ret, Dict); }
v_op(cglobs) { Go(ret, v->cdict); }
