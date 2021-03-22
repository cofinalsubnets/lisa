#include "lips.h"
#include "ev.h"

// "the interpreter"
// it's a stack machine with one free register (xp)
// that's implemented on top of the C compiler's calling
// convention. this allows us to keep all the most important
// vm state variables in CPU registers at all times while the
// interpreter is running, without any platform-specific code.
// basically the interpreter amounts to a set of functions
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


// this is the garbage collector interface used
// by the interpreter. interpreter functions that
// allocate memory check the available space and
// trigger garbage collection when more space is
// needed than is available.
//
// the amount of space required is passed in a
// register slot on the vm structure in order not
// to spill function arguments on AMD64, sorry.
static vm_op(gc) { num n = Xp; CallC(reqsp(v, n)); Next(0); }
#define avail (sp-hp)


// load instructions
#define fast_ref(b) (*(num*)((num)(b)+(num)GF(ip)-Num))
// the pointer arithmetic works because fixnums are
// premultiplied by sizeof(obj)
vm_op(immv) { xp = (obj) GF(ip); Next(2); }
vm_op(unit) { xp = nil; Next(1); }
vm_op(one)  { xp = Pn(1); Next(1); }
vm_op(zero) { xp = Pn(0); Next(1); }
vm_op(argn) { xp = fast_ref(Argv); Next(2); }
vm_op(arg0) { xp = Argv[0]; Next(1); }
vm_op(arg1) { xp = Argv[1]; Next(1); }
vm_op(locn) { xp = fast_ref(AR(Locs)); Next(2); }
vm_op(loc0) { xp = AR(Locs)[0]; Next(1); }
vm_op(loc1) { xp = AR(Locs)[1]; Next(1); }
vm_op(clon) { xp = fast_ref(AR(Clos)); Next(2); }
vm_op(clo0) { xp = AR(Clos)[0]; Next(1); }
vm_op(clo1) { xp = AR(Clos)[1]; Next(1); }

// environment operations
// stack push
vm_op(push) { Have(1); *--sp = xp; Next(1); }

// set a global variable
vm_op(tbind) {
  CallC(tbl_set(v, Dict, (obj) GF(ip), xp));
  Next(2); }

// set a local variable
vm_op(setl) { fast_ref(AR(Locs)) = xp; Next(2); }

// initialize local variable slots
vm_op(prel) {
  num n = Gn(GF(ip));
  Have(n + 2);
  tup t = (tup) hp;
  hp += n + 1;
  t->len = n;
  while (n--) t->xs[n] = nil;
  *--sp = puttup(t);
  Next(2); }

static vm_op(err_free) {
  Jump(panic, "free variable : %s", symnom(xp)); }

// late bind
vm_op(lbind) {
  obj w = (obj) GF(ip),
      d = XY(w), y = X(w);
  w = tbl_get(v, d, xp = YY(w));
  if (!w) Go(err_free, xp);
  xp = w;
  if (Gn(y) != 8) TypeCheck(xp, Gn(y));
  terp *q = G(FF(ip));
  if ((q == call || q == rec) && homp(xp)) {
    obj aa = (obj)GF(FF(ip));
    if (G(xp) == arity && aa >= (obj)GF(xp))
      xp += 2*Word; }
  G(ip) = immv;
  GF(ip) = (terp*) xp;
  Next(2); }


// control flow
// return to C
vm_op(yield) { return Pack(), xp; }

// return from a function
vm_op(ret) {
  ip = Gh(Retp);
  sp = (mem) ((num) Argv + Argc - Num);
  fp = (mem) ((num)   sp + Subd - Num);
  Next(0); }

// conditional jumps
vm_op(branch) { ip = Gh(xp == nil ? FF(ip) : Gh(GF(ip)));      Next(0); }
vm_op(barnch) { ip = Gh(xp == nil ? Gh(GF(ip)) : FF(ip));      Next(0); }
vm_op(brlt)   { ip = Gh(*sp++ <  xp ? Gh(GF(ip)) : FF(ip));    Next(0); }
vm_op(brgteq) { ip = Gh(*sp++ <  xp ? FF(ip) : Gh(GF(ip)));    Next(0); }
vm_op(brlteq) { ip = Gh(*sp++ <= xp ? Gh(GF(ip)) : FF(ip));    Next(0); }
vm_op(brgt)   { ip = Gh(*sp++ <= xp ? FF(ip) : Gh(GF(ip)));    Next(0); }
vm_op(breq)   { ip = Gh(eql(*sp++, xp) ? Gh(GF(ip)) : FF(ip)); Next(0); }
vm_op(brne)   { ip = Gh(eql(*sp++, xp) ? FF(ip) : Gh(GF(ip))); Next(0); }
// unconditional jumps
vm_op(jump) { Ap(Gh(GF(ip)), xp); }
vm_op(clos) { // with closure
  Clos = (obj) GF(ip);
  ip = Gh(G(FF(ip)));
  Next(0); }


// regular function call
vm_op(call) {
  Have(Size(fr));
  obj adic = (obj) GF(ip);
  num off = fp - (mem) ((num) sp + adic - Num);
  fp = sp -= Size(fr);
  Retp = Ph(ip+2);
  Subd = Pn(off);
  Clos = nil;
  Argc = adic;
  Ap(Gh(xp), nil); }

// general tail call
vm_op(rec) {
  num adic = Gn(GF(ip));
  if (Argc == (obj)GF(ip)) {
    for (mem p = Argv; adic--; *p++ = *sp++);
    sp = fp;
    Ap(Gh(xp), nil); }


  obj off = Subd, rp = Retp; // save return info
  mem src = sp + adic;
  // overwrite current frame with new frame
  sp = Argv + Gn(Argc);
  // important to copy in reverse order since they
  // may overlap
  for (num i = adic; i--; *--sp = *--src);
  fp = sp -= Size(fr);
  Retp = rp;
  Argc = Pn(adic);
  Subd = off;
  Clos = nil;
  Ap(Gh(xp), nil); }

// type/arity checking
vm_op(arity) { Arity((obj)GF(ip)); Next(2); }
vm_op(idnum) { TypeCheck(xp, Num); Next(1); }
vm_op(idtwo) { TypeCheck(xp, Two); Next(1); }
vm_op(idhom) { TypeCheck(xp, Hom); Next(1); }
vm_op(idtbl) { TypeCheck(xp, Tbl); Next(1); }

// continuations
//
// this is a simple but expensive way of doing continuations.
// it would be more memory efficient to do a copy-on-write
// kind of thing where the stack is only copied if the function
// returns. a spaghetti stack would be another option but it
// would be slower. faster continuations at the cost of slower
// function calls seems like a bad deal given the relative
// frequency of the two.
vm_op(ccc_u) {
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
  t->xs[0] = Pn(fp - sp);
  memcpy(t->xs+1, sp, w2b(ht));
  hom c = (hom) hp;
  hp += 4;
  c[0].g = cont;
  c[1].g = (terp*) puttup(t);
  c[2].g = NULL;
  c[3].g = (terp*) c;
  Argv[0] = Ph(c);
  Ap(Gh(x), nil); }

// call a continuation
vm_op(cont) {
  tup t = gettup(GF(ip));
  Have(t->len - 1);
  xp = Gn(Argc) == 0 ? nil : *Argv;
  num off = Gn(t->xs[0]);
  sp = Pool + Len - (t->len - 1);
  fp = sp + off;
  memcpy(sp, t->xs+1, w2b(t->len-1));
  Jump(ret); }

vm_op(rd_u) {
  obj x; CallC(x = parse(v, stdin), x = x ? pair(v, x, nil) : nil);
  Go(ret, x); }

// apply
vm_op(ap_u) {
  ArityCheck(2);
  obj x = Argv[0];
  TypeCheck(x, Hom);
  obj y = Argv[1];
  num adic = llen(y);
  Have(adic);
  obj off = Subd, rp = Retp;
  sp = Argv + Gn(Argc) - adic;
  for (num j = 0; j < adic; sp[j++] = X(y), y = Y(y));
  fp = sp -= Size(fr);
  Retp = rp;
  Argc = Pn(adic);
  Subd = off;
  Clos = nil;
  Ap(Gh(x), nil); }


// instructions used by the compiler
vm_op(hom_u) {
  ArityCheck(1);
  TypeCheck(*Argv, Num);
  num len = Gn(*Argv) + 2;
  Have(len);
  hom h = (hom) hp;
  hp += len;
  memset(h, -1, w2b(len));
  h[len-1].g = (terp*) h;
  h[len-2].g = NULL;
  Go(ret, Ph(h+len-2)); }
vm_op(tset) {
  obj x = *sp++, y = *sp++;
  CallC(x = tbl_set(v, xp, x, y));
  Ap(ip+1, x); }
vm_op(emx) {
  hom h = Gh(*sp++) - 1;
  G(h) = (terp*) xp;
  Ap(ip+1, Ph(h)); }
vm_op(emi) {
  hom h = Gh(*sp++) - 1;
  G(h) = (terp*) Gn(xp);
  Ap(ip+1, Ph(h)); }
vm_op(emx_u) {
  ArityCheck(2);
  TypeCheck(Argv[1], Hom);
  hom h = Gh(Argv[1]) - 1;
  G(h) = (terp*) Argv[0];
  Go(ret, Ph(h)); }
vm_op(emi_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Num);
  TypeCheck(Argv[1], Hom);
  hom h = Gh(Argv[1]) - 1;
  G(h) = (terp*) Gn(Argv[0]);
  Go(ret, Ph(h)); }
vm_op(hom_geti_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Hom);
  Go(ret, Pn(G(Argv[0]))); }
vm_op(hom_getx_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Hom);
  Go(ret, (obj)G(Argv[0])); }
vm_op(hom_seek_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Hom);
  TypeCheck(Argv[1], Num);
  Go(ret, Ph(Gh(Argv[0])+Gn(Argv[1]))); }

// hash tables
vm_op(tblg) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  Go(ret, xp ? xp : nil); }
vm_op(tget) {
  xp = tbl_get(v, xp, *sp++);
  Ap(ip+1, xp ? xp : nil); }
vm_op(tblc) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  Go(ret, xp ? Pn(0) : nil); }

static obj tblss(vm v, num i, num l) {
  mem fp = Fp;
  return i > l-2 ? Argv[i-1] :
    (tbl_set(v, Xp, Argv[i], Argv[i+1]),
     tblss(v, i+2, l)); }

vm_op(tbls) {
  obj x = nil;
  ArityCheck(1);
  xp = *Argv;
  TypeCheck(xp, Tbl);
  CallC(x = tblss(v, 1, Gn(Argc)));
  Go(ret, x); }
vm_op(tbld) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  obj x = nil;
  CallC(x = tbl_del(v, Argv[0], Argv[1]));
  Go(ret, x); }
vm_op(tblks) {
  ArityCheck(1);
  TypeCheck(Argv[0], Tbl);
  obj x;
  CallC(x = tbl_keys(v, Argv[0]));
  Go(ret, x); }
vm_op(tbll) {
  ArityCheck(1);
  TypeCheck(Argv[0], Tbl);
  Go(ret, Pn(gettbl(*Argv)->len)); }
vm_op(tblmk) {
  CallC(Xp = table(v),
    tblss(v, 0, Gn(Argc)));
  Go(ret, Xp); }

// string instructions
vm_op(strl) {
  ArityCheck(1);
  TypeCheck(*Argv, Oct);
  Go(ret, Pn(getoct(*Argv)->len-1)); }
vm_op(strg) {
  ArityCheck(2);
  TypeCheck(Argv[0], Oct);
  TypeCheck(Argv[1], Num);
  Go(ret, Gn(Argv[1]) < getoct(Argv[0])->len-1 ?
    Pn(getoct(Argv[0])->text[Gn(Argv[1])]) :
    nil); }

vm_op(strc) {
  num l = Gn(Argc), sum = 0, i = 0;
  while (i < l) {
    obj x = Argv[i++];
    TypeCheck(x, Oct);
    sum += getoct(x)->len - 1; }
  num words = b2w(sum+1) + 1;
  Have(words); oct d = (oct) hp; hp += words;
  d->len = sum + 1;
  d->text[sum] = 0;
  while (i) {
    oct x = getoct(Argv[--i]);
    sum -= x->len - 1;
    memcpy(d->text+sum, x->text, x->len - 1); }
  Go(ret, putoct(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
vm_op(strs) {
  ArityCheck(3);
  TypeCheck(Argv[0], Oct);
  TypeCheck(Argv[1], Num);
  TypeCheck(Argv[2], Num);
  oct src = getoct(Argv[0]);
  num lb = Gn(Argv[1]), ub = Gn(Argv[2]);
  lb = max(lb, 0), ub = max(min(ub, src->len-1), lb);
  num words = 1 + b2w(ub - lb + 1);
  Have(words);
  oct dst = (oct) hp; hp += words;
  dst->len = ub - lb + 1;
  dst->text[ub - lb] = 0;
  memcpy(dst->text, src->text + lb, ub - lb);
  Go(ret, putoct(dst)); }

vm_op(strmk) {
  num i, l = Gn(Argc)+1, size = 1 + b2w(l);
  Have(size);
  oct s = (oct) hp;
  hp += size;
  for (i = 0; i < l-1; i++) {
    obj x = Argv[i];
    TypeCheck(x, Num);
    if (x == Pn(0)) break;
    s->text[i] = Gn(x); }
  s->text[i] = 0;
  s->len = i+1;
  Go(ret, putoct(s)); }

vm_op(vararg) {
  num reqd = Gn(GF(ip)),
      vdicity = Gn(Argc) - reqd;
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
static vm_op(encl) {
  num n = Xp;
  obj x = (obj) GF(ip);
  mem block = hp;
  hp += n;
  obj arg = nil; // optional argument array
  if (n > 11) {
    n -= 12;
    tup t = (tup) block;
    block += 1 + n;
    t->len = n;
    while (n--) t->xs[n] = Argv[n];
    arg = puttup(t); }

  tup t = (tup) block; // compiler thread closure array (1 length 5 elements)
  hom at = (hom) (block+6); // compiler thread (1 instruction 2 data 2 tag)

  t->len = 5; // initialize alpha closure
  t->xs[0] = arg;
  t->xs[1] = xp; // Locs or nil
  t->xs[2] = Clos;
  t->xs[3] = Y(x);
  t->xs[4] = Ph(at);

  at[0].g = pc0;
  at[1].g = (terp*) puttup(t);
  at[2].g = (terp*) X(x);
  at[3].g = 0;
  at[4].g = (terp*) at;

  Ap(ip+2, Ph(at)); }

vm_op(prencl) {
  num n = Gn(Argc);
  n += n ? 12 : 11;
  Have(n);
  Xp = n;
  Jump(encl); }

vm_op(encll) { Go(prencl, Locs); }
vm_op(encln) { Go(prencl, nil); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
vm_op(pc0) {
  obj ec = (obj) GF(ip),
      arg = AR(ec)[0],
      loc = AR(ec)[1];
  num adic = nilp(arg) ? 0 : AL(arg);
  Have(Size(fr) + adic + 1);
  num off = (mem) fp - sp;
  G(ip) = pc1;
  sp -= adic;
  for (num z = adic; z--; sp[z] = AR(arg)[z]);
  ec = (obj) GF(ip);
  fp = sp -= Size(fr);
  Retp = Ph(ip);
  Subd = Pn(off);
  Argc = Pn(adic);
  Clos = AR(ec)[2];
  if (!nilp(loc)) *--sp = loc;
  ip = Gh(AR(ec)[3]);
  Next(0); }

// finalize function instance closure
vm_op(pc1) {
  G(ip) = clos;
  GF(ip) = (terp*) xp;
  Next(0); }

// this is used to create closures.
vm_op(take) {
  num n = Gn((obj)GF(ip));
  Have(n + 1);
  tup t = (tup) hp;
  hp += n + 1;
  t->len = n;
  memcpy(t->xs, sp, w2b(n));
  sp += n;
  Go(ret, puttup(t)); }

// print to console
vm_op(em_u) {
  num l = Gn(Argc), i;
  if (l) {
    for (i = 0; i < l - 1; i++)
      emsep(v, Argv[i], stdout, ' ');
    emit(v, xp = Argv[i], stdout); }
  fputc('\n', stdout);
  Jump(ret); }

vm_op(pc_u) {
  ArityCheck(1);
  xp = *Argv;
  TypeCheck(xp, Num);
  fputc(Gn(xp), stdout);
  Jump(ret); }

vm_op(emse) {
  emsep(v, xp, stdout, Gn(GF(ip)));
  Next(2); }

// pairs
vm_op(cons) {
  Have(1); hp[0] = xp, hp[1] = *sp++;
  xp = puttwo(hp); hp += 2; Next(1); }
vm_op(car) { Ap(ip+1, X(xp)); }
vm_op(cdr) { Ap(ip+1, Y(xp)); }

vm_op(err_arity) { Jump(panic, E_ARITY, xp, Xp); }
vm_op(err_type) { Jump(panic, E_TYPE, tnom(xp), tnom(Xp)); }
vm_op(err_div0) { Jump(panic, E_ZERO, Gn(xp)); }
vm_op(cons_u) {
  num aa = Gn(Argc);
  if (!aa) {xp = 0, Xp = 1; Jump(err_arity); }
  Have(2); hp[0] = Argv[0], hp[1] = aa == 1 ? nil : Argv[1];
  xp = puttwo(hp), hp += 2; Jump(ret); }
vm_op(car_u) {
  ArityCheck(1); TypeCheck(*Argv, Two);
  Go(ret, X(*Argv)); }
vm_op(cdr_u) {
  ArityCheck(1); TypeCheck(*Argv, Two);
  Go(ret, Y(*Argv)); }

// arithmetic
#define ok Pn(0)
vm_op(neg) { Ap(ip+1, Pn(-Gn(xp))); }
vm_op(add) {
  xp = xp + *sp++ - Num;
  Next(1); }
vm_op(sub) {
  xp = *sp++ - xp + Num;
  Next(1); }
vm_op(mul) {
  xp = Pn(Gn(xp) * Gn(*sp++));
  Next(1); }
vm_op(dqv) {
  if (xp == Pn(0)) Go(err_div0, *sp);
  xp = Pn(Gn(*sp++) / Gn(xp));
  Next(1); }
vm_op(mod) {
  if (xp == Pn(0)) Go(err_div0, *sp);
  xp = Pn(Gn(*sp++) % Gn(xp));
  Next(1); }

#define mm_u(_c,_v,_z,op){\
  obj x,m=_z,*xs=_v,*l=xs+_c;\
  if (_c) for(;xs<l;m=m op Gn(x)){\
    x = *xs++; TypeCheck(x, Num);}\
  Go(ret, Pn(m));}
#define mm_u0(_c,_v,_z,op){\
  obj x,m=_z,*xs=_v,*l=xs+_c;\
  if (_c) for(;xs<l;m=m op Gn(x)){\
    x = *xs++; TypeCheck(x, Num);\
    if (x == Pn(0)) Go(err_div0, Pn(m));}\
  Go(ret, Pn(m));}

vm_op(add_u) {
  mm_u(Gn(Argc), Argv, 0, +); }
vm_op(mul_u) {
  mm_u(Gn(Argc), Argv, 1, *); }
vm_op(sub_u) {
  num i = Gn(Argc);
  if (i == 0) Go(ret, Pn(0));
  TypeCheck(*Argv, Num);
  if (i == 1) Go(ret, Pn(-Gn(*Argv)));
  mm_u(i-1,Argv+1,Gn(Argv[0]),-); }

vm_op(div_u) {
  num i = Gn(Argc);
  if (i == 0) Go(ret, Pn(1));
  TypeCheck(*Argv, Num);
  mm_u0(i-1,Argv+1,Gn(*Argv),/); }
vm_op(mod_u) {
  num i = Gn(Argc);
  if (i == 0) Go(ret, Pn(1));
  TypeCheck(*Argv, Num);
  mm_u0(i-1,Argv+1,Gn(*Argv),%); }

// type predicates
vm_op(numpp) { xp = nump(xp) ? ok : nil; Next(1); }
vm_op(hompp) { xp = homp(xp) ? ok : nil; Next(1); }
vm_op(twopp) { xp = twop(xp) ? ok : nil; Next(1); }
vm_op(sympp) { xp = symp(xp) ? ok : nil; Next(1); }
vm_op(strpp) { xp = octp(xp) ? ok : nil; Next(1); }
vm_op(tblpp) { xp = tblp(xp) ? ok : nil; Next(1); }
vm_op(nilpp) { xp = nilp(xp) ? ok : nil; Next(1); }
vm_op(vecpp) { xp = tupp(xp) ? ok : nil; Next(1); }

// comparison
int eql(obj, obj);

// lists are immutable, so we can opportunistically
// deduplicate them.
static int twoeq(obj a, obj b) {
  if (!eql(X(a), X(b))) return 0;
  X(a) = X(b);
  if (!eql(Y(a), Y(b))) return 0;
  Y(a) = Y(b);
  return 1; }

static int streq(obj a, obj b) {
  oct o = getoct(a), m = getoct(b);
  if (o->len != m->len) return 0;
  for (num i = 0; i < o->len; i++)
    if (o->text[i] != m->text[i]) return 0;
  return 1; }

int eql(obj a, obj b) {
  if (a == b) return 1;
  if (kind(a) != kind(b)) return 0;
  switch (kind(a)) {
    case Two: return twoeq(a, b);
    case Oct: return streq(a, b);
    default: return 0; } }

vm_op(lt)    { xp = *sp++ <  xp ? xp : nil; Next(1); }
vm_op(lteq)  { xp = *sp++ <= xp ? xp : nil; Next(1); }
vm_op(gteq)  { xp = *sp++ >= xp ? xp : nil; Next(1); }
vm_op(gt)    { xp = *sp++ >  xp ? xp : nil; Next(1); }
// there should be a separate instruction for simple equality.
vm_op(eq)   {
  obj y = *sp++;
  xp = eql(xp, y) ? ok : nil;
  Next(1); }

#define ord_w(r){\
  obj n=Gn(Argc),*xs=Argv,m,*l;\
  switch(n){\
    case 0: no: Go(ret, nil);\
    case 1: break;\
    default: for(l=xs+n-1,m=*xs;xs<l;m=*++xs)\
               if(!(m r xs[1])) goto no;}\
  Go(ret, ok);}

#define ord_wv(r){\
  obj n=Gn(Argc),*xs=Argv,m,*l;\
  switch(n){\
    case 0: no: Go(ret, nil);\
    case 1: break;\
    default: for(l=xs+n-1,m=*xs;xs<l;m=*++xs)\
               if(!(r(m,xs[1]))) goto no;}\
  Go(ret, ok);}

#define ord_v(r) Go(ret, ord_u(Gn(Argc), Argv, r))

vm_op(lt_u)   { ord_w(<); }
vm_op(lteq_u) { ord_w(<=); }
vm_op(eq_u)   { ord_wv(eql); }
vm_op(gteq_u) { ord_w(>=); }
vm_op(gt_u)   { ord_w(>); }

#define typpp(t) {\
  for (obj *xs = Argv, *l=xs+Gn(Argc);xs<l;)\
    if (kind(*xs++)!=t) Go(ret, nil);\
  Go(ret, ok); }
vm_op(nump_u) { typpp(Num); }
vm_op(homp_u) { typpp(Hom); }
vm_op(strp_u) { typpp(Oct); }
vm_op(tblp_u) { typpp(Tbl); }
vm_op(twop_u) { typpp(Two); }
vm_op(symp_u) { typpp(Sym); }
vm_op(nilp_u) { typpp(Nil); }
vm_op(vecp_u) { typpp(Tup); }

// stack manipulation
vm_op(tuck) { Have(1); sp--, sp[0] = sp[1], sp[1] = xp; Next(1); }
vm_op(drop) { sp++; Next(1); }
vm_op(dupl) { Have(1); --sp; sp[0] = sp[1]; Next(1); }

// errors
vm_op(fail) { Jump(panic, NULL); }
vm_op(zzz) { exit(EXIT_SUCCESS); }
vm_op(gsym_u) {
  Have(Size(sym));
  sym y = (sym) hp; hp += Size(sym);
  y->nom = y->l = y->r = nil;
  y->code = v->count++ * mix;
  Go(ret, putsym(y)); }

// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
static Inline void perrarg(vm v, mem fp) {
  num argc = fp == Pool + Len ? 0 : Gn(Argc), i = 0;
  if (argc == 0) return;
  for (fputs(" at ", stderr);;fputc(' ', stderr)) {
    obj x = Argv[i++];
    emit(v, x, stderr);
    if (i == argc) break; } }

static Inline hom button(hom h) {
  while (h->g) h++;
  return h; }

vm_op(panic, const char *msg, ...) {
  // an error proves that the function that failed is
  // undefined at its arguments.
  fputs("# ", stderr), emit(v, Ph(ip), stderr),
  fputs(" does not exist", stderr), perrarg(v, fp);
  if (msg) {
    fputs(" : ", stderr);
    va_list xs; va_start(xs, msg);
    vfprintf(stderr, msg, xs);
    va_end(xs); }
  fputc('\n', stderr);
  for (;;) {
    ip = Gh(Retp), fp += Size(fr) + Gn(Argc) + Gn(Subd);
    if (button(ip)[-1].g == yield) break;
    fputs("#  in ", stderr), emsep(v, Ph(ip), stderr, '\n'); }
  return Hp = hp, restart(v); }

obj restart(vm v) {
  Fp = Sp = Pool + Len;
  Xp = Ip = nil;
  v->mem_root = NULL;
  //reqsp(v, 0);
  longjmp(v->restart, 1); }


