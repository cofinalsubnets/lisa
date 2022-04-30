#include "lips.h"
#include "ll.h"

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

Ll(ap_u) {
  Arity(2);
  ob x = Argv[0], y = Argv[1];
  TypeCheck(x, Hom);
  u64 adic = llen(y);
  Have(adic);
  ob off = Subr, rp = Retp;
  sp = Argv + getnum(Argc) - adic;
  for (u64 j = 0; j < adic;
    sp[j++] = gettwo(y)->a,
    y = gettwo(y)->b);
  fp = sp -= Width(fr);
  Retp = rp;
  Argc = putnum(adic);
  Subr = off;
  Clos = nil;
  return ApY(x, nil); }

// continuations
Ll(ccc_u) {
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
  t->xs[0] = putnum(fp - sp);
  cpy64(t->xs+1, sp, depth);
  yo c = (yo) hp;
  hp += 4;
  c[0].ll = (vm*) cont;
  c[1].ll = (vm*) putvec(t);
  c[2].ll = NULL;
  c[3].ll = (vm*) c;
  *Argv = (ob) c;
  return ApY(ip, nil); }

// call a continuation
Ll(cont) {
  vec t = getvec((ob) IP[1].ll);
  Have(t->len - 1);
  xp = getnum(Argc) == 0 ? nil : *Argv;
  i64 off = getnum(t->xs[0]);
  sp = v->pool + v->len - (t->len - 1);
  fp = sp + off;
  cpy64(sp, t->xs+1, t->len-1);
  return ApC(ret, xp); }

Ll(vararg) {
  i64 reqd = getnum((i64) IP[1].ll),
      vdic = getnum(Argc) - reqd;
  Arity(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) {
    Have1();
    cpy64(fp-1, fp, Width(fr) + getnum(Argc));
    sp = --fp;
    Argc += sizeof(void*);
    Argv[reqd] = nil;
    return ApN(2, xp); }
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
  return ApN(2, xp); }

// type predicates
#define Tp(t)\
  Vm(t##pp) { return ApN(1, (t##p(xp)?ok:nil)); }\
  Vm(t##p_u) {\
    for (ob *xs = Argv, *l = xs + getnum(Argc); xs < l;)\
      if (!t##p(*xs++)) return ApC(ret, nil);\
    return ApC(ret, N1); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(vec) Tp(nil)

// stack manipulation
Vm(dupl) { Have1(); --sp; sp[0] = sp[1]; return ApN(1, xp); }

static vm recne;

////
/// Branch Instructions
//
// unconditional jump
Vm(jump) { return ApY((ob) IP[1].ll, xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Vm(nom) {\
  if (test) return ApY((ob)a(IP),x);\
  else return ApY((ob)b(IP),y); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq,  eql(*sp++, xp), GF, N1, FF, nil)
Br(brne,  eql(*sp++, xp), FF, N1, GF, nil)

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
  return ApY(ip, xp); }

// "inner" function call
Vm(call) {
  Have(Width(fr));
  ob adic = (ob) IP[1].ll;
  i64 off = fp - (ob*) ((i64) sp + adic - Num);
  fp = sp -= Width(fr);
  Retp = (ob)((yo)ip+2);
  Subr = putnum(off);
  Clos = nil;
  Argc = adic;
  return ApY(xp, nil); }

// tail call
Vm(rec) {
  if (Argc != (ip = (ob) IP[1].ll)) return ApC(recne, xp);
  cpy64(Argv, sp, ip = getnum(ip));
  sp = fp;
  return ApY(xp, nil); }

// tail call with different arity
static Vm(recne) {
 v->xp = Subr;
 v->ip = (yo) Retp; // save return info
 fp = Argv + getnum(Argc - ip);
 cpy64r(fp, sp, getnum(ip)); // copy from high to low
 sp = fp -= Width(fr);
 Retp = (ob) v->ip;
 Argc = ip;
 Subr = v->xp;
 return ApY(xp, Clos = nil); }

// errors
Vm(fail) { return Pack(), err(v, "fail"); }

#define type_err_msg "wrong type : %s for %s"
Vm(type_error) {
  enum class exp = v->xp, act = Q(xp);
  return Pack(), err(v, type_err_msg, tnom(act), tnom(exp)); }

Vm(oob_error) {
  i64 a = v->xp, b = (i64) v->ip;
  return Pack(), err(v, "oob : %d >= %d", a, b); }

#define arity_err_msg "wrong arity : %d of %d"
Vm(ary_error) {
  i64 a = getnum(Argc), b = v->xp;
  return Pack(), err(v, arity_err_msg, a, b); }

Vm(div_error) { return Pack(), err(v, "/ 0"); }

// type/arity checking
#define DTc(n, t) Vm(n) {\
  if (Q(xp-t)==0) return ApN(1, xp);\
  return v->xp = t, ApC(type_error, xp); }
DTc(idZ, Num) DTc(idH, Hom) DTc(idT, Tbl) DTc(id2, Two)
Vm(arity) {
  ob reqd = (ob) IP[1].ll;
  if (reqd <= Argc) return ApN(2, xp);
  else return v->xp = getnum(reqd), ApC(ary_error, xp); }

SI u0 show_call(en v, yo ip, fr fp) {
  fputc('(', stderr);
  emit(v, (ob) ip, stderr);
  for (i64 i = 0, argc = getnum(fp->argc); i < argc;)
    fputc(' ', stderr), emit(v, fp->argv[i++], stderr);
  fputc(')', stderr); }

NoInline ob err(en v, const char *msg, ...) {
  yo ip = (yo) v->ip;
  fr fp = (fr) v->fp,
     top = (fr) (v->pool + v->len);
  fputs("# ", stderr);
  if (fp < top) show_call(v, ip, fp), fputs(" : ", stderr);
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  fputc('\n', stderr);
  // print backtrace
  if (fp < top) for (;;) {
    ip = (yo) fp->retp;
    fp = (fr) ((ob*) fp + Width(fr) + getnum(fp->argc) + getnum(fp->subd));
    if (fp == top) break;
    fputs("# in ", stderr);
    show_call(v, ip, fp);
    fputc('\n', stderr); }
  v->fp = (fr) (v->sp = v->pool + v->len);
  v->ip = (yo) (v->xp = nil);
  return 0; }


#define mm_u(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);}\
  return ApC(ret, putnum(xp));}

#define mm_u0(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);\
    if (x == N0) return ApC(div_error, xp);}\
  return ApC(ret, putnum(xp));}

Vm(sub_u) {
  if (!(xp = getnum(Argc))) return ApC(ret, N0);
  TypeCheck(*Argv, Num);
  if (xp == 1) return ApC(ret, putnum(-getnum(*Argv)));
  mm_u(xp-1,Argv+1,getnum(*Argv),-); }

Vm(sar_u) {
  if (Argc == N0) return ApC(ret, N0);
  TypeCheck(*Argv, Num);
  mm_u(getnum(Argc)-1, Argv+1, getnum(*Argv), >>); }

Vm(sal_u) {
  if (Argc == N0) return ApC(ret, N0);
  TypeCheck(*Argv, Num);
  mm_u(getnum(Argc)-1, Argv+1, getnum(*Argv), <<); }

Vm(dqv) {
  if (xp == N0) return ApC(div_error, xp);
  xp = putnum(getnum(*sp++) / getnum(xp));
  return ApN(1, xp); }

Vm(div_u) {
  if (!(xp = getnum(Argc))) return ApC(ret, N1);
  TypeCheck(*Argv, Num);
  mm_u0(xp-1, Argv+1, getnum(*Argv), /); }

Vm(mod) {
  if (xp == N0) return ApC(div_error, xp);
  xp = putnum(getnum(*sp++) % getnum(xp));
  return ApN(1, xp); }

Vm(mod_u) {
  if (!(xp = getnum(Argc))) return ApC(ret, N1);
  TypeCheck(*Argv, Num);
  mm_u0(xp-1, Argv+1, getnum(*Argv), %); }

Vm(rnd_u) {
  xp = putnum(v->rand = lcprng(v->rand));
  return ApC(ret, xp); }

#define OP(nom, x, n) Vm(nom) { xp = (x); return ApN(n, xp); }
#define OP1(nom, x) OP(nom, x, 1)
OP1(neg, putnum(-getnum(xp)))
BINOP(add,  xp + *sp++ - Num)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  putnum(getnum(*sp++) * getnum(xp)))
BINOP(band, xp & *sp++)
BINOP(sub,  *sp++ - xp + Num)
BINOP(sar,  putnum(getnum(*sp++) >> getnum(xp)))
BINOP(sal,  putnum(getnum(*sp++) << getnum(xp)))

#define UBINOP(nom, dflt, op) Vm(nom##_u) { mm_u(getnum(Argc), Argv, dflt, op); }

UBINOP(add, 0, +)
UBINOP(bor, 0, |)
UBINOP(bxor, 0, ^)
UBINOP(mul, 1, *)
UBINOP(band, -1, &)
#include <string.h>

static NoInline u1 eql_(ob a, ob b) { return eql(a, b); }

static NoInline u1 eql_two(two a, two b) {
  return eql_(a->a, b->a) && eql_(a->b, b->b); }

static NoInline u1 eql_str(str a, str b) {
  return a->len == b->len &&
    strcmp(a->text, b->text) == 0; }

Inline u1 eql(ob a, ob b) {
  return a == b || (Q(a) == Q(b) &&
    ((twop(a) && eql_two(gettwo(a), gettwo(b))) ||
     (strp(a) && eql_str(getstr(a), getstr(b))))); }

#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
BINOP(eq, eql(xp, *sp++) ? ok : nil)
#define cmp(n, op) BINOP(n, op(*sp++, xp) ? xp : nil)
cmp(lt, LT) cmp(lteq, LE) cmp(gteq, GE) cmp(gt, GT)
#undef cmp
#define cmp(op, n) Vm(n##_u) {\
  ob n = getnum(Argc), *xs = Argv, m, *l;\
  switch (n) {\
    case 0: return ApC(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) return ApC(ret, nil);\
    case 1: return ApC(ret, N1); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(eql, eq)
#define OP2(nom, x) OP(nom, x, 2)
////
/// Load Instructions
//
// constants
OP1(unit, nil)
OP1(one, putnum(1))
OP1(zero, putnum(0))
// immediate value from thread
OP2(imm, (ob) IP[1].ll)

// indexed references
#define Ref(b) (*(i64*)((i64)(b)+(i64)IP[1].ll-Num))
// pointer arithmetic works because fixnums are premultiplied by W

// function arguments
OP2(arg, Ref(Argv))
OP1(arg0, Argv[0])
OP1(arg1, Argv[1])
// local variables
OP2(loc, Ref(getvec(Locs)->xs))
OP1(loc0, getvec(Locs)->xs[0])
OP1(loc1, getvec(Locs)->xs[1])
// closure variables
OP2(clo, Ref(getvec(Clos)->xs))
OP1(clo0, getvec(Clos)->xs[0])
OP1(clo1, getvec(Clos)->xs[1])

////
/// Store Instructions
//
// stack push
Vm(push) {
  Have1();
  *--sp = xp;
  return ApN(1, xp); }
// set a local variable
Vm(loc_) {
  Ref(getvec(Locs)->xs) = xp;
  return ApN(2, xp); }
// set a global variable
Vm(tbind) {
  CallC(tbl_set(v, Top, (ob) IP[1].ll, xp));
  return ApN(2, xp); }

// allocate local variable array
Vm(locals) {
  i64 n = getnum((i64) IP[1].ll);
  Have(n + 2);
  vec t = (vec) hp;
  set64(t->xs, nil, t->len = n);
  hp += n + 1;
  *--sp = putvec(t);
  return ApN(2, xp); }

// late binding
// long b/c it does the "static" type and arity checks
// that would have been done by the compiler if the function
// had been bound early.
Vm(lbind) {
  ob w = (ob) IP[1].ll, d = AB(w), y = A(w);
  if (!(w = tbl_get(v, d, xp = BB(w)))) {
    char *nom = nilp(getsym(xp)->nom) ? "()" : getstr(getsym(xp)->nom)->text;
    return Pack(), err(v, "free variable : %s", nom); }
  xp = w;
  if (y != putnum(sizeof(ob))) Tc(xp, getnum(y)); // do the type check
  // omit the arity check if possible
  if (IP[2].ll == call || IP[2].ll == rec) {
    yo x = gethom(xp);
    if (x[0].ll == arity && IP[3].ll >= x[1].ll)
      xp = (ob) (x + 2); }
  IP[0].ll = imm;
  IP[1].ll = (vm*) xp;
  return ApN(2, xp); }


// hash tables
Vm(tblg) {
  Arity(2);
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  return ApC(ret, xp ? xp : nil); }

OP1(tget, (xp = tbl_get(v, xp, *sp++)) ? xp : nil)
OP1(thas, tbl_get(v, xp, *sp++) ? ok : nil)
OP1(tlen, putnum(gettbl(xp)->len))

Vm(tkeys) {
  CallC(v->xp = tblkeys(v, xp));
  bind(xp, xp);
  return ApN(1, xp); }

Vm(tblc) {
  Arity(2);
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  return ApC(ret, xp ? ok : nil); }

static ob tblss(en v, i64 i, i64 l) {
  fr fp = (fr) v->fp;
  if (i > l-2) return fp->argv[i-1];
  ob _;
  bind(_, tbl_set(v, v->xp, fp->argv[i], fp->argv[i+1]));
  return tblss(v, i+2, l); }

Vm(tbls) {
  Arity(1);
  xp = *Argv;
  TypeCheck(xp, Tbl);
  CallC(v->xp = tblss(v, 1, getnum(Argc)));
  bind(xp, xp);
  return ApC(ret, xp); }

Vm(tblmk) {
  Pack();
  bind(v->xp, table(v)); // xp <- table
  bind(xp, tblss(v, 0, getnum(Argc))); // _ <- updates
  Unpack();
  return ApC(ret, xp); }

Vm(tblks) {
  Arity(1);
  TypeCheck(*Argv, Tbl);
  CallC(v->xp = tblkeys(v, *Argv));
  bind(xp, xp);
  return ApC(ret, xp); }

Vm(tbll) {
  Arity(1);
  TypeCheck(*Argv, Tbl);
  return ApC(ret, putnum(gettbl(*Argv)->len)); }

Vm(tset) {
  ob x = *sp++, y = *sp++;
  CallC(v->xp = tbl_set(v, xp, x, y));
  bind(xp, xp);
  return ApN(1, xp); }

// pairs
OP1(car, A(xp)) OP1(cdr, B(xp))
Vm(cons) {
  Have1();
  hp[0] = xp;
  hp[1] = *sp++;
  xp = puttwo(hp);
  hp += 2;
  return ApN(1, xp); }

Vm(car_u) {
  Arity(1);
  TypeCheck(*Argv, Two);
  return ApC(ret, A(*Argv)); }

Vm(cdr_u) {
  Arity(1);
  TypeCheck(*Argv, Two);
  return ApC(ret, B(*Argv)); }

Vm(cons_u) {
  Arity(2);
  Have(2);
  two w = (two) hp;
  hp += 2;
  w->a = Argv[0], w->b = Argv[1];
  return ApC(ret, puttwo(w)); }

// this is used to create closures.
Vm(take) {
  u64 n = getnum((ob) IP[1].ll);
  Have(n + 1);
  vec t = (vec) hp;
  hp += n + 1;
  t->len = n;
  cpy64(t->xs, sp, n);
  sp += n;
  return ApC(ret, putvec(t)); }

Vm(vset_u) {
  Arity(3);
  CheckType(Argv[0], Vec);
  CheckType(Argv[1], Num);
  i64 idx = getnum(Argv[1]);
  vec ary = getvec(Argv[0]);
  if (idx < 0 || idx >= ary->len) {
    v->xp = idx;
    v->ip = (yo) ary->len;
    return ApC(oob_error, xp); }
  return ApC(ret, ary->xs[idx] = Argv[2]); }

Vm(vget_u) {
  Arity(2);
  CheckType(Argv[0], Vec);
  CheckType(Argv[1], Num);
  i64 idx = getnum(Argv[1]);
  vec ary = getvec(Argv[0]);
  if (idx < 0 || idx >= ary->len) {
    v->xp = idx;
    v->ip = (yo) ary->len;
    return ApC(oob_error, xp); }
  return ApC(ret, ary->xs[idx]); }

Vm(vec_u) {
  ob n = getnum(Argc);
  Have(n + 1);
  vec t = (vec) hp;
  hp += 1 + n;
  cpy64(t->xs, Argv, t->len = n);
  return ApC(ret, putvec(t)); }
