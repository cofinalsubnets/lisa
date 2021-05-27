#include "lips.h"
#include "terp.h"
// subs for some libc functions: memset, memcpy, strlen
u0 fill(_*_d, O i, N l) { // fill with a word
 for (M d = _d; l--; *d++=i); }
u0 wcpy(_*_d, Ko _*_s, N l) { // copy words
 mem d = _d; Ko O *s = _s;
 while (l--) *d++=*s++; }
void bcpy(void*_d, const void*_s, u64 l) { // copy bytes
 char *d = _d; const char *s = _s;
 while (l--) *d++=*s++; }

////
/// bootstrap thread compiler
//
// functions construct their continuations by pushing function
// pointers onto the main stack with Pu()
#define Pu(...) pushs(v,__VA_ARGS__,non)
#define Push(...) Pu(__VA_ARGS__)
// and then calling them with ccc
#define Cc ((c1*)Gn(*Sp++))
#define ccc Cc
// there's a natural correspondence between the Pu/Cc pattern
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
#define toplp(x) !e
#define arg(x)  AR(x)[0] // argument variables : a list
#define loc(x)  AR(x)[1] // local variables : a list
#define clo(x)  AR(x)[2] // closure variables : a list
#define par(x)  AR(x)[3] // surrounding scope : tuple or nil
#define name(x) AR(x)[4] // function name : a symbol or nil
#define asig(x) AR(x)[5] // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.
//

typedef obj
 c1(lips, mem, i64),
 c2(lips, mem, i64, obj),
 c3(lips, mem, i64, obj, obj);
static u0
 c_de_r(lips, mem, obj),
 scan(lips, mem, obj),
 pushs(lips, ...);
static c1 c_ev, c_d_bind, inst, insx, c_ini;
static c2 c_eval, c_sy, c_2, c_imm, ltu, c_ap, c_la_clo;
static c3 late;
static obj
 look(lips, obj, obj),
 hfin(lips, obj),
 hini(lips, i64);
#define interns(v,c) intern(v,string(v,c))

En { Here, Loc, Arg, Clo, Wait };
#define c1(nom,...) static O nom(V v,M e,Z m,##__VA_ARGS__)
#define c2(nom,...) static O nom(V v,M e,Z m,O x,##__VA_ARGS__)

// emit code backwards like cons
static Inline obj em1(terp *i, obj k) {
  return k -= W, G(k) = i, k; }
static Inline obj em2(terp *i, obj j, obj k) {
  return em1(i, em1((T)j, k)); }

static obj imx(lips v, mem e, i64 m, terp *i, obj x) {
 return Pu(Pn(i), x), insx(v, e, m); }

static obj apply(lips v, obj f, obj x) {
  Pu(f, x);
  hom h = cells(v, 5);
  h[0] = call;
  h[1] = (T) Pn(2);
  h[2] = yield;
  h[3] = NULL;
  h[4] = (T) h;
  return call(v, Ph(h), Fp, Sp, Hp, tblget(v, Dict, App)); }

static obj compile(lips v, obj x) {
 Pu(Pn(c_ev), x, Pn(inst), Pn(yield), Pn(c_ini));
 return ccc(v, NULL, 0); }

obj eval(lips v, obj x) {
 x = pair(v, x, nil);
 return apply(v, tblget(v, Dict, Eva), x); }

static obj rwlade(lips v, obj x) {
 obj y;
 mm(&x);
 while (twop(X(x)))
  y = snoc(v, YX(x), XY(x)),
  y = pair(v, La, y),
  y = pair(v, y, YY(x)),
  x = pair(v, XX(x), y) ;
 return um, x; }

static int scan_def(lips v, mem e, obj x) {
 if (!twop(x)) R 1; // this is an even case so export all the definitions to the local scope
 if (!twop(Y(x))) R 0; // this is an odd case so ignore these, they'll be imported after the rewrite
 mm(&x);
 int r = scan_def(v, e, YY(x));
 if (r) {
  x = rwlade(v, x);
  obj y = pair(v, X(x), loc(*e));
  loc(*e) = y;
  scan(v, e, XY(x)); }
 R um, r; }

St _ scan(V v, M e, O x) {
 if (!twop(x) || X(x) == La || X(x) == Qt) R;
 if (X(x) == De) R (void) scan_def(v, e, Y(x));
 Fo (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
 um; }

St O asign(V v, O a, Z i, M m) { O x;
 if (!twop(a)) R *m = i, a;
 if (twop(Y(a)) && XY(a) == Va)
  R *m = -(i+1), pair(v, X(a), nil);
 Mm(a, x = asign(v, Y(a), i+1, m));
 R pair(v, X(a), x); }

St Ve tuplr(V v, Z i, va_list xs) { Ve t; O x;
 R (x = va_arg(xs, O)) ?
  (Mm(x, t = tuplr(v, i+1, xs)), t->xs[i] = x, t) :
  ((t = cells(v, Size(tup) + i))->len = i, t); }

St O tupl(V v, ...) { Ve t; va_list xs;
 R va_start(xs, v),
  t = tuplr(v, 0, xs),
  va_end(xs),
  puttup(t); }

St O scope(V v, M e, O a, O n) { Z s = 0;
  R Mm(n, a = asign(v, a, 0, &s)),
    tupl(v, a, nil, nil, e ? *e : nil, n, Pn(s), non); }

St O compose(V v, M e, O x) {
  Pu(Pn(c_ev), x, Pn(inst), Pn(ret), Pn(c_ini));
  scan(v, e, Sp[1]);
  x = ccc(v, e, 4); // 4 = 2 + 2
  Z i = llen(loc(*e));
  if (i) x = em2(locals,  Pn(i), x);
  i = Gn(asig(*e));
     if (i > 0) x = em2(arity, Pn(i), x);
  El if (i < 0) x = em2(vararg, Pn(-i-1), x);
  x = hfin(v, x);
  R twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expression, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
St O ltu(V v, M e, O n, O l) { O y;
  l = Y(l);
  Mm(n,
    l = twop(l) ? l : pair(v, l, nil),
    Mm(y, l = linitp(v, l, &y),
          Mm(l, n = pair(v, n, toplp(e) ? nil : e ? name(*e):nil)),
          n = scope(v, e, l, n)),
    l = compose(v, &n, X(y)));
  R l; }


c2(c_la) {
  T j = imm;
  O k, nom = *Sp == Pn(c_d_bind) ? Sp[1] : nil;
  Mm(nom, Mm(x, k = ccc(v, e, m+2)));
  Mm(k,
    x = homp(x = ltu(v, e, nom, x)) ? x :
    (j = toplp(e) || !twop(loc(*e)) ? encln : encll,
     c_la_clo(v, e, X(x), Y(x))));
  R em2(j, x, k); }

c2(c_imm) { R Pu(Pn(imm), x), insx(v, e, m); }

St O c_la_clo(V v, M e, O arg, O seq) {
  Z i = llen(arg);
  mm(&arg), mm(&seq);
  Fo (Pu(Pn(insx), Pn(take), Pn(i), Pn(c_ini));
      twop(arg);
      Pu(Pn(c_ev), X(arg), Pn(inst), Pn(push)), arg = Y(arg));
  R arg = ccc(v, e, 0), um, um,
    pair(v, seq, arg); }

c1(c_d_bind) {
 O y = *Sp++;
 R toplp(e) ? imx(v, e, m, tbind, y) :
              imx(v, e, m, loc_, Pn(idx(loc(*e), y))); }

static u0 c_de_r(V v, M e, O x) {
 if (!twop(x)) R;
 x = rwlade(v, x);
 Mm(x, c_de_r(v, e, YY(x))),
 Pu(Pn(c_ev), XY(x), Pn(c_d_bind), X(x)); }

// syntactic sugar for define
static obj def_sug(lips v, obj x) {
 obj y = nil;
 Mm(y, x = linitp(v, x, &y));
 x = pair(v, x, y),   x = pair(v, Se, x);
 x = pair(v, x, nil), x = pair(v, La, x);
 return pair(v, x, nil); }

c2(c_de) { return
 !twop(Y(x))    ? c_imm(v, e, m, nil) :
 llen(Y(x)) % 2 ? c_eval(v, e, m, def_sug(v, x)) :
                  (c_de_r(v, e, Y(x)), ccc(v, e, m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 Xp
#define S2 Ip

// before generating anything, store the
// exit address in stack 2
c1(c_co_pre) {
 obj x = ccc(v, e, m);
 return X(S2 = pair(v, x, S2)); }

// before generating a branch emit a jump to
// the top of stack 2
c1(c_co_pre_con) {
 obj x = ccc(v, e, m+2), k = X(S2);
 return G(k) == ret ? em1(ret, x) : em2(jump, k, x); }

// after generating a branch store its address
// in stack 1
c1(c_co_post_con) {
 obj x = ccc(v, e, m);
 return X(S1 = pair(v, x, S1)); }

// before generating an antecedent emit a branch to
// the top of stack 1
c1(c_co_pre_ant) {
 obj x = ccc(v, e, m+2);
 return x = em2(branch, X(S1), x), S1 = Y(S1), x; }

static u0 c_co_r(lips v, mem e, obj x) {
 if (!twop(x)) x = pair(v, nil, nil);
 if (!twop(Y(x))) Pu(Pn(c_ev), X(x), Pn(c_co_pre_con));
 else Mm(x, Pu(Pn(c_co_post_con), Pn(c_ev), XY(x),
               Pn(c_co_pre_con)),
            c_co_r(v, e, YY(x))),
      Pu(Pn(c_ev), X(x), Pn(c_co_pre_ant)); }

c2(c_co) { return
 Mm(x, Pu(Pn(c_co_pre))),
 c_co_r(v, e, Y(x)),
 x = ccc(v, e, m),
 S2 = Y(S2),
 x; }

static u0 c_se_r(lips v, mem e, obj x) {
 if (twop(x)) Mm(x, c_se_r(v, e, Y(x))),
              Pu(Pn(c_ev), X(x)); }
c2(c_se) {
 if (!twop(x = Y(x))) x = pair(v, nil, nil);
 return c_se_r(v, e, x), ccc(v, e, m); }

c1(c_call) {
 obj a = *Sp++, k = ccc(v, e, m + 2);
 return em2(G(k) == ret ? rec : call, a, k); }

#define L(n,x) pair(v, Pn(n), x)
static obj look(lips v, obj e, obj y) { obj q; return
 nilp(e) ?
  ((q = tblget(v, Dict, y)) ?  L(Here, q) : L(Wait, Dict)) :
 ((q = idx(loc(e), y)) != -1) ?
  L(Loc, e) :
 ((q = idx(arg(e), y)) != -1) ?
  L(Arg, e) :
 ((q = idx(clo(e), y)) != -1) ?
  L(Clo, e) :
 look(v, par(e), y); }
#undef L

c2(late, obj d) { obj k; return
 x = pair(v, d, x),
 Mm(x, k = ccc(v, e, m+2)),
 Mm(k, x = pair(v, Pn(8), x)),
 em2(lbind, x, k); }

c2(c_sy) { O y, q;
 Mm(x, y = X(q = look(v, e ? *e:nil, x)));
 Sw (Gn(y)) {
  case Here: return c_imm(v, e, m, Y(q));
  case Wait: return late(v, e, m, x, Y(q));
  default:
   if (Y(q) == *e) switch (Gn(y)) {
    case Loc: return imx(v, e, m, loc, Pn(idx(loc(*e), x)));
    case Arg: return imx(v, e, m, arg, Pn(idx(arg(*e), x)));
    case Clo: return imx(v, e, m, clo, Pn(idx(clo(*e), x))); }
   y = llen(clo(*e));
   Mm(x, q = snoc(v, clo(*e), x)), clo(*e) = q;
   return imx(v, e, m, clo, Pn(y)); } }

c1(c_ev) { return c_eval(v, e, m, *Sp++); }
c2(c_eval) {
 return (symp(x)?c_sy:twop(x)?c_2:c_imm)(v,e,m,x); }

c2(c_qt) { return c_imm(v, e, m, twop(x = Y(x)) ? X(x) : x); }
c2(c_2) { obj z = X(x); return 
 (z==Qt?c_qt:
  z==If?c_co:
  z==De?c_de:
  z==La?c_la:
  z==Se?c_se:c_ap)(v,e,m,x); }

#define Rec(...) {\
  O _s1 = S1, _s2 = S2;\
  Mm(_s1, Mm(_s2,__VA_ARGS__));\
  S1 = _s1, S2 = _s2; }

c2(c_ap) {
 obj y = tblget(v, Mac, X(x));
 if (y) {
  Rec(x = apply(v, y, Y(x)));
  return c_eval(v, e, m, x); }
 for (mm(&x),
      Pu(Pn(c_ev), X(x), Pn(inst), Pn(idH),
         Pn(c_call), Pn(llen(Y(x))));
      twop(x = Y(x));
      Pu(Pn(c_ev), X(x), Pn(inst), Pn(push)));
 return um, ccc(v, e, m); }

c1(inst) {
 terp* i = (terp*) Gn(*Sp++);
 return em1(i, ccc(v, e, m+1)); }

c1(insx) {
 terp* i = (terp*) Gn(*Sp++);
 obj x = *Sp++, k;
 Mm(x, k = ccc(v, e, m+2));
 return em2(i, x, k); }

c1(c_ini) {
 obj k = hini(v, m+1);
 return em1((T)(e ? name(*e):Eva), k); }

static u0 pushss(lips v, i64 i, va_list xs) {
 obj x; (x = va_arg(xs, obj)) ?
  (Mm(x, pushss(v, i, xs)), *--Sp = x) :
  reqsp(v, i); }

static u0 pushs(V v, ...) {
 i64 i = 0;
 va_list xs; va_start(xs, v);
 while (va_arg(xs, O)) i++;
 va_end(xs), va_start(xs, v);
 if (Avail < i) pushss(v, i, xs);
 else for (mem sp = Sp -= i; i--; *sp++ = va_arg(xs, obj));
 va_end(xs); }

obj hini(V v, Z n) {
 hom a = cells(v, n + 2);
 return
  G(a+n) = NULL,
  GF(a+n) = (T) a,
  fill((M) a, nil, n),
  Ph(a+n); }

static obj hfin(lips v, obj a) {
 return (obj) (GF(button(Gh(a))) = (terp*) a); }

obj homnom(lips v, obj x) {
 terp *k = G(x);
 if (k == clos || k == pc0 || k == pc1)
  return homnom(v, (O) G(FF(x)));
 mem h = (M) Gh(x);
 while (*h) h++;
 x = h[-1];
 return (mem) x >= Pool && (M) x < Pool+Len ? x :
        x == Ob yield                       ? Eva :
                                              nil; }

static u0 rpr(lips v, mem d, const char *n, terp *u) {
 obj x, y = pair(v, interns(v, n), nil);
 Mm(y, x = hini(v, 2));
 x = em2(u, y, x);
 tblset(v, *d, X(y), x); }

static u0 rin(lips v, mem d, const char *n, terp *u) {
 obj y = interns(v, n);
 tblset(v, *d, y, Pn(u)); }

#define RPR(a,b) rpr(v,&d,a,b)
#define RIN(x) rin(v,&d,"i-"#x,x)
static Inline obj code_dictionary(V v) {
 obj d = table(v);;
 Mm(d, prims(RPR), insts(RIN));
 return d; }
#undef RPR
#undef RIN

static Inline u0 init_globals_array(lips v) {
 vec t = cells(v, sizeof (struct tup)/W + NGlobs);
 fill(t->xs, nil, t->len = NGlobs);
 obj z, y = Glob = puttup(t);
 Mm(y,
  z = code_dictionary(v), Top = z,
  z = table(v), Mac = z,
#define bsym(i,s)(z=interns(v,s),AR(y)[i]=z)
  bsym(Eval, "ev"), bsym(Apply, "ap"),
  bsym(Def, ":"),   bsym(Cond, "?"), bsym(Lamb, "\\"),
  bsym(Quote, "`"), bsym(Seq, ","),  bsym(Splat, ".")); }
#undef bsym

#define NOM "lips"
#define USR_PATH ".local/lib/"NOM"/"
#define SYS_PATH "/usr/lib/"NOM"/"
static int seekp(Ko Ch* p) {
 int b, c;
 b = open(getenv("HOME"), O_RDONLY);
 c = openat(b, USR_PATH, O_RDONLY), close(b);
 b = openat(c, p, O_RDONLY), close(c);
 if (-1 < b) return b;
 b = open(SYS_PATH, O_RDONLY);
 c = openat(b, p, O_RDONLY), close(b);
 return c; }


lips bootstrap(lips v) {
 if (v == NULL) R v;
 const char *path = "prelude.lips";
 int pre = seekp(path);
 if (pre == -1) errp(v, "can't find %s", path);
 else {
  FILE *f = fdopen(pre, "r");
  if (setjmp(v->restart)) return
   errp(v, "error in %s", path),
   fclose(f), finalize(v);
  script(v, f), fclose(f); }
 return v; }

lips initialize(int argc, const char **argv) {
 lips v = malloc(sizeof(struct lips));
 if (!v || setjmp(v->restart))
  return errp(v, "oom"), finalize(v);
 v->t0 = clock(),
 v->ip = v->xp = v->syms = v->glob = nil,
 v->fp = v->hp = v->sp = (M) w2b(1),
 v->count = 0, v->mem_len = 1, v->mem_pool = NULL,
 v->mem_root = NULL;
 init_globals_array(v);
 obj y = interns(v, "ns");
 tblset(v, Top, y, Top);
 y = interns(v, "macros");
 tblset(v, Top, y, Mac);
 y = interns(v, "argv");
 obj a = nil;
 mm(&y); mm(&a);
 for (obj z; argc--;)
  z = string(v, argv[argc]),
  a = pair(v, z, a);
 um, um, tblset(v, Top, y, a);
 srand(clock());
 return v; }

lips finalize(lips v) {
 if (v) free(v->mem_pool), free(v);
 return NULL; }

#undef arg
#undef loc
#undef clo

// " the virtual machine "
// it's a stack machine with one free register that's
// implemented on top of the C compiler's calling convention.
// this allows us to keep the most important state variables
// in CPU registers at all times while the interpreter is
// running, without any platform-specific code.

// " the interpreter "
// is all the functions of type terp:
#define Vm(n,...) NoInline obj \
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
#define Jump(f,...) R (f)(v,ip,fp,sp,hp,xp,##__VA_ARGS__)
#define Cont(n, x) R ip+=w2b(n), xp=x, G(ip)(v,ip,fp,sp,hp,xp)
#define Ap(f,x) R ip=f,G(ip)(v,ip,fp,sp,hp,x)
#define Go(f,x) R f(v,ip,fp,sp,hp,x)
#define N(n) ip+=w2b(n);Ap(ip, xp)
// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function
static Vm(gc) { i64 n = Xp; CallC(reqsp(v, n)); N(0); }
// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.
#define avail (sp-hp)
#define Have(n) if (avail < n) Jump((Xp=n,gc))
#define Have1() if (hp == sp) Jump((Xp=1,gc)) // common case, faster comparison

// the interpreter takes a very basic approach to error
// handling: something is wrong? jump to nope().
St Vm(nope);
#define TyCh(x,t) if(kind((x)-(t)))Jump(nope) // type check
#define ArCh(n) if(Pn(n)>Argc)Jump(nope)

// " virtual machine instructions "
//
// load instructions
Vm(imm) { xp = (obj) GF(ip); N(2); }
// common constants
Vm(unit) { xp = nil;   N(1); }
Vm(one)  { xp = Pn(1); N(1); }
Vm(zero) { xp = Pn(0); N(1); }

// indexed load instructions
// this pointer arithmetic works because fixnums are
// premultiplied by W
#define fast_idx(b) (*(Z*)((Z)(b)+(Z)GF(ip)-Num))
Vm(arg)  { xp = fast_idx(Argv);     N(2); }
Vm(arg0) { xp = Argv[0];            N(1); }
Vm(arg1) { xp = Argv[1];            N(1); }
Vm(loc)  { xp = fast_idx(AR(Locs)); N(2); }
Vm(loc0) { xp = AR(Locs)[0];        N(1); }
Vm(loc1) { xp = AR(Locs)[1];        N(1); }
Vm(clo)  { xp = fast_idx(AR(Clos)); N(2); }
Vm(clo0) { xp = AR(Clos)[0];        N(1); }
Vm(clo1) { xp = AR(Clos)[1];        N(1); }

// store instructions
Vm(push) { Have1(); *--sp = xp; N(1); } // stack push
Vm(loc_) { fast_idx(AR(Locs)) = xp; N(2); } // set a local variable
Vm(tbind) { CallC(tblset(v, Dict, Ob GF(ip), xp)); N(2); }

// initialize local variable slots
Vm(locals) {
 i64 n = Gn(GF(ip));
 Have(n + 2);
 vec t = (Ve) hp;
 fill(t->xs, nil, t->len = n);
 hp += n + 1;
 *--sp = puttup(t);
 N(2); }

// late bind
// this function is a lil complicated, because it incorporates
// the "static" type and arity checking that would have been
// done by the compiler if the function had been bound early.
Vm(lbind) {
 obj w = Ob GF(ip),
     d = XY(w), y = X(w);
 if (!(w = tblget(v, d, xp = YY(w)))) Jump(nope); // free variable
 xp = w;
 if (Gn(y) != 8) TyCh(xp, Gn(y)); // do the type check
 terp *q = G(FF(ip)); // omit the arity check if possible
 if (q == call || q == rec) {
  obj aa = (obj) GF(FF(ip));
  if (G(xp) == arity && aa >= (obj) GF(xp))
   xp += W2; }
 G(ip) = imm;
 GF(ip) = (T) xp;
 N(2); }

// control flow instructions
// return to C
Vm(yield) { Pack(); return xp; }

// conditional jumps
Vm(branch) { Ap(xp == nil ? (obj) FF(ip) : (obj) GF(ip), xp); }
Vm(barnch) { Ap(xp == nil ? (obj) GF(ip) : (obj) FF(ip), xp); }
// relational jumps
Vm(brlt)   { Ap(*sp++ <  xp    ? (obj) GF(ip) : (obj) FF(ip), xp); }
Vm(brgteq) { Ap(*sp++ <  xp    ? (obj) FF(ip) : (obj) GF(ip), xp); }
Vm(brlteq) { Ap(*sp++ <= xp    ? (obj) GF(ip) : (obj) FF(ip), xp); }
Vm(brgt)   { Ap(*sp++ <= xp    ? (obj) FF(ip) : (obj) GF(ip), xp); }
Vm(breq)   { Ap(eql(*sp++, xp) ? (obj) GF(ip) : (obj) FF(ip), xp); }
Vm(brne)   { Ap(eql(*sp++, xp) ? (obj) FF(ip) : (obj) GF(ip), xp); }

// unconditional jumps
Vm(jump) { Ap((obj) GF(ip), xp); }
Vm(clos) { Clos = (obj) GF(ip); Ap((obj) G(FF(ip)), xp); }

// return from a function
Vm(ret) {
 ip = Retp;
 sp = (M) ((Z) Argv + Argc - Num);
 fp = (M) ((Z)   sp + Subd - Num);
 N(0); }

// regular function call
Vm(call) {
#define fwds (sizeof(struct fr)/W)
 Have(fwds);
 obj adic = (obj) GF(ip);
 i64 off = fp - (M) ((Z) sp + adic - Num);
 fp = sp -= fwds;
 Retp = Ph(ip+W2);
 Subd = Pn(off);
 Clos = nil;
 Argc = adic;
 Ap(xp, nil); }

// general tail call
Vm(rec) {
 i64 adic = Gn(GF(ip));
 if (Argc == (obj) GF(ip)) {
  for (mem p = Argv; adic--; *p++ = *sp++);
  sp = fp;
  Ap(xp, nil); }

 obj off = Subd, rp = Retp; // save return info
 mem src = sp + adic;
 // overwrite current frame with new frame
 sp = Argv + Gn(Argc);
 // important to copy in reverse order since they
 // may overlap
 for (i64 i = adic; i--; *--sp = *--src);
 fp = sp -= Size(fr);
 Retp = rp;
 Argc = Pn(adic);
 Subd = off;
 Clos = nil;
 Ap(xp, nil); }

// type/arity checking
#define arn(n) if(n>Argc)Jump(nope)
#define tcn(k) {if(kind(xp-k))Jump(nope);}
Vm(idZ) { tcn(Num); N(1); }
Vm(id2) { tcn(Two); N(1); }
Vm(idH) { tcn(Hom); N(1); }
Vm(idT) { tcn(Tbl); N(1); }
Vm(arity) { arn(Ob GF(ip)); N(2); }

// continuations
//
// this is a simple but expensive way of doing continuations.
// it would be more memory efficient to do a copy-on-write
// kind of thing where the stack is only copied if the function
// returns. a spaghetti stack would be another option but it
// would be slower. faster continuations at the cost of slower
// function calls seems like a bad deal given the relative
// frequency of the two.
Vm(ccc_u) {
 obj x;
 ArCh(1);
 TyCh(x = Argv[0], Hom);
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
 wcpy(t->xs+1, sp, ht);
 hom c = (hom) hp;
 hp += 4;
 c[0] = cont;
 c[1] = (T) puttup(t);
 c[2] = NULL;
 c[3] = (T) c;
 Argv[0] = Ph(c);
 Ap(x, nil); }

// call a continuation
Vm(cont) {
 vec t = gettup(GF(ip));
 Have(t->len - 1);
 xp = Gn(Argc) == 0 ? nil : *Argv;
 i64 off = Gn(t->xs[0]);
 sp = Pool + Len - (t->len - 1);
 fp = sp + off;
 wcpy(sp, t->xs+1, t->len-1);
 Jump(ret); }

Vm(ap_u) {
 ArCh(2);
 obj x = Argv[0], y = Argv[1];
 TyCh(x, Hom);
 i64 adic = llen(y);
 Have(adic);
 obj off = Subd, rp = Retp;
 sp = Argv + Gn(Argc) - adic;
 for (Z j = 0; j < adic; y = Y(y)) sp[j++] = X(y);
 fp = sp -= Size(fr);
 Retp = rp;
 Argc = Pn(adic);
 Subd = off;
 Clos = nil;
 Ap(x, nil); }

// instructions used by the compiler
Vm(hom_u) {
 obj x;
 ArCh(1);
 TyCh(x = *Argv, Num);
 i64 len = Gn(x) + 2;
 Have(len);
 hom h = (hom) hp;
 hp += len;
 fill((M) h, nil, len);
 h[len-1] = (T) h;
 h[len-2] = NULL;
 Go(ret, Ph(h+len-2)); }

Vm(tset) {
 obj x = *sp++, y = *sp++;
 CallC(x = tblset(v, xp, x, y));
 Ap(ip+W, x); }

Vm(emx) {
 obj h = *sp++ - W;
 G(h) = (T) xp;
 Ap(ip+W, h); }

Vm(emi) {
 obj h = *sp++ - W;
 G(h) = (terp*) Gn(xp);
 Ap(ip+W, h); }

Vm(emx_u) {
 ArCh(2);
 TyCh(Argv[1], Hom);
 obj h = Argv[1] - W;
 G(h) = (T) Argv[0];
 Go(ret, h); }

Vm(emi_u) {
 ArCh(2);
 TyCh(Argv[0], Num);
 TyCh(Argv[1], Hom);
 obj h = Argv[1] - W;
 G(h) = (terp*) Gn(Argv[0]);
 Go(ret, h); }

Vm(hom_geti_u) {
 ArCh(1);
 TyCh(Argv[0], Hom);
 Go(ret, Pn(G(Argv[0]))); }

Vm(hom_getx_u) {
 ArCh(1);
 TyCh(Argv[0], Hom);
 Go(ret, (obj) G(Argv[0])); }

Vm(hom_seek_u) {
 ArCh(2);
 TyCh(Argv[0], Hom);
 TyCh(Argv[1], Num);
 Go(ret, Ph(Gh(Argv[0])+Gn(Argv[1]))); }

// hash tables
Vm(tblg) {
 ArCh(2);
 TyCh(Argv[0], Tbl);
 xp = tblget(v, Argv[0], Argv[1]);
 Go(ret, xp ? xp : nil); }

Vm(tget) {
 xp = tblget(v, xp, *sp++);
 Ap(ip+W, xp ? xp : nil); }

#define ok Pn(1)
Vm(thas)  { xp = tblget(v, xp, *sp++) ? ok : nil; N(1); }
Vm(tlen)  { xp = putnum(gettbl(xp)->len); N(1); }
Vm(tkeys) { O x; CallC(x = tblkeys(v, xp)); xp = x; N(1); }

Vm(tblc) {
 ArCh(2);
 TyCh(Argv[0], Tbl);
 xp = tblget(v, Argv[0], Argv[1]);
 Go(ret, xp ? Pn(0) : nil); }

static obj tblss(lips v, i64 i, i64 l) {
 mem fp = Fp;
 R i > l-2 ? Argv[i-1] :
  (tblset(v, Xp, Argv[i], Argv[i+1]),
   tblss(v, i+2, l)); }

Vm(tbls) {
 obj x = nil;
 ArCh(1);
 TyCh(xp = *Argv, Tbl);
 CallC(x = tblss(v, 1, Gn(Argc)));
 Go(ret, x); }

Vm(tblmk) {
 CallC(Xp = table(v), tblss(v, 0, Gn(Argc)));
 Go(ret, Xp); }

Vm(tbld) {
 obj x = nil;
 ArCh(2);
 TyCh(Argv[0], Tbl);
 CallC(x = tbldel(v, Argv[0], Argv[1]));
 Go(ret, x); }

Vm(tblks) {
 ArCh(1);
 TyCh(Argv[0], Tbl);
 obj x;
 CallC(x = tblkeys(v, Argv[0]));
 Go(ret, x); }

Vm(tbll) {
 ArCh(1);
 TyCh(Argv[0], Tbl);
 Go(ret, Pn(gettbl(*Argv)->len)); }

// string instructions
Vm(strl) {
 ArCh(1);
 TyCh(*Argv, Oct);
 Go(ret, Pn(getoct(*Argv)->len-1)); }

Vm(strg) {
 ArCh(2);
 TyCh(Argv[0], Oct);
 TyCh(Argv[1], Num);
 Go(ret, Gn(Argv[1]) < getoct(Argv[0])->len-1 ?
  Pn(getoct(Argv[0])->text[Gn(Argv[1])]) :
  nil); }

Vm(strc) {
 i64 l = Gn(Argc), sum = 0, i = 0;
 while (i < l) {
  obj x = Argv[i++];
  TyCh(x, Oct);
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
  bcpy(d->text+sum, x->text, x->len - 1); }
 Go(ret, putoct(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(strs) {
 ArCh(3);
 TyCh(Argv[0], Oct);
 TyCh(Argv[1], Num);
 TyCh(Argv[2], Num);
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
 bcpy(dst->text, src->text + lb, ub - lb);
 Go(ret, putoct(dst)); }

Vm(strmk) {
 i64 i, l = Gn(Argc)+1, size = 1 + b2w(l);
 Have(size);
 str s = (oct) hp;
 hp += size;
 for (i = 0; i < l-1; i++) {
  obj x = Argv[i];
  TyCh(x, Num);
  if (x == Pn(0)) break;
  s->text[i] = Gn(x); }
 s->text[i] = 0;
 s->len = i+1;
 Go(ret, putoct(s)); }

Vm(vararg) {
 i64 reqd = Gn(GF(ip)),
     vdic = Gn(Argc) - reqd;
 ArCh(reqd);
 // in this case we need to add another argument
 // slot to hold the nil.
 if (!vdic) {
  Have1();
  sp = --fp;
  for (Z i = 0; i < Size(fr) + reqd; i++)
   fp[i] = fp[i+1];
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
  for (Z i = vdic; i--;)
   t[i].x = Argv[reqd + i],
   t[i].y = puttwo(t+i+1);
  t[vdic-1].y = nil;
  Argv[reqd] = puttwo(t); }
 N(2); }

// the next few functions create and store
// lexical environments.
static Vm(encl) {
 i64 n = Xp;
 obj x = Ob GF(ip);
 mem block = hp;
 hp += n;
 obj arg = nil; // optional argument array
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
 at[1] = (T) puttup(t);
 at[2] = (T) X(x);
 at[3] = 0;
 at[4] = (T) at;

 Ap(ip+W2, Ph(at)); }

Vm(prencl) {
 i64 n = Gn(Argc);
 n += n ? 12 : 11;
 Have(n);
 Xp = n;
 Jump(encl); }

Vm(encll) { Go(prencl, Locs); }
Vm(encln) { Go(prencl, nil); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
Vm(pc0) {
 obj ec = Ob GF(ip),
     arg = AR(ec)[0],
     loc = AR(ec)[1];
 i64 adic = nilp(arg) ? 0 : AL(arg);
 Have(Size(fr) + adic + 1);
 i64 off = (M) fp - sp;
 G(ip) = pc1;
 sp -= adic;
 for (Z z = adic; z--; sp[z] = AR(arg)[z]);
 ec = Ob GF(ip);
 fp = sp -= Size(fr);
 Retp = ip;
 Subd = Pn(off);
 Argc = Pn(adic);
 Clos = AR(ec)[2];
 if (!nilp(loc)) *--sp = loc;
 ip = AR(ec)[3];
 N(0); }

// finalize function instance closure
Vm(pc1) {
 G(ip) = clos;
 GF(ip) = (T) xp;
 N(0); }

// this is used to create closures.
Vm(take) {
 Z n = Gn(Ob GF(ip));
 Have(n + 1);
 vec t = (vec) hp;
 hp += n + 1;
 t->len = n;
 wcpy(t->xs, sp, n);
 sp += n;
 Go(ret, puttup(t)); }

// print to console
Vm(em_u) {
 i64 l = Gn(Argc), i;
 if (l) {
  for (i = 0; i < l - 1; i++)
   emsep(v, Argv[i], stdout, ' ');
  emit(v, xp = Argv[i], stdout); }
 fputc('\n', stdout);
 Jump(ret); }

// pairs
Vm(cons) {
 Have1(); hp[0] = xp, hp[1] = *sp++;
 xp = puttwo(hp); hp += 2; N(1); }
Vm(car) { Ap(ip+W, X(xp)); }
Vm(cdr) { Ap(ip+W, Y(xp)); }

Vm(cons_u) {
 i64 aa = Gn(Argc);
 if (!aa) Jump(nope);
 Have(2); hp[0] = Argv[0], hp[1] = aa == 1 ? nil : Argv[1];
 xp = puttwo(hp), hp += 2; Jump(ret); }
Vm(car_u) { ArCh(1); TyCh(*Argv, Two); Go(ret, X(*Argv)); }
Vm(cdr_u) { ArCh(1); TyCh(*Argv, Two); Go(ret, Y(*Argv)); }

// arithmetic
Vm(neg) { Ap(ip+W, Pn(-Gn(xp))); }
Vm(add) { xp = xp + *sp++ - Num; N(1); }
Vm(sub) { xp = *sp++ - xp + Num; N(1); }
Vm(mul) { xp = Pn(Gn(xp) * Gn(*sp++)); N(1); }
Vm(dqv) {
 if (xp == Pn(0)) Jump(nope);
 xp = Pn(Gn(*sp++) / Gn(xp));
 N(1); }
Vm(mod) {
 if (xp == Pn(0)) Jump(nope);
 xp = Pn(Gn(*sp++) % Gn(xp));
 N(1); }

#define mm_u(_c,_v,_z,op){\
 O x,m=_z,*xs=_v,*l=xs+_c;\
 if (_c) Fo(;xs<l;m=m op Gn(x)){\
  x = *xs++; TyCh(x, Num);}\
 Go(ret, Pn(m));}
#define mm_u0(_c,_v,_z,op){\
 O x,m=_z,*xs=_v,*l=xs+_c;\
 if (_c) Fo(;xs<l;m=m op Gn(x)){\
  x = *xs++; TyCh(x, Num);\
  if (x == Pn(0)) Jump(nope);}\
 Go(ret, Pn(m));}

Vm(add_u) {
 mm_u(Gn(Argc), Argv, 0, +); }
Vm(mul_u) {
 mm_u(Gn(Argc), Argv, 1, *); }
Vm(sub_u) {
 i64 i = Gn(Argc);
 if (i == 0) Go(ret, Pn(0));
 TyCh(*Argv, Num);
 if (i == 1) Go(ret, Pn(-Gn(*Argv)));
 mm_u(i-1,Argv+1,Gn(Argv[0]),-); }

Vm(div_u) {
 Z i = Gn(Argc);
 if (i == 0) Go(ret, Pn(1));
 TyCh(*Argv, Num);
 mm_u0(i-1,Argv+1,Gn(*Argv),/); }
Vm(mod_u) {
 Z i = Gn(Argc);
 if (i == 0) Go(ret, Pn(1));
 TyCh(*Argv, Num);
 mm_u0(i-1,Argv+1,Gn(*Argv),%); }

#define Tf(x) ((x)?ok:nil)
// type predicates
Vm(numpp) { Ap(ip+W, Tf(nump(xp))); }
Vm(hompp) { Ap(ip+W, Tf(homp(xp))); }
Vm(twopp) { Ap(ip+W, Tf(twop(xp))); }
Vm(sympp) { Ap(ip+W, Tf(symp(xp))); }
Vm(strpp) { Ap(ip+W, Tf(octp(xp))); }
Vm(tblpp) { Ap(ip+W, Tf(tblp(xp))); }
Vm(nilpp) { Ap(ip+W, Tf(nilp(xp))); }
Vm(vecpp) { Ap(ip+W, Tf(tupp(xp))); }


// pairs are immutable, so we can take this opportunity to
// deduplicate them.
St int twoeq(O a, O b) {
 if (!eql(X(a), X(b))) return false;
 X(a) = X(b);
 if (!eql(Y(a), Y(b))) return false;
 Y(a) = Y(b);
 return true; }

static int streq(obj a, obj b) {
 str o = getoct(a), m = getoct(b);
 if (o->len != m->len) return false;
 for (Z i = 0; i < o->len; i++)
  if (o->text[i] != m->text[i]) return false;
 return true; }

int eql(obj a, obj b) {
 if (a == b)             return true;
 if (kind(a) != kind(b)) return false;
 switch (kind(a)) {
  case Two: return twoeq(a, b);
  case Oct: return streq(a, b);
  Df:       return false; } }

Vm(lt)    { xp = *sp++ <  xp ? xp : nil; N(1); }
Vm(lteq)  { xp = *sp++ <= xp ? xp : nil; N(1); }
Vm(gteq)  { xp = *sp++ >= xp ? xp : nil; N(1); }
Vm(gt)    { xp = *sp++ >  xp ? xp : nil; N(1); }
// there should be a separate instruction for simple equality.
Vm(eq)    { xp = eql(xp, *sp++) ? ok : nil; N(1); }

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

Vm(lt_u)   { ord_w(<); }
Vm(lteq_u) { ord_w(<=); }
Vm(eq_u)   { ord_wv(eql); }
Vm(gteq_u) { ord_w(>=); }
Vm(gt_u)   { ord_w(>); }

#define typpp(t) {\
 for (obj *xs = Argv, *l=xs+Gn(Argc);xs<l;)\
  if (kind(*xs++)!=t) Go(ret, nil);\
 Go(ret, ok); }
Vm(nump_u) { typpp(Num); }
Vm(homp_u) { typpp(Hom); }
Vm(strp_u) { typpp(Oct); }
Vm(tblp_u) { typpp(Tbl); }
Vm(twop_u) { typpp(Two); }
Vm(symp_u) { typpp(Sym); }
Vm(nilp_u) { typpp(Nil); }
Vm(vecp_u) { typpp(Tup); }

// stack manipulation
Vm(tuck) { Have1(); sp--, sp[0] = sp[1], sp[1] = xp; N(1); }
Vm(drop) { sp++; N(1); }
Vm(dupl) { Have1(); --sp; sp[0] = sp[1]; N(1); }

// errors
Vm(fail) { Jump(nope); }
Vm(zzz) { exit(EXIT_SUCCESS); }
Vm(gsym_u) {
 Have(sizeof(struct sym)/W);
 sym y = (sym) hp;
 hp += Size(sym);
 y->nom = y->l = y->r = nil;
 y->code = v->count++ * mix;
 Go(ret, putsym(y)); }

Vm(hfin_u) {
 ArCh(1);
 TyCh(*Argv, Hom);
 obj a = *Argv;
 GF(button(Gh(a))) = (terp*) a;
 Go(ret, a); }

Vm(ev_u) {
 ArCh(1);
 obj x;
 CallC(x = compile(v, *Argv),
       x = G(x)(v, x, Fp, Sp, Hp, nil));
 Go(ret, x); }

Vm(rnd_u) { Go(ret, Pn(rand())); }


// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
static Inline u0 perrarg(V v, M fp) {
 i64 i = 0, argc = fp == Pool + Len ? 0 : Gn(Argc);
 if (argc) for (fputc(' ', stderr);;fputc(' ', stderr)) {
  obj x = Argv[i++];
  emit(v, x, stderr);
  if (i == argc) break; }
 fputc(')', stderr); }

St Vm(nope) {
 fputs("# (", stderr), emit(v, Ph(ip), stderr),
 perrarg(v, fp);
 fputs(" does not exist\n", stderr);
 for (;;) {
  ip = Retp, fp += Size(fr) + Gn(Argc) + Gn(Subd);
  if (button(Gh(ip))[-1] == yield) Bk;
  fputs("#  in ", stderr), emsep(v, Ph(ip), stderr, '\n'); }
 return Hp = hp, restart(v); }

obj restart(V v) {
 Fp = Sp = Pool + Len;
 Xp = Ip = nil;
 v->mem_root = NULL;
 longjmp(v->restart, 1); }
