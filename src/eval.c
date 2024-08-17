#include "i.h"


// index of item in list
static long lidx(core f, word l, word x) {
  for (long i = 0; twop(l); l = B(l), i++)
    if (eql(f, A(l), x)) return i;
  return -1; }
// list length
static size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
thread mo_ini(void *_, size_t len) {
  struct tag *t = (void*) ((cell) _ + len);
  return t->null = NULL, t->head = _; }

// allocate a thread
thread mo_n(core f, size_t n) {
  thread k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

struct tag *ttag(thread k) {
  return k->x ? ttag(k + 1) : (void*) k; }

static vm tie, drop, define;

typedef struct scope { // track lexical scope
  word args, // stack bindings // all variables on stack
       imps, // internal bindings // closure variables // prefix of sb
       pals, // stack inset // number of arguments on stack
       lams,
       alts,
       ends; // stacks used for branch addresses
  struct scope *par;
} *scope;


static thread analyze(core, word);

// compile and execute expression
NoInline status eval(core f, word x) {
  status s; thread i = f->ip, k;
  avec(f, i,
    k = analyze(f, x),
    s = !k ? Oom : k->ap(f, k, f->hp, f->sp));
  return f->ip = i, s; }

static scope enscope(core f, scope par, word args, word imps) {
  return (scope) thd(f, 7, args, imps, nil, nil, nil, nil, par); }

// compiler operates in two phases
// - analyze expression; put continuation on stack; compute code size bound
#define C0(n, ...) size_t n(core f, scope *c, size_t m, word x, ##__VA_ARGS__)
typedef C0(c0);
// and
// - allocate thread; pass to continuation; emit code and trim extra length
#define C1(n, ...) thread n(core f, scope *c, thread k, ##__VA_ARGS__)
typedef C1(c1);

static thread c1apn(core, scope*, thread);

static c0 c0cond, c0let, c0args;
// basic functions
static C1(yieldk) { return k; }
static C1(pull) { return ((c1*) (*f->sp++))(f, c, k); }
static thread cata(core f, scope *c, size_t m) {
  thread k = mo_n(f, m);
  if (k) memset(k, -1, m * sizeof(word)), k += m;
  return pull(f, c, k); }
static C1(c1i) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static C1(c1ix) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }
// generic instruction c0 handlers
static size_t c0i(core f, scope *c, size_t m, vm *i) {
  return pushs(f, 2, c1i, i) ? m + 1 : 0; }
static size_t c0ix(core f, scope *c, size_t m, vm *i, word x) {
  return pushs(f, 3, c1ix, i, x) ? m + 2 : 0; }
// analyzer
static c0 ana;
static thread analyze(core f, word x) {
  size_t m;
  thread k = 0;
  scope c = pushs(f, 2, x, yieldk) ? enscope(f, (scope) nil, nil, nil) : NULL;
  if (c) avec(f, c,
    m = ana(f, &c, 1, pop1(f)),
    m = m ? c0i(f, &c, m, yield) : m,
    k = m ? cata(f, &c, m) : k);
  return k; }

static NoInline size_t look(core, scope*, size_t, word, scope);
static c0 c0list;
static size_t ana(core f, scope *c, size_t m, word x) {
  if (homp(x) && datp(x)) {
    if (htwop(ptr(x))) return c0list(f, c, m, x);
    if (hstrp(ptr(x))) return look(f, c, m, x, *c); }
  return c0ix(f, c, m, K, x); }

static c1 c1var, c2var;
static size_t c0var1(core f, scope *c, size_t m, word x) {
  return nilp((word) (*c)->par) ? c0ix(f, c, m, K, nil) :
         pushs(f, 3, c1var, x, (*c)->pals) ? m + 2 :
         0; }

static long indexof(core f, scope c, word var) {
  size_t i = 0;
  // is it a closure variable?
  for (word l = c->imps; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  for (word l = c->args; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  return -1; }

static thread c2var(core f, scope *c, thread k) {
  word var = *f->sp++,
       pals = *f->sp++;
  size_t i = lidx(f, pals, var);
  return
    k[-2].ap = ref,
    k[-1].x = putnum(i),
    pull(f, c, k - 2); }

// emit stack reference instruction
static thread c1var(core f, scope *c, thread k) {
  word var = *f->sp++, // variable name
       ins = llen(*f->sp++), // stack inset
       idx = indexof(f, *c, var);
  return
    k[-2].ap = ref,
    k[-1].x = putnum(idx + ins),
    pull(f, c, k - 2); }

static word immval(core f, scope *c, word x) {
  return nump(x) || !datp(x) ? x :
         htwop((thread) x) && !twop(B(x)) ? A(x):
         0; }

// c0 lazy reference
static size_t c0laz(core f, scope *c, size_t m, word x, scope d) {
  bind(x, (word) pairof(f, x, (word) d));
  bind(m, c0ix(f, c, m, tie, x)); // deferred resolve instruction
  return c0args(f, c, m, B(B(A(f->sp[2])))); }

static word assoc(core f, word l, word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, A(A(l)))) return A(l);
  return 0; }

static size_t look(core f, scope *c, size_t m, word k, scope d) {
  word x;
  if (nilp((word) d)) {
    x = assoc(f, f->dict, k);
    if (x) return c0ix(f, c, m, K, B(x));
    k = (word) pairof(f, k, (*c)->imps),
    k = k ? A((*c)->imps = k) : k;
    return k ? c0var1(f, c, m, k) : 0; }

  // look in vals
  if ((x = assoc(f, d->lams, k)))
    return c0laz(f, c, m, x, d);

  // look in pals
  if ((x = lidx(f, d->pals, k)) >= 0)
    return pushs(f, 3, c2var, k, d->pals) ? m + 2 : 0;

  // look in imps args
  x = indexof(f, d, k);
  if (x >= 0) {
    if (*c != d)
      k = (word) pairof(f, k, (*c)->imps),
      k = k ? A((*c)->imps = k) : k;
    return k ? c0var1(f, c, m, k) : 0; }
  // recur on outer scope
  return look(f, c, m, k, d->par); }

// emits call instruction and modifies to tail call
// if next operation is return
static thread c1ap(core f, scope *c, thread k) {
  if (k->ap == ret) k->ap = tap; // tail call
  else (--k)->ap = ap; // regular call
  return pull(f, c, k); } // ok

static thread c1apn(core f, scope *c, thread k) {
  word n = *f->sp++;
  if (k->ap == ret) k->x = n, (--k)->ap = tapn;
  else (--k)->x = n, (--k)->ap = apn;
  return pull(f, c, k); }

// cons on list unless already there
static word uinsert(core f, word l, word x) { return
  lidx(f, l, x) < 0 ? (word) pairof(f, x, l) : l; }

// codegen apply function argument
static size_t c0args(core f, scope *c, size_t m, word b) {
  MM(f, &b); // handle oom here ..
  if (((*c)->pals = (word) pairof(f, nil, (*c)->pals))) {
    for (; m && twop(b); b = B(b))
      m = ana(f, c, m + 1, A(b)),
      m = m && pushs(f, 1, c1ap) ? m : 0;
    (*c)->pals = B((*c)->pals); }
  else m = 0;
  return UM(f), m; }

// whole innerlambda thread whateverer
// returns pair of thread and closure variable symbols
static pair c0lambi(core f, scope *c, word x) {
  if (!pushs(f, 2, x, yieldk)) return 0;
  size_t m; bind(m, ana(f, c, 4, pop1(f))); // include 4 for ret(n) and curry(n)
  size_t arity = // including closure args
    llen((*c)->args) + llen((*c)->imps);
  thread k; bind(k, // return from appropriate height
    pushs(f, 3, c1ix, ret, putnum(arity)) ? cata(f, c, m) : 0); // allocate & emit
  if (arity > 1) // curry if more than 1 argument
    (--k)->x = putnum(arity),
    (--k)->ap = cur;
  // all the code has been emitted so now trim the thread
  ttag(k)->head = k;
  // apply to enclosed variables (if none then quoting has no effect)
  return pairof(f, (word) k, (*c)->imps); }

// lambda decons pushes last list item to stack returns init of list
static word ldepairof(core f, word x) {
  if (!twop(x)) return pushs(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushs(f, 1, A(x)) ? nil : 0;
  word y = A(x);
  return avec(f, y, x = ldepairof(f, B(x))),
         x ? (word) pairof(f, y, x) : x; }

// lambda wrapper parses expression and manages inner scope
static word c0lambw(core f, scope *c, word imps, word exp) {
  avec(f, imps, exp = ldepairof(f, exp));
  scope d = exp ? enscope(f, *c, exp, imps) : 0;
  avec(f, d, exp = d ? (word) c0lambi(f, &d, pop1(f)) : 0);
  return exp; }

// conditionals
// to emit targeted jumps etc
static c1 c1endpush, c1endpop, c1postbranch, c1altpush, c1endpeek;

// conditional expression analyzer
static size_t c0cond(core f, scope *c, size_t m, word x) {
  if (!pushs(f, 2, x, c1endpop)) return 0;
  struct two p = { data, &typ_two, nil, nil };
  for (x = pop1(f), MM(f, &x); m; x = B(B(x))) {
    if (!twop(x)) x = (word) &p;
    m = ana(f, c, m + 2, A(x));
    if (!twop(B(x))) { // at end, default branch
      m = pushs(f, 1, c1endpeek) ? m : 0;
      break; }
    m = pushs(f, 1, c1postbranch) ? m : 0;
    m = m ? ana(f, c, m + 2, A(B(x))) : m;
    m = pushs(f, 2, c1altpush, c1endpeek) ? m : 0; }
  return UM(f), m && pushs(f, 1, c1endpush) ? m : 0; }

// first emitter called for cond expression
// pushes cond expression exit address onto scope stack ends
static thread c1endpush(core f, scope *c, thread k) {
  pair w = pairof(f, (word) k, (*c)->ends);
  return !w ? 0 : pull(f, c, (thread) A((*c)->ends = (word) w)); }

// last emitter called for cond expression
// pops cond expression exit address off scope stack ends
static thread c1endpop(core f, scope *c, thread k) {
  return (*c)->ends = B((*c)->ends), pull(f, c, k); }

static thread c1altpush(core f, scope *c, thread k) {
  pair w = pairof(f, (word) k, (*c)->alts);
  if (!w) return (thread) w;
  (*c)->alts = (word) w;
  k = (thread) w->a;
  return pull(f, c, k); }

static thread c1endpeek(core f, scope *c, thread k) {
  k -= 2;
  thread addr = (cell) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x;
  else k[0].ap = jump, k[1].x = (word) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off scope stack alts
static thread c1postbranch(core f, scope *c, thread k) {
  return k[-2].ap = cond,
         k[-1].x = A((*c)->alts),
         (*c)->alts = B((*c)->alts),
         pull(f, c, k - 2); }

static bool lambp(core f, word x) {
  if (!twop(x) || !strp(x = A(x))) return false;
  string s = (string) x;
  return s->len == 1 && s->text[0] == '\\'; }

// DEFINE
// let expressions

static word ldels(core f, word lam, word l) {
  if (!twop(l)) return nil;
  word m = ldels(f, lam, B(l));
  if (!assoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static word desugr(core f, word *d, word *e, word a) {
  if (!twop(a)) return (word) pairof(f, *e, nil);
  word b; avec(f, a, b = desugr(f, d, e, B(a)));
  return !b ? b : (word) pairof(f, A(a), b); }

static status desug(core f, word *d, word *e) {
  if (!twop(*d)) return Ok;
  word x, l = (word) strof(f, "\\");
  if (!l || !pushs(f, 1, l)) return Oom;
  do if (!(x = (word) desugr(f, d, e, B(*d))) ||
         !(x = (word) pairof(f, f->sp[0], x)))
    return Oom;
  else *d = A(*d), *e = x;
  while (twop(*d));
  return f->sp++, Ok; }

static word revn(core f, word l, word n) {
  if (!twop(l)) return n;
  avec(f, l, n = revn(f, B(l), n));
  if (n) n = (word) pairof(f, A(l), n);
  return n; }

static word revr(core f, word l, word n) {
  if (!twop(l)) return n;
  word m = l;
  l = B(l), B(m) = n, n = m;
  return revr(f, l, n); }

// this function is loooong
static size_t c0l(core f, scope *b, scope *c, size_t m, word exp) {
  if (!twop(exp)) return nil;
  if (!twop(B(exp))) return A(exp);
  // lots of variables :(
  word nom = nil, def = nil, lam = nil,
       v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v);

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = B(B(exp))) {
    d = A(exp), e = A(B(exp)), desug(f, &d, &e);
    if (!(nom = (word) pairof(f, d, nom)) ||
        !(def = (word) pairof(f, e, def)))
      goto fail;
    else if (lambp(f, A(def))) {
      // if it's a lambda compile it and record in lam list
      word x = c0lambw(f, c, nil, B(A(def)));
      x = x ? (word) pairof(f, A(nom), x) : x;
      x = x ? (word) pairof(f, x, lam) : x;
      if (x) lam = x;
      else goto fail; } }

  // if there's no body then use the last definition
  bool even = !twop(exp);
  if (even) {
    word x = (word) pairof(f, A(nom), nil);
    if (!x) goto fail;
    exp = x; }

  // find closures
  // for each function f with closure C(f)
  // for each function g with closure C(g)
  // if f in C(g) then C(g) include C(f)
  long j;
  do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
    for (e = lam; twop(e); e = B(e)) // for each bound function variable
      if (A(A(d)) != A(A(e)) && // skip yourself
          lidx(f, B(B(A(e))), A(A(d))) >= 0) // if you need this function
        for (word u, v = B(A(d)); twop(v); v = B(v)) { // then you need its variables
          if (!(u = uinsert(f, B(B(A(e))), A(v)))) goto fail; // oom
          else if (u != B(B(A(e)))) B(B(A(e))) = u, j++; } // if list is updated then record the change
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); e = B(e)) B(B(A(e))) = ldels(f, lam, B(B(A(e))));

  string l;

  (*c)->lams = lam, e = nil;
  // construct lambda with reversed argument list
  exp = revn(f, nom, exp);
  l = strof(f, "\\");
  exp = exp && l ? (word) pairof(f, (word) l, exp) : 0;
  if (!exp) goto fail;
  // exp is now the required lambda expression, analyze it
  m = ana(f, b, m, exp);
  if (!m) goto fail;
  if (!((*b)->pals = (word) pairof(f, nil, (*b)->pals))) goto fail;
  // now evaluate definitions in order tracking var names on pals list
  // first reverse the nom and def lists
  nom = revr(f, nom, nil), def = revr(f, def, nil);
  size_t nn = 0;
  // store lambdas on scope for lazy binding and construct new lambda application expression
  // - reverse noms onto exp
  // - reverse expressions onto e = nil and recompile lambdas
  for (; twop(nom); nom = B(nom), def = B(def), nn++) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = assoc(f, lam, A(nom));
      word _;
      if (!(_ = c0lambw(f, c, B(B(d)), B(A(def))))) goto fail;
      else A(def) = B(d) = _; }
    // if toplevel then bind
    if (even && nilp((*b)->args)) {
      thread t = cells(f, 2 * Width(struct two) + 2 + Width(struct tag));
      if (!t) goto fail;
      two w = (two) t,
          x = w + 1;
      t += 2 * Width(struct two);
      t[0].ap = define, t[1].x = A(nom), t[2].x = 0, t[3].m = t;
      ini_two(w, A(def), nil);
      ini_two(x, (word) t, (word) w);
      A(def) = (word) x; }
    if (!(m = ana(f, b, m, A(def))) ||
        !((*b)->pals = (word) pairof(f, A(nom), (*b)->pals)))
      goto fail; }
  m = pushs(f, 2, c1apn, putnum(nn)) ? m + 2 : 0;
  if (m) for (nn++; nn--; (*b)->pals = B((*b)->pals));
done: return UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), m;
fail: m = 0; goto done; }

static size_t c0let(core f, scope *c, size_t m, word x) {
  scope d = *c;
  avec(f, x, d = enscope(f, d, d->args, d->imps));
  avec(f, d, m = c0l(f, c, &d, m, x));
  return m; }

static Vm(tie) {
  word ref = ip[1].x, var = A(A(ref));
  scope env = (scope) B(ref);
  var = A(B(assoc(f, env->lams, var)));
  ip[0].ap = K;
  ip[1].x = var;
  return K(f, ip, hp, sp); }

static size_t c0do(core f, scope *c, size_t m, word x) {
  if (!twop(x)) return c0ix(f, c, m, K, nil);
  for (MM(f, &x); m && twop(B(x)); x = B(x))
    m = ana(f, c, m, A(x)),
    m = m ? c0i(f, c, m, drop) : m;
  return UM(f), m ? ana(f, c, m, A(x)) : m; }

static size_t c0mac(core f, scope *c, size_t m, word x, word b) {
  x = (word) pairof(f, b, x);
  if (x) b = x, x = B(b), B(b) = nil, x = (word) pairof(f, b, x);
  if (x) b = x, x = B(b), B(b) = nil, x = (word) pairof(f, x, b);
  // XXX ignores errors
  return !x || eval(f, x) != Ok ? 0 : ana(f, c, m, pop1(f)); }

static size_t c0ap(core f, scope *c, size_t m, word a, word b);
static size_t c0list(core f, scope *c, size_t m, word x) {
  word a = A(x), b = B(x);
  if (!twop(b)) return c0ix(f, c, m, K, a); // singleton list quote
  if (strp(a)) {
    if (((string) a)->len == 1)
      switch (((string) a)->text[0]) { // special form?
        case ',': return c0do(f, c, m, b);
        case ':': return c0let(f, c, m, b);
        case '?': return c0cond(f, c, m, b);
        case '\\': return (x = c0lambw(f, c, nil, b)) ? ana(f, c, m, x) : x; }
    if ((x = assoc(f, f->macro, a))) // macro?
      return c0mac(f, c, m, B(x), b); }
  return c0ap(f, c, m, a, b); }

static size_t c0apc(core f, scope *c, size_t m, word v, word q, word b) {
  thread ip = f->ip;
  union cell y[] = {{K}, {.x=v}, {K}, {.x=q}, {ap}, {yield}};
  status s;
  avec(f, ip, avec(f, b, s = y->ap(f, y, f->hp, f->sp)));
  f->ip = ip;
  return s == Ok ? c0ap(f, c, m, pop1(f), B(b)) : 0; }

static size_t c0ap(core f, scope *c, size_t m, word a, word b) {
  if (!twop(b)) return c0ix(f, c, m, K, a); // can be reached recursively
  // apply to immediate value now if all it does is curry
  word v = immval(f, c, a), q = immval(f, c, A(b));
  if (q && v && (nump(v) || datp(v) || ptr(v)->ap == cur)) return c0apc(f, c, m, v, q, b);
//  if (v && homp(v) && ptr(v)->ap == cur && getnum(ptr(v)[1].x) == llen(b))
  avec(f, b, m = ana(f, c, m, a)); // evaluate function expression
  return c0args(f, c, m, b); } // apply to arguments

static Vm(drop) { return ip[1].ap(f, ip + 1, hp, sp + 1); }
static Vm(define) {
  Have(2 * Width(struct two));
  two w = ini_two((two) hp, ip[1].x, sp[0]),
      x = ini_two(w + 1, (word) w, f->dict);
  f->dict = (word) x;
  ip = (thread) sp[1];
  sp[1] = sp[0];
  return ip->ap(f, ip, hp + 2 * Width(struct two), sp + 1); }
