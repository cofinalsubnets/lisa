#include "i.h"


// at all times vm is running a function. thread compiler tracks
// function state using this type
typedef struct scope {
  // these parameters represent stack state at a point in compile process
  word args, // list // positional stack bindings of function
       imps, // list // enclosed stack bindings of function
       pals; // list // expression level stack bindings
  // these are values of variables known at compile time
  word lams; // dict // known function definitions
  // these are two stacks of jump target addresses for conditional expressions
  word alts, // list // alternate branch address stack
       ends; // list // exit branch address stack
  // this is the enclosing function scope if any
  struct scope *par;
} *scope;


// scope constructor
static scope enscope(core f, scope par, word args, word imps) {
  if (!pushs(f, 3, args, imps, par)) return 0;
  scope c = (scope) mo_n(f, 7);
  if (c)
    c->args = pop1(f), c->imps = pop1(f),
    c->par = (scope) pop1(f),
    c->pals = c->alts = c->ends = nil,
    c->lams = nil;
  return c; }

// compiler operates in two phases
// - analyze expression; put continuation on stack; compute code size bound
#define C0(n, ...) size_t n(core f, scope *c, size_t m, word x, ##__VA_ARGS__)
typedef C0(c0);
// and
// - allocate thread; pass to continuation; emit code and trim extra length
#define C1(n, ...) thread n(core f, scope *c, thread k, ##__VA_ARGS__)
typedef C1(c1);

static thread c1apn(core, scope*, thread);

static c0 analyze_if, analyze_let, analyze_arguments;
// basic functions
static C1(yieldk) { return k; }
static C1(pull) { return ((c1*) (*f->sp++))(f, c, k); }
static thread construct(core f, scope *c, size_t m) {
  thread k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(word));
  return pull(f, c, k + m); }
static C1(c1i) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static C1(c1ix) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }
// generic instruction c0 handlers
static size_t em1(core f, scope *c, size_t m, vm *i) {
  return pushs(f, 2, c1i, i) ? m + 1 : 0; }
static size_t em2(core f, scope *c, size_t m, vm *i, word x) {
  return pushs(f, 3, c1ix, i, x) ? m + 2 : 0; }
// analyzer

static NoInline size_t analyze_symbol(core, scope*, size_t, word, scope);
static c0 analyze_list;
static C0(analyze) {
  if (homp(x) && datp(x)) {
    if (htwop(ptr(x))) return analyze_list(f, c, m, x);
    if (hstrp(ptr(x))) return analyze_symbol(f, c, m, x, *c); }
  return em2(f, c, m, K, x); }

static c1 c1var, c2var;
static C0(c0var1) {
  return nilp((word) (*c)->par) ? em2(f, c, m, K, nil) : // XXX undefined case
         pushs(f, 3, c1var, x, (*c)->pals) ? m + 2 :
         0; }

static long index_of(core f, scope c, word var) {
  size_t i = 0;
  // is it a closure variable?
  for (word l = c->imps; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  for (word l = c->args; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  return -1; }

static vm lazy_bind, drop, define;

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
       idx = index_of(f, *c, var);
  return
    k[-2].ap = ref,
    k[-1].x = putnum(idx + ins),
    pull(f, c, k - 2); }

static size_t analyze_symbol(core f, scope *c, size_t m, word k, scope d) {
  word x;
  if (nilp((word) d)) {
    x = table_get(f, f->dict, k, 0);
    if (x) return em2(f, c, m, K, x);
    k = (word) pairof(f, k, (*c)->imps),
    k = k ? A((*c)->imps = k) : k;
    return k ? c0var1(f, c, m, k) : 0; }

  // look in vals
  if ((x = lassoc(f, d->lams, k))) {
    // lazy bind
    bind(x, (word) pairof(f, x, (word) d));
    bind(m, em2(f, c, m, lazy_bind, x));
    x = B(B(A(f->sp[2]))); // get the closure args to pass in
    return analyze_arguments(f, c, m, x); } // XXX

  // look in pals
  if ((x = lidx(f, d->pals, k)) >= 0)
    return pushs(f, 3, c2var, k, d->pals) ? m + 2 : 0;

  // look in imps args
  x = index_of(f, d, k);
  if (x >= 0) {
    if (*c != d)
      k = (word) pairof(f, k, (*c)->imps),
      k = k ? A((*c)->imps = k) : k;
    return k ? c0var1(f, c, m, k) : 0; }
  // recur on outer scope
  return analyze_symbol(f, c, m, k, d->par); }

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

// codegen apply function argument
static size_t analyze_arguments(core f, scope *c, size_t m, word b) {
  MM(f, &b); // handle oom here ..
  if (!((*c)->pals = (word) pairof(f, nil, (*c)->pals))) m = 0;
  else {
    for (; m && twop(b); b = B(b))
      m = analyze(f, c, m + 1, A(b)),
      m = m && pushs(f, 1, c1ap) ? m : 0;
    (*c)->pals = B((*c)->pals); }
  UM(f);
  return m; }

// lambda decons pushes last list item to stack returns init of list
static word linit(core f, word x) {
  if (!twop(x)) return pushs(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushs(f, 1, A(x)) ? nil : 0;
  word y = A(x);
  return avec(f, y, x = linit(f, B(x))),
         x ? (word) pairof(f, y, x) : x; }

static word analyze_lambda(core f, scope *c, word imps, word exp) {
  // storing exp in scope->args for the moment is expedient
  scope d = enscope(f, *c, exp, imps);
  if (!d) return 0;
  MM(f, &d);
  // get the real args
  word args = linit(f, d->args);
  if (!(d->args = args)) goto fail;
  exp = f->sp[0], f->sp[0] = (word) yieldk;
  size_t m = analyze(f, &d, 4, exp);
  if (!m) goto fail;
  size_t arity = llen(d->args) + llen(d->imps);
  thread k = pushs(f, 3, c1ix, ret, putnum(arity)) ? construct(f, &d, m) : 0;
  if (!k) goto fail;
  if (arity > 1) (--k)->x = putnum(arity), (--k)->ap = cur;
  ttag(k)->head = k;
  UM(f);
  return (word) pairof(f, (word) k, d->imps);
fail:
  UM(f);
  return 0; }

// conditionals
// to emit targeted jumps etc
static c1 c1endpush, c1endpop, c1postbranch, c1altpush, c1endpeek;

// conditional expression analyzer
static size_t analyze_if(core f, scope *c, size_t m, word x) {
  if (!pushs(f, 2, x, c1endpop)) return 0;
  struct pair p = { data, &typ_two, nil, nil };
  for (x = pop1(f), MM(f, &x); m; x = B(B(x))) {
    if (!twop(x)) x = (word) &p;
    m = analyze(f, c, m + 2, A(x));
    if (!twop(B(x))) { // at end, default branch
      m = pushs(f, 1, c1endpeek) ? m : 0;
      break; }
    m = pushs(f, 1, c1postbranch) ? m : 0;
    m = m ? analyze(f, c, m + 2, A(B(x))) : m;
    m = pushs(f, 2, c1altpush, c1endpeek) ? m : 0; }
  return UM(f), m && pushs(f, 1, c1endpush) ? m : 0; }

// first emitter called for cond expression
// pushes cond expression exit address onto scope stack ends
static C1(c1endpush) {
  pair w = pairof(f, (word) k, (*c)->ends);
  return !w ? 0 : pull(f, c, (thread) A((*c)->ends = (word) w)); }

// last emitter called for cond expression
// pops cond expression exit address off scope stack ends
static C1(c1endpop) {
  return (*c)->ends = B((*c)->ends), pull(f, c, k); }

static C1(c1altpush) {
  pair w = pairof(f, (word) k, (*c)->alts);
  if (!w) return (thread) w;
  (*c)->alts = (word) w;
  k = (thread) w->a;
  return pull(f, c, k); }

static C1(c1endpeek) {
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
static C1(c1postbranch) {
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
  if (!lassoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static word desugr(core f, word *d, word *e, word a) {
  if (!twop(a)) return (word) pairof(f, *e, nil);
  word b; avec(f, a, b = desugr(f, d, e, B(a)));
  return !b ? b : (word) pairof(f, A(a), b); }

static status desug(core f, word *d, word *e) {
  if (!twop(*d)) return Ok;
  word x, l = (word) literal_string(f, "\\");
  if (!l || !pushs(f, 1, l)) return Oom;
  do if (!(x = (word) desugr(f, d, e, B(*d))) ||
         !(x = (word) pairof(f, f->sp[0], x)))
    return Oom;
  else *d = A(*d), *e = x;
  while (twop(*d));
  return f->sp++, Ok; }

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
      word x = analyze_lambda(f, c, nil, B(A(def)));
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
        for (word v = B(A(d)); twop(v); v = B(v)) { // then you need its variables
          word vars = B(B(A(e))), var = A(v);
          if (lidx(f, vars, var) < 0 && !(vars = (word) pairof(f, var, vars))) goto fail; // oom
          else if (vars != B(B(A(e)))) B(B(A(e))) = vars, j++; } // if list is updated then record the change
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); e = B(e)) B(B(A(e))) = ldels(f, lam, B(B(A(e))));

  string l;

  (*c)->lams = lam, e = nil;
  // construct lambda with reversed argument list
  exp = lconcat(f, nom, exp);
  l = literal_string(f, "\\");
  exp = exp && l ? (word) pairof(f, (word) l, exp) : 0;
  if (!exp) goto fail;
  // exp is now the required lambda expression, analyze it
  m = analyze(f, b, m, exp);
  if (!m) goto fail;
  if (!((*b)->pals = (word) pairof(f, nil, (*b)->pals))) goto fail;
  // now evaluate definitions in order tracking var names on pals list
  // first reverse the nom and def lists
  nom = rlconcat(f, nom, nil), def = rlconcat(f, def, nil);
  size_t nn = 0;
  // store lambdas on scope for lazy binding and construct new lambda application expression
  // - reverse noms onto exp
  // - reverse expressions onto e = nil and recompile lambdas
  for (; twop(nom); nom = B(nom), def = B(def), nn++) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = lassoc(f, lam, A(nom));
      word _;
      if (!(_ = analyze_lambda(f, c, B(B(d)), B(A(def))))) goto fail;
      else A(def) = B(d) = _; }
    // if toplevel then bind
    if (even && nilp((*b)->args)) {
      thread t = cells(f, 2 * Width(struct pair) + 2 + Width(struct tag));
      if (!t) goto fail;
      two w = (two) t,
          x = w + 1;
      t += 2 * Width(struct pair);
      t[0].ap = define, t[1].x = A(nom), t[2].x = 0, t[3].m = t;
      ini_pair(w, A(def), nil); // dict add
      ini_pair(x, (word) t, (word) w);
      A(def) = (word) x; }
    if (!(m = analyze(f, b, m, A(def))) ||
        !((*b)->pals = (word) pairof(f, A(nom), (*b)->pals)))
      goto fail; }
  m = pushs(f, 2, c1apn, putnum(nn)) ? m + 2 : 0;
  if (m) for (nn++; nn--; (*b)->pals = B((*b)->pals));
done: return UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), m;
fail: m = 0; goto done; }

static size_t analyze_let(core f, scope *c, size_t m, word x) {
  scope d = *c;
  avec(f, x, d = enscope(f, d, d->args, d->imps));
  avec(f, d, m = c0l(f, c, &d, m, x));
  return m; }

static Vm(lazy_bind) {
  word ref = ip[1].x, var = A(A(ref));
  scope env = (scope) B(ref);
  var = A(B(lassoc(f, env->lams, var)));
  ip[0].ap = K;
  ip[1].x = var;
  return K(f, ip, hp, sp); }

static size_t analyze_sequence(core f, scope *c, size_t m, word x) {
  if (!twop(x)) return em2(f, c, m, K, nil);
  for (MM(f, &x); m && twop(B(x)); x = B(x))
    m = analyze(f, c, m, A(x)),
    m = m ? em1(f, c, m, drop) : m;
  return UM(f), m ? analyze(f, c, m, A(x)) : m; }

static size_t analyze_list(core f, scope *c, size_t m, word x) {
  word a = A(x), b = B(x);
  if (!twop(b)) return em2(f, c, m, K, a); // singleton list quote
  if (strp(a)) {
    if (((string) a)->len == 1)
      switch (((string) a)->text[0]) { // special form?
        case '`': return em2(f, c, m, K, twop(b) ? A(b) : nil); // quote
        case ',': return analyze_sequence(f, c, m, b); // sequence
        case ':': return analyze_let(f, c, m, b);
        case '?': return analyze_if(f, c, m, b);
        case '\\': return (x = analyze_lambda(f, c, nil, b)) ? analyze(f, c, m, x) : x; }
    if ((x = table_get(f, f->macro, a, 0))) { // macro?
      x = (word) pairof(f, b, x);
      if (x) b = x, x = B(b), B(b) = nil, x = (word) pairof(f, b, x);
      if (x) b = x, x = B(b), B(b) = nil, x = (word) pairof(f, x, b);
      return !x || !pushs(f, 1, x) || eval(f) != Ok ? 0 : analyze(f, c, m, pop1(f)); } }
  avec(f, b, m = analyze(f, c, m, a));
  return m ? analyze_arguments(f, c, m, b) : m; }

static Vm(drop) { return ip[1].ap(f, ip + 1, hp, sp + 1); }
static Vm(define) {
  Pack(f);
  if (!table_set(f, f->dict, ip[1].x, sp[0])) return Oom;
  Unpack(f);
  return op(1, sp[0]); }

// compile and execute expression
NoInline status eval(core f) {
  size_t m = 1;
  scope c = enscope(f, (scope) nil, nil, nil);
  if (!c) return Oom;
  word x = f->sp[0];
  f->sp[0] = (word) yieldk;
  thread k = 0;
  avec(f, c,
    m = analyze(f, &c, m, x),
    m = m ? em1(f, &c, m, yield) : m,
    k = m ? construct(f, &c, m) : k);
  k = k ? (thread) pushs(f, 1, k) : k;
  if (!k) return Oom;
  f->sp[0] = (word) f->ip;
  f->ip = k;
  status s = k->ap(f, k, f->hp, f->sp);
  if (s == Ok)
    x = f->sp[0],
    f->ip = (thread) *++f->sp,
    f->sp[0] = x;
  return s; }
