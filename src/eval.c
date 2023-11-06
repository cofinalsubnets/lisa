#include "i.h"
static vm tie;

typedef struct scope { // track lexical scope
  word args, // stack bindings // all variables on stack
       imps, // internal bindings // closure variables // prefix of sb
       pals, // stack inset // number of arguments on stack
       lams,
       alts,
       ends; // stacks used for branch addresses
  struct scope *par;
} *scope;


static thread analyze(state, word);

// compile and execute expression
NoInline status eval(state f, word x) {
  thread k = analyze(f, x);
  return k ? k->ap(f, k, f->hp, f->sp) : Oom; }

static scope enscope(state f, scope *par, word args, word imps) {
  scope s;
  avec(f, args, avec(f, imps, s = (scope) mo_n(f, Width(struct scope))));
  if (s) s->args = args, s->imps = imps,
         s->alts = s->ends = s->pals = s->lams = nil,
         s->par = par ? *par : (scope) nil;
  return s; }

// compiler operates in two phases
// - analyze expression; put continuation on stack; compute code size bound
#define C0(n, ...) size_t n(state f, scope *c, size_t m, word x, ##__VA_ARGS__)
typedef C0(c0);
// and
// - allocate thread; pass to continuation; emit code and trim extra length
#define C1(n, ...) thread n(state f, scope *c, thread k, ##__VA_ARGS__)
typedef C1(c1);

static c0 c0cond, c0let, c0args;
// basic functions
static C1(yieldk) { return k; }
static C1(pull) { return ((c1*) (*f->sp++))(f, c, k); }
static thread cata(state f, scope *c, size_t m) {
  thread k = mo_n(f, m);
  if (k) memset(k, -1, m * sizeof(word)), k += m;
  return pull(f, c, k); }
static C1(c1i) { return
  k[-1].x = *f->sp++,
  pull(f, c, k - 1); }
static C1(c1ix) { return
  k[-2].x = *f->sp++,
  k[-1].x = *f->sp++,
  pull(f, c, k - 2); }
// generic instruction c0 handlers
static size_t c0i(state f, scope *c, size_t m, vm *i) {
  return push2(f, (word) c1i, (word) i) ? m + 1 : 0; }
static size_t c0ix(state f, scope *c, size_t m, vm *i, word x) {
  return push3(f, (word) c1ix, (word) i, x) ? m + 2 : 0; }
// analyzer
static c0 ana;
static thread analyze(state f, word x) {
  size_t m;
  thread k = 0;
  scope c = push2(f, x, (word) yieldk) ? enscope(f, NULL, nil, nil) : NULL;
  if (c) avec(f, c,
    m = ana(f, &c, 1, pop1(f)),
    m = m ? c0i(f, &c, m, yield) : m,
    k = m ? cata(f, &c, m) : k);
  return k; }

static size_t seek(state, scope*, size_t, word, scope);
static c0 c0list;
static size_t ana(state f, scope *c, size_t m, word x) {
  if (homp(x) && datp(x)) switch (ptr(x)[1].x) {
    case Pair: return c0list(f, c, m, x);
    case String: return seek(f, c, m, x, *c); }
  return c0ix(f, c, m, K, x); }

static c1 c1var;
static size_t c0var1(state f, scope *c, size_t m, word x) {
  return push3(f, (word) c1var, x, (*c)->pals) ? m + 2 : 0; }

static long stkidxof(state f, scope c, word var) {
  size_t i = 0;
  // is it a closure variable?
  for (word l = c->imps; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  for (word l = c->args; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  return -1; }

// emit stack reference instruction
static thread c1var(state f, scope *c, thread k) {
  word var = *f->sp++, // variable name
       ins = getnum(*f->sp++), // stack inset
       idx = stkidxof(f, *c, var);
  return
    k[-2].ap = ref,
    k[-1].x = putnum(idx + ins),
    pull(f, c, k - 2); }

// c0 lazy reference
static size_t c0laz(state f, scope *c, size_t m, word x, scope d) {
  x = (word) cons(f, x, (word) d);
  if (!x) return 0;
  m = c0ix(f, c, m, tie, x); // deferred resolve instruction
  if (!m) return m;
  return c0args(f, c, m, B(B(A(f->sp[2])))); }

static size_t seek(state f, scope *c, size_t m, word k, scope d) {
  word x;
  if (nilp((word) d)) return 
    x = dict_lookup(f, k),
    c0ix(f, c, m, K, x ? x : k);
    
  // look in vals
  if ((x = assoc(f, d->lams, k)))
    return c0laz(f, c, m, x, d);
  // look in imps args
  x = stkidxof(f, d, k);
  if (x >= 0) {
    if (*c != d)
      k = (word) cons(f, k, (*c)->imps),
      k = k ? A((*c)->imps = k) : k;
    return k ? c0var1(f, c, m, k) : 0; }
  // recur on outer scope
  return seek(f, c, m, k, d->par); }

// emits call instruction and modifies to tail call
// if next operation is return
static thread c1ap(state f, scope *c, thread k) {
  if (k->ap == ret) k->ap = tap; // tail call
  else (--k)->ap = ap; // regular call
  return pull(f, c, k); } // ok

// cons on list unless already there
static word uinsert(state f, word l, word x) {
  return lidx(f, l, x) < 0 ? (word) cons(f, x, l) : l; }

// codegen apply function argument
static size_t c0args(state f, scope *c, size_t m, word b) {
  for (MM(f, &b), (*c)->pals += 2; m && twop(b); b = B(b))
    m = ana(f, c, m + 1, A(b)),
    m = m && push1(f, (word) c1ap) ? m : 0;
  return (*c)->pals -= 2, UM(f), m; }

// lambda decons: pushes last list item to stack, returns init of list.
static word ldecons(state f, word x) {
  if (!twop(x)) return push1(f, nil) ? nil : 0;
  if (!twop(B(x))) return push1(f, A(x)) ? nil : 0;
  word y = A(x);
  avec(f, y, x = ldecons(f, B(x)));
  return x ? (word) cons(f, y, x) : x; }

// whole innerlambda thread whateverer
// returns pair of thread and closure variable symbols
static pair c0lambi(state f, scope *c, word x) {
  if (!push2(f, x, (word) yieldk)) return 0;
  size_t m = ana(f, c, 4, pop1(f)); // include 4 for ret(n) and curry(n)
  if (!m) return 0;
  size_t arity = // including closure args
    llen((*c)->args) + llen((*c)->imps);
  thread k = // return from appropriate height
    push3(f, (word) c1ix, (word) ret, putnum(arity)) ?
      cata(f, c, m) :
      0; // allocate & emit
  if (!k) return 0;
  if (arity > 1) // curry if more than 1 argument
    (--k)->x = putnum(arity),
    (--k)->ap = curry;
  // all the code has been emitted so now trim the thread
  mo_tag(k)->head = k;
  // apply to enclosed variables (if none then quoting has no effect)
  return cons(f, (word) k, (*c)->imps); }

// lambda wrapper parses expression and manages inner scope
static word c0lambw(state f, scope *c, word imps, word exp) {
  avec(f, imps, exp = ldecons(f, exp));
  scope d = exp ? enscope(f, c, exp, imps) : 0;
  avec(f, d, exp = d ? (word) c0lambi(f, &d, pop1(f)) : 0);
  return exp; }

// COND
// conditionals

// pullback instructions to emit targeted jumps etc
static c1 c1precond, c1postcond, c1prebranch, c1postbranch;

// conditional expression analyzer
static size_t c0cond(state f, scope *c, size_t m, word x) {
  if (!push2(f, x, (word) c1postcond)) return 0;
  // FIXME probably a nicer way to write this loop ...
  for (x = pop1(f), MM(f, &x); m; x = B(B(x))) {
    if (!twop(x)) { // at end, no default branch
      m = ana(f, c, m, nil); break; }
    if (!twop(B(x))) { // at end, default branch
      m = ana(f, c, m + 2, A(x));
      m = m && push1(f, (word) c1prebranch) ? m : 0;
      break; }
    m = ana(f, c, m + 4, A(x));
    m = m && push1(f, (word) c1postbranch) ? m : 0;
    m = m ? ana(f, c, m, A(B(x))) : 0;
    m = m && push1(f, (word) c1prebranch) ? m : 0; }
  return UM(f),
    m && push1(f, (word) c1precond) ? m : 0; }

// first emitter called for cond expression
// pushes cond expression exit address onto scope stack ends
static thread c1precond(state f, scope *c, thread k) {
  pair w = cons(f, (word) k, (*c)->ends);
  return !w ? 0 : pull(f, c, (thread) A((*c)->ends = (word) w)); }

// last emitter called for cond expression
// pops cond expression exit address off scope stack ends
static thread c1postcond(state f, scope *c, thread k) {
  (*c)->ends = B((*c)->ends);
  return pull(f, c, k); }

// first emitter called for a branch
// pushes next branch address onto scope stack alts
static thread c1prebranch(state f, scope *c, thread k) {
  pair w = cons(f, (word) k, (*c)->alts);
  if (!w) return (thread) w;
  (*c)->alts = (word) w;
  k = (thread) A(w) - 2;
  thread addr = (cell) A((*c)->ends);
  // if the destination is a return or tail call,
  // then forward it instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x;
  else k[0].ap = jump, k[1].x = (word) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off scope stack alts
static thread c1postbranch(state f, scope *c, thread k) {
  k[-2].ap = cond;
  k[-1].x = A((*c)->alts);
  (*c)->alts = B((*c)->alts);
  return pull(f, c, k - 2); }

// DEFINE
// let expressions
static bool lambp(state f, word x) {
  if (!twop(x) || !strp(x = A(x))) return false;
  string s = (string) x;
  return s->len == 1 && s->text[0] == '\\'; }

static word c0lrb(state f, scope *c, word exp, word *args, word *defs) {
  // base case
  MM(f, &exp);
  // make lambda from args
  // make list of expressions
  // compile lambda definitions to jjj

  UM(f);
  return 0; }

  /*
  // XXX
  // transitively close closures in ->defns
  word var, d, e, import; MM(f, &d); MM(f, &e);
  do for (import = 0, d = *lams; twop(d); d = B(d)) // for each bound function variable
    for (e = *lams; twop(e); e = B(e)) // for each bound function variable
      if ((var = A(A(d))) != A(A(e)) && // skip self
          lidx(f, var, B(A(e))) >= 0) { // if you import this variable
        word u, vars = B(A(d)); // then also the ones that it uses
        for (MM(f, &vars); twop(vars); vars = B(vars))
          if ((u = uinsert(f, B(A(e)), A(vars))) && u != B(A(e)))
            B(A(e)) = u, import++;
        UM(f); }
  while (import); UM(f); UM(f);

  // recompile
  (*c)->vals = *lams;

  // solve XXX
  /// construct lambda expression
  string l = strof(f, "\\");
  if (!l) return UM(f), 0;
  // reverse arg list
  for (word i, h = *args, t; twop(h);
    i = B(h), B(h) = exp, exp = h, h = i);
  exp = (word) cons(f, (word) l, exp); // lambda expression
  // apply to arguments
  exp = exp ? (word) cons(f, exp, nil) : exp;
  if (exp) LL(f, exp);
  return UM(f), exp; }
  */

static word c0lr(state f, scope *c, word exp, word *args, word *defs) {
  if (!twop(exp) && !(exp = (word) cons(f, nil, nil))) return exp;
  if (!twop(B(exp))) return c0lrb(f, c, exp, args, defs); // base case
  MM(f, &exp);

  // insert nom into args
  word n = (word) cons(f, A(exp), *args);
  if (!n) return UM(f), 0; // oom
  *args = n;

  n = (word) cons(f, A(B(exp)), *defs);
  if (!n) return UM(f), 0; // oom
  *defs = n;

  // recur
  if ((n = c0lr(f, c, B(B(exp)), args, defs)) &&
      (n = (word) cons(f, A(n), n)))
    A(B(n)) = A(B(exp));

  if (n) println(f, n, stdout);
  return UM(f), n; }

static word ldels(state f, word lam, word l) {
  if (!twop(l)) return nil;
  word m = ldels(f, lam, B(l));
  if (!assoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static word c0l(state f, scope *c, word exp) {
  if (!twop(exp)) return nil;
  // lots of variables :(
  word nom = nil, def = nil, lam = nil,
       var = nil, vars = nil,
       d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &vars);

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = B(B(exp)))
    if (!(nom = (word) cons(f, A(exp), nom)) ||
        !(def = (word) cons(f, A(B(exp)), def)))
      goto fail;
    else if (lambp(f, A(def))) {
      // if it's a lambda compile it and record in lam list
      word x = c0lambw(f, c, nil, B(A(def)));
      x = x ? (word) cons(f, A(nom), x) : x;
      x = x ? (word) cons(f, x, lam) : x;
      if (x) lam = x;
      else goto fail; }

  // if there's no body use nil
  if (!twop(exp)) if (!(exp = (word) cons(f, nil, nil))) goto fail;

  // solve closures
  // for each function f with closure C(f)
  // for each function g with closure C(g)
  // if f in C(g) then C(g) include C(f)
  //
  long imports;
  do for (imports = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
    for (e = lam; twop(e); e = B(e)) // for each bound function variable
      if ((var = A(A(d))) != A(A(e)) && // skip self
          lidx(f, var, B(A(e))) >= 0) { // if you import this variable
        word u, vars = B(A(d)); // then also the ones that it uses
        for (; twop(vars); vars = B(vars)) {
          if (!(u = uinsert(f, B(A(e)), A(vars)))) { // oom
            UM(f), UM(f), UM(f); goto fail; }
          if (u != B(A(e))) // if list is updated then record change
            B(A(e)) = u, imports++; }
        UM(f); }
  while (imports);

  // now delete defined functions from the closure variable lists
  for (e = lam; twop(e); e = B(e))
    B(B(A(e))) = ldels(f, lam, B(B(A(e))));

  // store lambdas on scope for lazy binding
  (*c)->lams = lam;

  // construct new lambda application expression
  // - reverse noms onto exp
  // - reverse expressions onto e = nil and recompile lambdas
  for (e = nil; twop(nom);) {
    word _ = B(nom);
    B(nom) = exp, exp = nom, nom = _;
    // if lambda then recompile
    if (lambp(f, A(def))) {
      d = assoc(f, lam, A(exp));
      // recompile with the solved variables
      word x = c0lambw(f, c, B(B(d)), B(A(def)));
      if (!x) goto fail;
      // put in def list and lam list (latter is used for lazy binding)
      A(def) = B(d) = x; }
    // rotate onto e
    _ = B(def), B(def) = e, e = def, def = _; }

  if (!twop(e)) exp = A(exp);
  else {
    // - put lambda symbol
    string l = strof(f, "\\");
    if (!l || !(exp = (word) cons(f, (word) l, exp))) goto fail;
    // - cons them together
    exp = (word) cons(f, exp, e); }
  
done: return UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), exp;
fail: exp = 0; goto done; }

static size_t c0let(state f, scope *c, size_t m, word x) {
  scope d;
  avec(f, x, d = enscope(f, c, (*c)->args, (*c)->imps));
  if (!d) return 0;
  word args = nil, defs = nil;
  MM(f, &d);
  x = c0l(f, &d, x);
  UM(f);
  return x ? ana(f, c, m, x) : x; }

static Vm(tie) {
  word ref = ip[1].x, var = A(A(ref));
  scope env = (scope) B(ref);
  var = A(lookup(f, env->lams, var));
  return
    ip[0].ap = K,
    ip[1].x = var,
    K(f, ip, hp, sp); }

static size_t c0list(state f, scope *c, size_t m, word x) {
  word a = A(x), b = B(x);
  if (!twop(b)) // singleton list is quote
    return c0ix(f, c, m, K, a);
  if (strp(a)) { // special forms
    string s = (string) a;
    if (s->len == 1) switch (s->text[0]) {
      case ':': return c0let(f, c, m, b);
      case '\\': return
        x = c0lambw(f, c, nil, b),
        x ? ana(f, c, m, x) : x;
      case '?': return c0cond(f, c, m, b); } }
  return
    avec(f, b, m = ana(f, c, m, a)), // evaluate function expression
    c0args(f, c, m, b); } // apply to arguments
