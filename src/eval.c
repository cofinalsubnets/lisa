#include "i.h"

static thread analyze(state, word);

// compile and execute expression
NoInline status eval(state f, word x) {
  thread k = analyze(f, x);
  return k ? k->ap(f, k, f->hp, f->sp) : Oom; }

typedef struct scope { // track lexical scope
  word ib, // internal bindings // closure variables // prefix of sb
       sb, // stack bindings // all variables on stack
       sn, // stack inset // number of arguments on stack
       defns,
       s1, s2; // stacks used for branch addresses
  struct scope *par;
} *scope;

static scope enscope(state f, scope *par, word sb) {
  scope s = (scope) mo_n(f, Width(struct scope));
  if (s) s->sb = sb,
         s->s1 = s->s2 = s->ib = s->sn = s->defns = nil,
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

// basic functions
static C1(yieldk) { return k; }
static C1(pull) { return ((c1*) (*f->sp++))(f, c, k); }
static thread cata(state f, scope *c, size_t m) {
  thread k = mo_n(f, m);
  return pull(f, c, !k ? k : m + (thread) memset(k, -1, m * sizeof(word))); }
static C1(c1i) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static C1(c1ix) { return
  k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }
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
  scope c = push2(f, x, (word) yieldk) ? enscope(f, NULL, nil) : NULL;
  if (c) avec(f, c,
    m = ana(f, &c, 1, pop1(f)),
    m = m ? c0i(f, &c, m, yield) : m,
    k = m ? cata(f, &c, m) : k);
  return k; }

static c0 c0var, c0list;
static size_t ana(state f, scope *c, size_t m, word x) {
  if (homp(x) && datp(x)) switch (ptr(x)[1].x) {
    case Pair: return c0list(f, c, m, x);
    case String: return c0var(f, c, m, x); }
  return c0ix(f, c, m, K, x); }

static c1 c1var;
static size_t c0var1(state f, scope *c, size_t m, word x) {
  return push3(f, (word) c1var, x, (*c)->sn) ? m + 2 : 0; }

static thread c1var(state f, scope *c, thread k) {
  word sym = *f->sp++,
       idx = getnum(*f->sp++) + lidx(f, (*c)->sb, sym);
  return
    k[-2].ap = ref,
    k[-1].x = putnum(idx),
    pull(f, c, k - 2); }

static size_t c0var(state f, scope *c, size_t m, word x) {
  // is x bound locally?
  if (lidx(f, (*c)->sb, x) >= 0)
    return c0var1(f, c, m, x); // emit c1 to resolve
  // bound in defns?
  word y = lookup(f, (*c)->defns, x);
  if (y) return ana(f, c, m, y);
  // bound in an outer scope?
  for (scope d = (*c)->par; homp((word) d); d = d->par)
    if (lidx(f, d->sb, x) >= 0) {
      x = (word) cons(f, x, (*c)->sb); // cons onto sb -- creates local binding
      if (x) (*c)->sb = x, // save sb
             x = (word) cons(f, A(x), (*c)->ib); // cons onto ib -- export to enclosing scope
      if (x) (*c)->ib = x, // save ib
             x = c0var1(f, c, m, A(x));
      return x; }
  // bound globally? else self-quoting
  y = dict_lookup(f, x);
  return c0ix(f, c, m, K, y ? y : x); }

static thread c1ap(state f, scope *c, thread k) {
  if (k->ap == ret) k->ap = tap; // tail call
  else (--k)->ap = ap; // regular call
  return pull(f, c, k); } // ok

static c0 c0cond, c0let, c0lamb;
static size_t c0list(state f, scope *c, size_t m, word x) {
  word a = A(x), b = B(x);
  if (!twop(b)) return c0ix(f, c, m, K, a); // singleton list is quote
  if (strp(a)) {
    string s = (string) a;
    if (s->len == 1) switch (s->text[0]) {
      case ':': return c0let(f, c, m, b);
      case '\\': return c0lamb(f, c, m, b);
      case '?': return c0cond(f, c, m, b); } }
  // apply function to arguments
  MM(f, &b);
  m = ana(f, c, m, a);
  (*c)->sn += 2;
  for (; m && twop(b); b = B(b))
    m = ana(f, c, m + 1, A(b)),
    m = m && push1(f, (word) c1ap) ? m : 0;
  (*c)->sn -= 2;
  UM(f);
  return m; }

// lambda decons: pushes last list item to stack, returns init of list.
static word ldecons(state f, word x) {
  if (!twop(x)) return push1(f, nil) ? nil : 0;
  if (!twop(B(x))) return push1(f, A(x)) ? nil : 0;
  word y = A(x);
  avec(f, y, x = ldecons(f, B(x)));
  return x ? (word) cons(f, y, x) : x; }

static size_t c0lamb(state f, scope *c, size_t m, word x) {
  word args = ldecons(f, x);
  scope d = args ? enscope(f, c, args) : 0;
  if (!d) return 0;
  MM(f, &d);
  bool ok = push2(f, pop1(f), (word) yieldk);
  // inner thread length
  size_t n = ok ? ana(f, &d, 4, pop1(f)) : 0; // initial value 4 includes for return and curry instructions
  if (n) {
    size_t sbn = llen(d->sb); // arity including closure args
    // return from appropriate height
    ok = push3(f, (word) c1ix, (word) ret, putnum(sbn));
    thread k = ok ? cata(f, &d, n) : 0; // allocate & emit
    if (!k) x = 0;
    else { // finalize
      if (sbn > 1) // curry if more than 1 argument
        (--k)->x = putnum(sbn), (--k)->ap = curry;
      mo_tag(k)->head = k; // trim thread
      // apply to enclosed variables (if none then quoting has no effect)
      x = (word) cons(f, (word) k, d->ib); } }
  return UM(f),
    x ? ana(f, c, m, x) : x; }

// COND
// conditionals

// pullback instructions to emit targeted jumps etc
static c1 c1precond, c1postcond, c1prebranch, c1postbranch;

// conditional expression analyzer
static size_t c0cond(state f, scope *c, size_t m, word x) {
  if (!push2(f, x, (word) c1postcond)) return 0;
  for (x = pop1(f), MM(f, &x); m; x = B(B(x))) {
    if (!twop(x)) { // at end, no default branch
      x = (word) cons(f, nil, nil); // nil default branch
      if (!x) { m = 0; break; } } // OOM case
    if (!twop(B(x))) { // at end, default branch
      m = ana(f, c, m, A(x)); break; }
    m = ana(f, c, m + 4, A(x));
    m = m && push1(f, (word) c1postbranch) ? m : 0;
    m = m ? ana(f, c, m, A(B(x))) : 0;
    m = m && push1(f, (word) c1prebranch) ? m : 0; }
  return UM(f),
    m && push1(f, (word) c1precond) ? m : 0; }

// first emitter called for cond expression
// pushes cond expression exit address onto scope stack s2
static thread c1precond(state f, scope *c, thread k) {
  pair w = cons(f, (word) k, (*c)->s2);
  return !w ? 0 : pull(f, c, (thread) A((*c)->s2 = (word) w)); }

// last emitter called for cond expression
// pops cond expression exit address off scope stack s2
static thread c1postcond(state f, scope *c, thread k) {
  (*c)->s2 = B((*c)->s2);
  return pull(f, c, k); }

// first emitter called for a branch
// pushes next branch address onto scope stack s1
static thread c1prebranch(state f, scope *c, thread k) {
  pair w = cons(f, (word) k, (*c)->s1);
  if (!w) return (thread) w;
  (*c)->s1 = (word) w;
  k = (thread) A(w) - 2;
  thread addr = (cell) A((*c)->s2);
  // if the destination is a return or tail call,
  // then forward it instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x;
  else k[0].ap = jump, k[1].x = (word) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off scope stack s1
static thread c1postbranch(state f, scope *c, thread k) {
  k[-2].ap = cond;
  k[-1].x = A((*c)->s1);
  (*c)->s1 = B((*c)->s1);
  return pull(f, c, k - 2); }

// DEFINE
// let expressions
static bool lambp(state f, word x) {
  if (!twop(x) || !strp(x = A(x))) return false;
  string s = (string) x;
  return s->len == 1 && s->text[0] == '\\'; }

static size_t c0let(state f, scope *c, size_t m, word x) {
  word defns = nil;
  MM(f, &defns);
  // ev all defns
  while (twop(x) && twop(B(x))) {
    word nom = A(x), exp = A(B(x));
    // desugar here
    if (!lambp(f, exp)) continue;
    // for function definitions, compile and put in ->defns
  }
  for (;;) {
    // transitively close closures in ->defns
  }
  UM(f);
  (*c)->defns = defns;
  // recompile // requires to consult ->defns from c0var
  // construct ap expression and recur
}
