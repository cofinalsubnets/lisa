#include "i.h"

static thread compile(state, word);

// takes an expression compiles and executes it
NoInline status eval(state f, word x) {
  thread k = compile(f, x);
  return k ? k->ap(f, k, f->hp, f->sp) : Oom; }

// tracks lexical scope
typedef struct scope {
  word ib, // internal bindings // closure variables // prefix of sb
       sb, // stack bindings // all variables on stack
       sn, // stack inset // number of arguments on stack
       defns,
       s1, s2; // stacks used for branch addresses
  struct scope *par;
} *scope;

static scope enscope(state f, struct scope **par, word sb) {
  scope sc = (scope) mo_n(f, Width(struct scope));
  if (sc) sc->sb = sb,
          sc->s1 = sc->s2 = sc->ib = sc->sn = sc->defns = nil,
          sc->par = par ? *par : (scope) nil;
  return sc; }

// compiler operates in two phases
// - analyze expression; put continuation on stack; compute code size bound
typedef size_t c0(state, scope*, size_t, word);
// - allocate thread; pass to continuation; emit code and trim extra length
typedef thread c1(state, scope*, thread);

static size_t c0list(state, scope*, size_t, pair), c0i(state, scope*, size_t, vm*);
static thread cata(state, scope*, size_t);
static c0 ana, c0var;
static c1 yieldk, pull, c1var;

static thread compile(state f, word x) {
  size_t m;
  thread k = 0;
  scope c = push2(f, x, (word) yieldk) ? enscope(f, NULL, nil) : NULL;
  if (c) avec(f, c,
    m = ana(f, &c, 1, pop1(f)),
    m = m ? c0i(f, &c, m, yield) : m,
    k = m ? cata(f, &c, m) : k);
  return k; }

static thread yieldk(state f, scope *c, thread k) { return k; }
static thread pull(state f, scope *c, thread k) {
  return ((c1*) (*f->sp++))(f, c, k); }

static thread c1i(state f, scope *c, thread k) {
  k[-1].x = *f->sp++;
  return pull(f, c, k - 1); }

static thread c1ix(state f, scope *c, thread k) {
  k[-2].x = *f->sp++;
  k[-1].x = *f->sp++;
  return pull(f, c, k - 2); }

static size_t c0i(state f, scope *c, size_t m, vm *i) {
  return push2(f, (word) c1i, (word) i) ? m + 1 : 0; }
static size_t c0ix(state f, scope *c, size_t m, vm *i, word x) {
  return push3(f, (word) c1ix, (word) i, x) ? m + 2 : 0; }

static thread cata(state f, scope *c, size_t m) {
  thread k = mo_n(f, m);
  if (k) memset(k, -1, m * sizeof(word)),
         k += m;
  return pull(f, c, k); }

static size_t ana(state f, scope *c, size_t m, word x) {
  if (homp(x) && datp(x)) switch (ptr(x)[1].x) {
    case Pair: return c0list(f, c, m, (pair) x);
    case String: return c0var(f, c, m, x); }
  return c0ix(f, c, m, K, x); }

static size_t c0var1(state f, scope *c, size_t m, word x) {
  return push3(f, (word) c1var, x, (*c)->sn) ? m + 2 : 0; }

static thread c1var(state f, scope *c, thread k) {
  word sym = *f->sp++,
       idx = getnum(*f->sp++) + lidx(f, (*c)->sb, sym);
  k[-2].ap = ref;
  k[-1].x = putnum(idx);
  return pull(f, c, k - 2); }

static size_t c0var(state f, scope *c, size_t m, word x) {
  // is x bound locally?
  if (lidx(f, (*c)->sb, x) >= 0)
    return c0var1(f, c, m, x); // emit c1 to resolve
  // bound in defns?
  //
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
  word y = dict_lookup(f, x);
  return c0ix(f, c, m, K, y ? y : x); }

static thread c1postbranch(state f, scope *c, thread k) {
  k[-2].ap = cond;
  k[-1].x = A((*c)->s1);
  (*c)->s1 = B((*c)->s1);
  return pull(f, c, k - 2); }

static thread c1precond(state f, scope *c, thread k) {
  pair w = cons(f, (word) k, (*c)->s2);
  if (!w) return (thread) w;
  (*c)->s2 = (word) w;
  return pull(f, c, (thread) A(w)); }

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

static thread c1postcond(state f, scope *c, thread k) {
  (*c)->s2 = B((*c)->s2);
  return pull(f, c, k); }

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
  return UM(f), m && push1(f, (word) c1precond) ? m : 0; }


// lambda decons: pushes last list item to stack, returns init of list.
static word ldecons(state f, word x) {
  if (!twop(x)) return push1(f, nil) ? nil : 0;
  if (!twop(B(x))) return push1(f, A(x)) ? nil : 0;
  word y = A(x);
  avec(f, y, x = ldecons(f, B(x)));
  return x ? (word) cons(f, y, x) : x; }

static size_t c0lambda(state f, scope *c, size_t m, word x) {
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
  UM(f);
  return x ? ana(f, c, m, x) : x; }

static thread c1ap(state f, scope *c, thread k) {
  if (k->ap == ret) k->ap = tap;
  else (--k)->ap = ap;
  return pull(f, c, k); }

static bool lambp(state f, word x) {
  if (!twop(x) || !strp(x = A(x))) return false;
  string s = (string) x;
  return s->len == 1 && s->text[0] == '\\'; }

static size_t c0define(state f, scope *c, size_t m, word x) {
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

static size_t c0list(state f, scope *c, size_t m, pair w) {
  word a = w->a, b = w->b;
  if (!twop(b)) return c0ix(f, c, m, K, a); // singleton list is quote
  if (strp(a)) {
    string s = (string) a;
    if (s->len == 1) switch (s->text[0]) {
      case ':': return c0define(f, c, m, b);
      case '\\': return c0lambda(f, c, m, b);
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
