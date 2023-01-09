#include "i.h"

static str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  return o ? (memcpy(o->text, c, bs), str_ini(o, bs)) : o; }

static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

static bool li_ini_env(la);
static enum status vm_exit(la v, enum status r) { return r; }

NoInline enum status li_ini(struct V *v) {
  memset(v, 0, sizeof(struct V));
  const size_t len = 1 << 10; // power of 2
                         //
  ob *pool = fresh_pool(len);
  if (pool) {
    v->len = len,
    v->hp = v->pool = pool,
    v->fp = (sf) (v->sp = pool + len),
    v->rand = v->t0 = clock();
    if (li_ini_env(v)) return Ok;
    li_fin(v); }

  return OomError; }

void li_fin(struct V *v) {
  if (v) free(v->pool), v->pool = NULL; }

static bool
  defprim(la, vm*, const char*) NoInline,
  inst(la, const char*, vm*);

bool neql(la v, ob x, ob y) { return false; }
bool eql(la v, ob a, ob b) { return a == b ||
  (!nump(a|b) && G(a) == act &&
   ((typ) GF(a))->equi(v, a, b)); }
static bool li_ini_stack(li v) {
  if (Avail < Width(struct frame) &&
      !please(v, Width(struct frame)))
    return false;
  ob *top = v->pool + v->len;
  v->fp = (frame) top - 1;
  v->sp = (ob*) v->fp;
  v->fp->subd = v->fp;
  v->fp->argc = 0;
  v->fp->retp = 0;
  v->fp->clos = 0;
  return true; }
#include "vm.h"
#define reg_prim(go, nom) && defprim(v, go, nom)
#define reg_inst(a) && inst(v, "i-"#a, a)
static bool li_ini_vm(li v) {
  ob _; return
    (v->topl = mktbl(v)) VM1(reg_inst) &&
    (v->macros = mktbl(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tbl_set(v, v->topl, _, (ob) v->topl) &&
    (_ = (ob) symofs(v, "macros")) &&
    tbl_set(v, v->topl, _, (ob) v->macros)
    VM2(reg_prim) &&
    li_ini_stack(v); }


static bool li_ini_env(struct V* v) {
  struct sym *y; return
    (y = symofs(v, "ev"), v->lex.eval = y) &&
    (y = symofs(v, ":"), v->lex.define = y) &&
    (y = symofs(v, "?"), v->lex.cond = y) &&
    (y = symofs(v, "\\"), v->lex.lambda = y) &&
    (y = symofs(v, "`"), v->lex.quote = y) &&
    (y = symofs(v, ","), v->lex.begin = y) &&
    (y = symofs(v, "."), v->lex.splat = y) &&
    li_ini_vm(v); }

static NoInline bool
defprim(struct V *v, vm *i, const char *n) {
  mo k; sym y; return
    (y = symofs(v, n)) &&
    (k = thd(v, i, y, NULL)) &&
    tbl_set(v, v->topl, (ob) GF(k), (ob) k); }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline bool inst(la v, const char *a, vm *b) {
  sym z; return
    (z  = symofs(v, a)) &&
    tbl_set(v, v->topl, (ob) z, (ob) b); }

static NoInline void errp(la, const char*, ...);

void report(la v, enum status s) {
  switch (s) {
    // not error codes, so print nothing.
    case Ok: case Eof: return;
    case DomainError: return errp(v, "has no value");
    case OomError: return errp(v, "oom at %d words", v->len);
    case SyntaxError: return errp(v, "syntax error");
    case ArityError: return
      errp(v, "wrong arity : %d of %d",
        v->fp->argc, getnum(v->xp));
    case SystemError:
      return errp(v, "system error : %s", strerror(errno));
    case NameError: {
      const char *n = "#sym";
      U l = 4;
      str s = ((sym) v->xp)->nom;
      if (s) n = s->text, l = s->len;
      return errp(v, "free variable : %.*s", l, n); } } }


static NoInline void errp_call(la v, mo ip, frame fp) {
  putc('(', stderr);
  transmit(v, stderr, (ob) ip);
  for (size_t i = 0, argc = fp->argc; i < argc;
    putc(' ', stderr),
    transmit(v, stderr, fp->argv[i++]));
  putc(')', stderr); }

// this prints a backtrace.
// TODO maybe show it upside down like python?
#define aubas (fp == fp->subd)
static NoInline void errp(la v, const char *msg, ...) {
  mo ip = v->ip;
  sf fp = v->fp;

  // print error
  fputs(";; ", stderr);

  // show the function if there is one
  if (!aubas)
    errp_call(v, ip, fp),
    putc(' ', stderr),
    ip = fp->retp,
    fp = fp->subd;

  // show message
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  putc('\n', stderr);

  // show backtrace
  while (!aubas)
    fputs(";; in ", stderr),
    errp_call(v, ip, fp),
    putc('\n', stderr),
    ip = (mo) fp->retp,
    fp = fp->subd; }


static size_t copy(la, size_t);
static void copy_to(la, size_t, ob*);
ob *fresh_pool(size_t n) { return malloc(n * sizeof(ob)); }

// FIXME the garbage collector works pretty well but it could be better:
//
// - it uses stack recursion so a process that constructs infinite
//   data will stack overflow, rather than fail gracefully with oom.
//   we could fix this with cheney's algorithm.
//
// - we allocate a new pool every cycle rather than keeping two pools
//   at all times. theoretically this means we have less memory allocated
//   most of the time, and if malloc is efficient then the overhead from
//   calling it every cycle should be negligible, but it would still be
//   better only to call out when we need to grow or shrink the pool.

#define MinVim 32
#define MaxVim 128
////
/// garbage collector
//
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// governs the size of the memory pool.
NoInline bool please(struct V *v, size_t req) {
  // copy into a new pool of the same size.
  size_t have = v->len,
         vim = copy(v, have);
  if (!vim) return 0;

  size_t want = have,
         need = have - (Avail - req);

  // grow if we're too slow or small
  if (vim < MinVim || need > want)
    do want <<= 1, vim <<= 1;
    while (vim < MinVim || need > want || vim < MinVim);

  // shrink if we're too big and fast
  else if (vim > MaxVim && need < want >> 1)
    do want >>= 1, vim >>= 1;
    while (vim > MaxVim && need < want >> 1);

  return want == have || copy(v, want) || need <= have; }

// copy : la_clock_t la size_t
// relocate all reachable data into a newly allocated
// memory pool of the given length. return 0 if a new
// pool can't be allocated or else a positive integer
// value u that's higher the less time we spend in GC:
//
//   u = t1 == t2 ? 1 : (t2 - t0) / (t2 - t1)
//
// where
//
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)

static size_t copy(struct V *v, size_t len) {
  U t1 = clock(), t0 = v->t0, t2;
  ob *pool0 = v->pool, *pool1 = fresh_pool(len);
  if (!pool1) return 0;
  copy_to(v, len, pool1),
  free(pool0),
  t2 = v->t0 = clock(),
  t1 = t2 - t1;
  return t1 ? (t2 - t0) / t1 : MaxVim; }

static void copy_to(struct V *v, size_t len1, ob *pool1) {
  ob len0 = v->len,
     *sp0 = v->sp,
     *pool0 = v->pool,
     *top0 = pool0 + len0,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->syms = 0;
  v->len = len1;
  v->hp = v->pool = pool1;
  v->sp = sp0 + shift;
  v->fp = (sf) ((ob*) v->fp + shift);

  v->xp = cp(v, v->xp, pool0, top0);
  v->ip = (mo) cp(v, (ob) v->ip, pool0, top0);

  // copy globals
  v->topl = (tbl) cp(v, (ob) v->topl, pool0, top0);
  v->macros = (tbl) cp(v, (ob) v->macros, pool0, top0);
  for (U i = 0; i < Width(struct glob); i++)
    ((ob*)&v->lex)[i] = cp(v, ((ob*)&v->lex)[i], pool0, top0);
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);

  // copy the stack
  ob *sp = v->sp;
  sf fp = v->fp;
  for (;;) {
    while (sp < (ob*) fp) *sp++ = cp(v, *sp0++, pool0, top0);
    if (sp0 == top0) break;
    sf fp0 = (sf) sp0;
    fp->argc = fp0->argc;
    fp->subd = (sf) ((ob*) fp0->subd + shift);
    fp->clos = (ob*) cp(v, (ob) fp0->clos, pool0, top0);
    fp->retp = (mo) cp(v, (ob) fp0->retp, pool0, top0);
    sp = fp->argv;
    sp0 = fp0->argv;
    fp = fp->subd; } }

static ob cp_mo(la v, mo src, ob *pool0, ob *top0) {
  struct tag *fin = mo_tag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;

  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  for (GF(d) = (vm*) dst; d-- > dst;
    G(d) = (vm*) cp(v, (ob) G(d), pool0, top0));
  return (ob) (src - ini + dst); }

#define stale(o) ((ob*)(o) >= pool0 && (ob*) o < top0)
NoInline ob cp(la v, ob x, ob *pool0, ob *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  ob y = (ob) G(x);
  if (!nump(y) && livep(v, y)) return y;
  if ((vm*) y == act) return
    ((typ) GF(x))->evac(v, x, pool0, top0);
  return cp_mo(v, (mo) x, pool0, top0); }

// push things onto the stack
static NoInline bool pushsr(la v, size_t i, va_list xs) {
  bool _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i+1, xs)),
     _ && (*--v->sp = x, true)); }

bool pushs(la v, ...) {
  bool _; va_list xs; return
  va_start(xs, v),
  _ = pushsr(v, 0, xs),
  va_end(xs),
  _; }

void *cells(la v, size_t n) { return
  Avail >= n || please(v, n) ? bump(v, n) : 0; }
