#include "i.h"

static str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  return o ? (memcpy(o->text, c, bs), str_ini(o, bs)) : o; }

static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

static bool la_ini_(la);
static enum status vm_exit(la v, enum status r) { return r; }

NoInline enum status la_ini(struct V *v) {
  memset(v, 0, sizeof(struct V));
  const size_t len = 1 << 10; // power of 2
                         //
  ob *pool = fresh_pool(len);
  if (pool) {
    v->len = len,
    v->hp = v->pool = pool,
    v->fp = (sf) (v->sp = pool + len),
    v->rand = v->t0 = clock(),
    v->exit = vm_exit;
    if (la_ini_(v)) return Ok;
    la_fin(v); }

  return OomError; }

void la_fin(struct V *v) {
  if (v) free(v->pool), v->pool = NULL; }

static bool
  defprim(la, vm*, const char*) NoInline,
  inst(la, const char*, vm*);

bool neql(la v, ob x, ob y) { return false; }
bool eql(la v, ob a, ob b) { return a == b ||
  (!nump(a|b) && G(a) == act &&
   ((typ) GF(a))->equi(v, a, b)); }
#include "vm.h"
#define reg_prim(go, nom) && defprim(v, go, nom)
#define reg_inst(a) && inst(v, "i-"#a, a)
static bool la_ini_vm(struct V *v) {
  ob _; return
    (v->topl = mktbl(v)) VM1(reg_inst) &&
    (v->macros = mktbl(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tbl_set(v, v->topl, _, (ob) v->topl) &&
    (_ = (ob) symofs(v, "macros")) &&
    tbl_set(v, v->topl, _, (ob) v->macros)
    VM2(reg_prim); }


static bool la_ini_(struct V* v) {
  struct sym *y; return
    (y = symofs(v, "ev"), v->lex.eval = y) &&
    (y = symofs(v, ":"), v->lex.define = y) &&
    (y = symofs(v, "?"), v->lex.cond = y) &&
    (y = symofs(v, "\\"), v->lex.lambda = y) &&
    (y = symofs(v, "`"), v->lex.quote = y) &&
    (y = symofs(v, ","), v->lex.begin = y) &&
    (y = symofs(v, "."), v->lex.splat = y) &&
    la_ini_vm(v); }

static NoInline bool
defprim(struct V *v, vm *i, const char *n) {
  mo k; sym y; return
    (y = symofs(v, n)) &&
    (with(y, k = mo_n(v, 2)), k) &&
    (k[0].ap = i,
     k[1].ap = (vm*) y,
     tbl_set(v, v->topl, (ob) y, (ob) k)); }

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
#define aubas (((ob*) fp) == v->pool + v->len)
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

