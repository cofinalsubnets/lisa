#include "la.h"
#include <string.h>
#include <time.h>
#include <errno.h>
#include <stdarg.h>

void la_reset(la v) {
  v->sp = v->pool + v->len;
  v->fp = (sf) v->sp;
  v->ip = 0;
  v->xp = 0; }

// initialization helpers
static bool
  defprims(la),
  inst(la, const char*, vm*);
static sym symofs(la, const char*);

void la_close(la v) {
  if (v) free(v->pool), v->pool = NULL; }

bool la_open(la v) {
  const size_t len = 1 << 10; // must be a power of 2

  ob *pool = calloc(len, sizeof(ob));
  if (!pool) return false;

  memset(v, 0, sizeof(struct la_carrier));

  v->len = len;
  v->hp = v->pool = pool;
  v->hp = pool;
  v->sp = pool + len;
  v->fp = (sf) v->sp;

  v->rand = v->run.t0 = clock();

  ob _;
  bool ok =
    // global symbols // FIXME stop using these if possible
    (v->lex[Eval] = symofs(v, "ev")) &&
    (v->lex[Def] = symofs(v, ":")) &&
    (v->lex[Cond] = symofs(v, "?")) &&
    (v->lex[Lamb] = symofs(v, "\\")) &&
    (v->lex[Quote] = symofs(v, "`")) &&
    (v->lex[Seq] = symofs(v, ",")) &&
    (v->lex[Splat] = symofs(v, ".")) &&

    // make the global namespace
    (v->topl = mktbl(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tblset(v, v->topl, _, (ob) v->topl)
    // register instruction addresses at toplevel so the
    // compiler can use them.
#define reg_intl(a) && inst(v, "i-"#a, a)
    i_internals(reg_intl)
    && defprims(v);

  if (!ok) la_close(v);
  return ok; }

static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

// static table of primitive functions
#define prim_ent(go, nom) { go, nom },
const struct prim prims[] = { i_primitives(prim_ent) };
#define LEN(ary) (sizeof(ary)/sizeof(*ary))
bool primp(mo x) {
  struct prim *_ = (struct prim*) x;
  return _ >= prims && _ < prims + LEN(prims); }

static bool defprims(la v) {
  const struct prim *p = prims,
                    *lim = p + LEN(prims);
  while (p < lim) {
    sym z = symofs(v, p->nom);
    if (!z || !tblset(v, v->topl, (ob) z, (ob) p++)) return false; }
  return true; }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static bool inst(la v, const char *a, vm *b) {
  sym z = symofs(v, a);
  return z && tblset(v, v->topl, (ob) z, putnum(b)); }

static NoInline str str0catr(la v, size_t l, va_list xs) {
  char *cs = va_arg(xs, char*);
  if (!cs) {
    str s = cells(v, Width(str) + b2w(l+1));
    if (s) ini_str(s, l+1), s->text[l] = 0;
    return s ; }
  size_t i = strlen(cs);
  str s = str0catr(v, l+i, xs);
  if (s) memcpy(s->text + l, cs, i);
  return s; }

static str str0cat(la v, ...) {
  va_list xs;
  va_start(xs, v);
  str s = str0catr(v, 0, xs);
  va_end(xs);
  return s; }

#include <sys/stat.h>
// the str returned is null-terminated.
// FIXME distunguish OOM from file not found
static str seek_lib_path(la v, const char *nom) {
  str s;
  char *home = getenv("HOME");
  struct stat _;
  if (home) {
    s = str0cat(v, home, "/.local/lib/lisa/", nom, ".la", NULL);
    if (s && 0 == stat(s->text, &_)) return s; }
  s = str0cat(v, "/lib/lisa/", nom, ".la", NULL);
  if (s && 0 == stat(s->text, &_)) return s;
  s = str0cat(v, "/usr/lib/lisa/", nom, ".la", NULL);
  if (s && 0 == stat(s->text, &_)) return s;
  s = str0cat(v, "/usr/local/lib/lisa/", nom, ".la", NULL);
  if (s && 0 == stat(s->text, &_)) return s;
  return 0; }

bool la_lib(la v, const char *nom) {
  str path = seek_lib_path(v, nom);
  if (!path) return
    errp(v, "module not found : %s", nom),
    false;
  FILE *in = fopen(path->text, "r");
  if (!in) return
    errp(v, "%s : %s", path->text, strerror(errno)),
    false;
  bool ok = true;
  for (ob x; ok && !feof(in);
    x = la_rx(v, in),
    ok = x ? la_ev(v, x) : feof(in));
  if (!ok) errp(v, "%s : %s", "error loading module", nom);
  return fclose(in), ok; }
