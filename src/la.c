#include "la.h"
#include <string.h>
#include <time.h>
#include <errno.h>
#include <stdarg.h>

// initialization helpers
static bool
  defprims(la),
  inst(la, const char*, vm*) NoInline;
static sym
  symofs(la, const char*) NoInline;

ob la_ev(la v, ob _) {
  static struct mo go[] = { {call}, {(vm*) putnum(1)}, {yield} };
  return !pushs(v, _, NULL) ? 0 :
    call(v, (ob) prims, go, v->hp, v->sp, v->fp); }

static NoInline bool la_ev_f(la v, FILE *in) {
  bool ok = true;
  for (ob x; ok && !feof(in);
    x = la_rx(v, in),
    ok = x ? la_ev(v, x) : feof(in));
  return ok; }

NoInline bool la_script(la v, const char *path) {
  FILE *in = fopen(path, "r");
  if (!in) return
    errp(v, "%s : %s", path, strerror(errno)),
    false;
  bool ok = la_ev_f(v, in);
  if (!ok) errp(v, "%s : %s", path, "error");
  return fclose(in), ok; }

la la_ini(void) {
  la v = malloc(sizeof(struct la));
  if (v && !la_open(v)) return la_fin(v), NULL;
  return v; }

void la_fin(la v) { la_close(v), free(v); }

void la_close(la v) {
  if (v) free(v->pool), v->pool = NULL; }

NoInline bool la_open(la v) {
  v->rand = v->t0 = clock();
  v->len = 1 << 10;
  v->pool = NULL;
  v->safe = NULL;

  // the heap is all used up to start, so the first allocation initializes the pool
  v->hp = v->sp = v->pool + v->len;
  v->fp = (sf) v->sp;
  v->topl = (tbl) nil;
  v->ip = (mo) nil;
  v->xp = nil;
  v->syms = 0;
  setw(v->lex, nil, LexN);

  ob _;
  bool ok =
    // global symbols // FIXME stop using these if possible
    (v->lex[Eval] = (ob) symofs(v, "ev")) &&
    (v->lex[Def] = (ob) symofs(v, ":")) &&
    (v->lex[Cond] = (ob) symofs(v, "?")) &&
    (v->lex[Lamb] = (ob) symofs(v, "\\")) &&
    (v->lex[Quote] = (ob) symofs(v, "`")) &&
    (v->lex[Seq] = (ob) symofs(v, ",")) &&
    (v->lex[Splat] = (ob) symofs(v, ".")) &&

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

static str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(str) + b2w(bs));
  if (o) memcpy(o->text, c, bs),
         ini_str(o, bs);
  return o; }

static NoInline sym symofs(la v, const char *s) {
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
static NoInline bool inst(la v, const char *a, vm *b) {
  sym z = symofs(v, a);
  return z && tblset(v, v->topl, (ob) z, putnum(b)); }

static NoInline str str_c_cat_r(la v, size_t l, va_list xs) {
  char *cs = va_arg(xs, char*);
  if (!cs) {
    str s = cells(v, Width(str) + b2w(l) + 1);
    if (s) ini_str(s, l), s->text[l] = 0;
    return s ; }
  size_t i = strlen(cs);
  str s = str_c_cat_r(v, l+i, xs);
  if (s) memcpy(s->text + l, cs, i);
  return s; }

static str str_c_cat(la v, ...) {
  va_list xs;
  va_start(xs, v);
  str s = str_c_cat_r(v, 0, xs);
  va_end(xs);
  return s; }

static FILE *seek_lib(la v, const char *nom) {
  str s;
  FILE *i;
  char *home = getenv("HOME");
  if (home) {
    s = str_c_cat(v, home, "/.local/lib/lisa/", nom, ".la", NULL);
    if (s && (i = fopen(s->text, "r"))) return i; }
  s = str_c_cat(v, "/usr/local/lib/lisa/", nom, ".la", NULL);
  if (s && (i = fopen(s->text, "r"))) return i;
  s = str_c_cat(v, "/usr/lib/lisa/", nom, ".la", NULL);
  if (s && (i = fopen(s->text, "r"))) return i;
  s = str_c_cat(v, "/lib/lisa/", nom, ".la", NULL);
  if (s && (i = fopen(s->text, "r"))) return i;
  return NULL; }

bool la_lib(la v, const char *nom) {
  FILE *p = seek_lib(v, nom);
  if (!p) return false;
  bool ok = la_ev_f(v, p);
  if (!ok) errp(v, "%s : %s", nom, "error loading lib");
  return fclose(p), ok; }
