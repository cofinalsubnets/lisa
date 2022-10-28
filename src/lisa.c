#include "lisa.h"
#include "vm.h"
#include <time.h>
#include <stdlib.h>

// static table of primitive functions
#define prim_ent(go, nom) { go, nom },
struct prim primitives[] = { i_primitives(prim_ent) };

#define LEN(ary) (sizeof(ary)/sizeof(*ary))
bool primp(ob x) {
  struct prim *_ = (struct prim*) x;
  return _ >= primitives && _ < primitives + LEN(primitives); }

static bool define_primitives(la v) {
  struct prim *p = primitives,
              *lim = p + LEN(primitives);
  for (;p < lim; p++) {
    ob z = interns(v, p->nom);
    if (!z || !tbl_set(v, v->topl, z, (ob) p)) return false; }
  return true; }
// initialization helpers
//
// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline ob inst(la v, const char *a, vm *b) {
  ob z = interns(v, a);
  return z ? tbl_set(v, v->topl, z, putnum(b)) : 0; }

// initialize a process
static bool la_ini(la v) {
  // set time & random seed
  v->rand = v->t0 = clock();

  // configure memory
  // how big a memory pool to start with?
  v->len = 1 << 10;
  // there is no pool yet
  v->pool = NULL;
  // no protected values
  v->keep = NULL;
  // the data stack starts at the top of memory
  // the call stack lives on the data stack
  // the heap is all used up to start, so the first
  // allocation initializes the pool
  v->fp = (fr) (v->hp = v->sp = v->pool + v->len);
  // everything else starts empty
  v->ip = (mo) (v->topl = v->syms = v->xp = nil);
  setw(v->lex, nil, LexN);

  ob _;

  bool ok =
    // global symbols // FIXME stop using these if possible
    (v->lex[Eval] = interns(v, "ev")) &&
    (v->lex[Def] = interns(v, ":")) &&
    (v->lex[Cond] = interns(v, "?")) &&
    (v->lex[Lamb] = interns(v, "\\")) &&
    (v->lex[Quote] = interns(v, "`")) &&
    (v->lex[Seq] = interns(v, ",")) &&
    (v->lex[Splat] = interns(v, ".")) &&

    // make the global namespace
    (v->topl = table(v)) &&
    (_ = interns(v, "_ns")) &&
    tbl_set(v, v->topl, _, v->topl)
    // register instruction addresses at toplevel so the
    // compiler can use them.
#define reg_intl(a) && inst(v, "i-"#a, a)
    i_internals(reg_intl)
    && define_primitives(v);

  if (!ok) free(v->pool);
  return ok; }

#ifndef PREF
#define PREF
#endif
#ifndef LANG
#define LANG
#endif
#ifndef SUFF
#define SUFF
#endif

// called after finishing successfully
static Vm(yield) { return Pack(), xp; }

static void repl(la v) {
  struct mo go[] = { {call}, {(vm*) putnum(1)}, {yield} };
  while (!feof(stdin)) {
    ob _ = rx(v, stdin);
    if (!_ && !feof(stdin)) fputs("# parse error\n", stderr);
    if (_ && Push(_)) {
      _ = call(v, (ob) primitives, go, v->hp, v->sp, v->fp);
      if (_) tx(v, stdout, _), fputc('\n', stdout); } } }

// takes scripts and if we want a repl, gives a thread
static mo act(la v, const char **nfs) {
  const char *nf = *nfs;
  mo k = nf ? act(v, nfs + 1) : mkmo(v, 1);
  return !k ? 0 :
    nf ? ana_p(v, nf, (ob) k) :
    (G(k) = yield, k); }

static mo actn(la v, const char *prelu, const char **scripts) {
  mo k = act(v, scripts);
  return k && prelu ? ana_p(v, prelu, (ob) k) : k; }

static NoInline ob la_go(la v) {
  ob xp, *hp, *sp; fr fp; mo ip;
  return Unpack(), ApN(0, xp); }

static NoInline int la_main(bool shell, const char *prelu, const char **scripts) {
  struct la V;
  if (!la_ini(&V)) return EXIT_FAILURE;
  V.ip = actn(&V, prelu, scripts);
  bool _ = V.ip && la_go(&V);
  if (_ && shell) repl(&V);
  free(V.pool);
  return _ ? EXIT_SUCCESS : EXIT_FAILURE; }

#include <getopt.h>
int main(int ac, char **av) {
  static const char
    *prelu = PREF "/lib/" LANG "/" LANG "." SUFF,
    *usage =
      "usage: %s [options and scripts]\n"
      "with no arguments, interact\n"
      "option:\n"
      "  -h show this message\n"
      "  -i interact\n"
      "  -_ don't bootstrap\n";

  for (bool shell = ac == 1;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, usage, *av); continue;
    case 'i': shell = true; continue;
    case '_': prelu = NULL; continue;
    case -1:
      av += optind;
      prelu = shell || optind != ac ? prelu : NULL;
      return la_main(shell, prelu, (const char**) av); } }
