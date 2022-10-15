#include "la.h"
#include "vm.h"
#include <time.h>
#include <stdlib.h>

static ob interns(la v, const char *s) {
  ob _ = string(v, s);
  return _ ? intern(v, _) : 0; }

// initialization helpers
//
// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline ob inst(la v, const char *a, vm *b) {
  ob z = interns(v, a);
  return z ? tbl_set(v, A(v->wns), z, putnum(b)) : 0; }

// make a primitive function
static NoInline ob prim(la v, const char *a, vm *i) {
  mo k = 0;
  ob nom = interns(v, a);
  if (nom) nom = pair(v, nom, nil);
  if (nom) with(nom, k = mkmo(v, 2));
  if (!k) return 0;
  k[0].ll = i;
  k[1].ll = (vm*) nom;
  return tbl_set(v, A(v->wns), A(nom), (ob) k); }

// initialize a process
static bool la_ini(la v) {
  ob _;
  // set time & random seed
  v->t0 = clock(),
  v->rand = v->t0,

  // configure memory
  // how big a memory pool do we start with?
  v->len = 1 << 10,
  // there is no pool yet
  v->pool = NULL,
  // no protected values
  v->keep = NULL,
  // the data stack starts at the top of memory
  v->sp = v->pool + v->len,
  // the call stack lives on the data stack
  v->fp = (fr) v->sp,
  // the heap is all used up to start, so the first
  // allocation initializes the pool
  v->hp = v->sp,
  // everything else starts empty
  v->ip = (mo) nil,
  v->wns = v->syms = v->xp = nil,
  setw(v->lex, nil, LexN);

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
    (_ = table(v)) &&
    (v->wns = pair(v, _, nil))
    // register instruction addresses at toplevel so the
    // compiler can use them.
#define register_inst(a, b) && ((b) ? prim(v,b,a) : inst(v, "i-"#a,a))
    insts(register_inst);

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

static ob ev(la v, ob x) {
  mo k;
  if (!Push(x) || !(k = mkmo(v, 6))) return 0;
  k[0].ll = imm;
  k[1].ll = (vm*) (k + 5);
  k[2].ll = call;
  k[3].ll = (vm*) putnum(1);
  k[4].ll = yield;
  k[5].ll = ev_u;
  return imm(v, nil, k, v->hp, v->sp, v->fp); }

static ob rxq(la v, FILE *i) {
  ob x; return
    (x = rx(v, i)) &&
    (x = pair(v, x, nil)) ?
    (x = pair(v, v->lex[Quote], x)) : 0; }

static ob ana_fd(la v, FILE *in, ob k) {
  ob x; return
    with(k, x = rxq(v, in)),
    !x ? feof(in) ? k : 0 :
      (with(x, k = ana_fd(v, in, k)), k) &&
      (with(k, x = (x = pair(v, x, nil)) ?
                   pair(v, v->lex[Eval], x) : 0), x) ?
      (ob) ana(v, x, k) : 0; }

#include <string.h>
#include <errno.h>
static mo ana_p(la v, const char *path, ob k) {
  FILE *in = fopen(path, "r");
  if (!in) return
    fprintf(stderr, "%s : %s", path, strerror(errno)),
    NULL;
  return
    k = ana_fd(v, in, k),
    fclose(in),
    (mo) k; }

// read eval print loop. starts after all scripts if indicated
static Vm(repl) {
  for (Pack(); !feof(stdin);) {
    if (!(xp = rx(v, stdin)))
      err(v, 0, "parse error");
    else if ((xp = ev(v, xp)))
      tx(v, stdout, xp),
      fputc('\n', stdout); }
  return nil; }

// takes scripts and if we want a repl, gives a thread
static mo act(la v, bool shell, const char **nfs) {
  const char *nf = *nfs;
  mo k = nf ? act(v, shell, nfs + 1) : mkmo(v, 1);
  if (!k) return 0;
  if (nf) return ana_p(v, nf, (ob) k);
  k[0].ll = shell ? repl : yield;
  return k; }

static mo actn(la v, bool shell, const char *prelu, const char **scripts) {
  mo k = act(v, shell, scripts);
  if (k && prelu) k = ana_p(v, prelu, (ob) k);
  return k; }

static NoInline ob la_go(la v) {
  ob xp, *hp, *sp; fr fp; mo ip;
  Unpack();
  return ApN(0, xp); }

static NoInline int la_main(bool shell, const char *prelu, const char **scripts) {
  la v = &((struct la){});
  if (!la_ini(v)) return EXIT_FAILURE;
  mo k = actn(v, shell, prelu, scripts);
  if (!k) return EXIT_FAILURE;
  v->ip = k;
  ob r = la_go(v);
  free(v->pool);
  return r ? EXIT_SUCCESS : EXIT_FAILURE; }

static const char
  *prelu = PREF "/lib/" LANG "/" LANG "." SUFF,
  *usage =
    "usage: %s [options and scripts]\n"
    "with no arguments, interact\n"
    "option:\n"
    "  -h show this message\n"
    "  -i interact\n"
    "  -_ don't bootstrap\n";

#include <getopt.h>
int main(int ac, char **av) {
  for (bool shell = ac == 1;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, usage, *av); continue;
    case 'i': shell = true; continue;
    case '_': prelu = NULL; continue;
    case -1:
      av += optind;
      prelu = shell || optind != ac ? prelu : NULL;
      return la_main(shell, prelu, (const char**) av); } }
