#include "la.h"
#include "io.h"
#include "vm.h"
#include <time.h>
#include <stdlib.h>

// finalize a process
void la_fin(la v) { if (v) free(v->pool), free(v); }

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
  if (nom) with(nom, k = mkthd(v, 2));
  if (!k) return 0;
  k[0].ll = i;
  k[1].ll = (vm*) nom;
  return tbl_set(v, A(v->wns), A(nom), (ob) k); }

// initialize a process
la la_ini(void) {
  ob _;
  la v = malloc(sizeof(struct la));
  if (!v) return 0;
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
  v->wns = v->sns = v->syms = v->xp = nil,
  setw(v->lex, nil, LexN);

  bool ok = 
    // global symbols // FIXME stop using these if possible
    (v->lex[Eval] = interns(v, "ev")) &&
    (v->lex[Apply] = interns(v, "ap")) &&
    (v->lex[Def] = interns(v, ":")) &&
    (v->lex[Cond] = interns(v, "?")) &&
    (v->lex[Lamb] = interns(v, "\\")) &&
    (v->lex[Quote] = interns(v, "`")) &&
    (v->lex[Seq] = interns(v, ",")) &&
    (v->lex[Splat] = interns(v, ".")) &&

    // make the toplevel namespace and initialize the cwd
    (_ = table(v)) &&
    (v->wns = pair(v, _, nil)) &&

    // create the directory and store toplevel at 0
    (v->sns = table(v)) &&
    (tbl_set(v, v->sns, nil, A(v->wns))) // &&
    // register instruction addresses at toplevel so the
    // compiler can use them.
#define register_inst(a, b) && ((b) ? prim(v,b,a) : inst(v, "i-"#a,a))
    insts(register_inst);

  return ok ? v : (la_fin(v), NULL); }

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
  if (!Push(x) || !(k = mkthd(v, 6))) return 0;
  k[0].ll = imm;
  k[1].ll = (vm*) (k + 5);
  k[2].ll =  call;
  k[3].ll = (vm*) putnum(1);
  k[4].ll =  yield;
  k[5].ll =  ev_u;
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
  k = ana_fd(v, in, k);
  fclose(in);
  return (mo) k; }

// read eval print loop. starts after all scripts if indicated
static Vm(repl) {
  for (Pack(); !feof(stdin);)
    if ((xp = rx(v, stdin)) && (xp = ev(v, xp)))
      tx(v, stdout, xp),
      fputc('\n', stdout);
  return nil; }

// takes scripts and if we want a repl, gives a thread
static mo act(la v, bool shell, const char **nfs) {
  const char *nf = *nfs;
  mo k = nf ? act(v, shell, nfs + 1) : mkthd(v, 1);
  return !k ? 0 :
    nf ? ana_p(v, nf, (ob) k) :
    (k[0].ll = shell ? repl : yield, k); }

#include <getopt.h>
int main(int argc, char **argv) {
  bool shell = argc == 1;
  const char
    *prelu = PREF "/lib/" LANG "/" LANG "." SUFF,
    *usage =
      "usage: %s [choix et scripts]\n"
      "sans paramètres, commencer l'interaction\n"
      "les choix:\n"
      "  -h afficher ce message\n"
      "  -i interagir\n"
      "  -_ mode nu\n";

  for (;;) switch (getopt(argc, argv, "hi_")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, usage, *argv); continue;
    case 'i': shell = true; continue;
    case '_': prelu = NULL; continue;
    case -1:
      argv += optind;
      prelu = shell || optind != argc ? prelu : NULL;

      la v = la_ini();
      ob r = v &&
        (r = (ob) act(v, shell, (const char**) argv)) &&
        (!prelu || (r = (ob) ana_p(v, prelu, r))) &&
        (r = pair(v, r, nil)) ? ev(v, r) : 0;
      la_fin(v);
      return r ? EXIT_SUCCESS : EXIT_FAILURE; } }
