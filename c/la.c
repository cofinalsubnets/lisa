#include "la.h"
#include <time.h>
#include <stdlib.h>

// finalize a process
void t1(pt v) { if (v) free(v->pool), free(v); }

static ob interns(pt v, const char *s) {
  ob _; return (_ = string(v, s)) ? intern(v, _) : 0; }

// initialization helpers
//
// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline bool inst(pt v, const char *a, host *b) {
  ob z; return !(z = interns(v, a)) ? 0 :
    !!tbl_set(v, A(v->wns), z, putnum(b)); }

// make a primitive function
static NoInline bool prim(pt v, const char *a, host *i) {
  ob nom; mo k; return
    (nom = interns(v, a)) &&
    (nom = pair(v, nom, nil)) &&
    (with(nom, k = cells(v, 4)), k) ?
      !!tbl_set(v, A(v->wns), A(nom), (ob)
        (k[0].ll = i,    k[1].ll = (ll*) nom,
         k[2].ll = NULL, k[3].ll = (ll*) k)) : 0; }

// initialize a process
pt t0(void) { ob _; pt v;
// how big a memory pool do we start with?
#define InitialPoolSize (1<<10)
  if ((v = malloc(sizeof(struct pt))))
    // set time & random seed
    v->t0 = clock(),
    v->rand = v->t0,

    // configure memory
    v->len = InitialPoolSize,
    // obviously there's no pool yet
    v->pool = NULL,
    // nor any protected values
    v->keep = NULL,
    // the data stack starts at the top of memory
    v->sp = v->pool + v->len,
    // the call stack lives on the data stack
    v->fp = (fr) v->sp,
    // the heap is all used up to start, so the first
    // allocation initializes the pool
    v->hp = v->sp,
    // everything else starts empty
    v->ip = (dt) nil,
    v->wns = v->sns = v->syms = v->xp = nil,
    setw(v->lex, nil, LexN);

  if (v && !(
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
    insts(register_inst)))
      t1(v), v = NULL;
  return v; }

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
static Ll(yield) { return Pack(), xp; }

static ob ev(pt v, ob x) {
  ob *k;
  with(x, k = cells(v, 11));
  if (!k) return 0;
  k[0] = (ob) imm;
  k[1] = x;
  k[2] = (ob) push;
  k[3] = (ob) imm;
  k[4] = (ob) (k + 8);
  k[5] = (ob) call;
  k[6] = putZ(1);
  k[7] = (ob) yield;
  k[8] = (ob) ev_u;
  k[9] = 0;
  k[10] = x = (ob) k;
  return imm(v, nil, (dt) x, v->hp, v->sp, v->fp); }

static ob ana_fd(ph v, FILE *in, ob k) { ob x; return
  with(k, x = rxq(v, in)),
  !x ? feof(in) ? k : 0 :
    (with(x, k = ana_fd(v, in, k)), k) &&
    (with(k, x = (x = pair(v, x, nil)) ?
                 pair(v, v->lex[Eval], x) : 0), x) ?
    (ob) ana(v, x, k) : 0; }

#include <string.h>
#include <errno.h>
static dt ana_p(la v, const char *path, ob k) {
  FILE *in = fopen(path, "r");
  return !in ?
    (fprintf(stderr, "%s : %s", path, strerror(errno)),
     NULL) :
    (k = ana_fd(v, in, k),
     fclose(in),
     (dt) k); }

// read eval print loop. starts after all scripts if indicated
static Vm(repl) {
  for (Pack(); !feof(stdin);)
    if ((xp = rx(v, stdin)) && (xp = ev(v, xp)))
      tx(v, stdout, xp),
      fputc('\n', stdout);
  return nil; }

// takes scripts and if we want a repl, gives a thread
static dt act(pt v, bool shell, const char **nfs) {
  const char *nf = *nfs;
  dt k = nf ? act(v, shell, nfs + 1) : cells(v, 3);
  return !k ? 0 : nf ?
    ana_p(v, nf, (ob) k) :
    (k[0].ll = shell ? repl : yield,
     k[1].ll = 0,
     k[2].ll = (ll*) k,
     k); }


static int tr(char **argv, bool shell, const char *prelu) {
  pt v = t0();
  ob r = v &&
    (r = (ob) act(v, shell, (const char**) argv)) &&
    (!prelu || (r = (ob) ana_p(v, prelu, r))) &&
    (r = pair(v, r, nil)) ? ev(v, r) : 0;

  t1(v);

  return r ? EXIT_SUCCESS : EXIT_FAILURE; }

#include <getopt.h>
int main(int argc, char **argv) {
  const char
    *prelu = PREF "/lib/" LANG "/" LANG "." SUFF,
    *usage =
      "usage: %s [options and scripts]\n"
      "with no arguments, start a repl\n"
      "options:\n"
      "  -h print this message\n"
      "  -i start repl unconditionally\n"
      "  -_ don't bootstrap\n";

  bool shell = argc == 1;

  for (;;) switch (getopt(argc, argv, "hi_")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, usage, *argv); continue;
    case 'i': shell = true; continue;
    case '_': prelu = NULL; continue;
    case -1:
      argv += optind;
      prelu = shell || optind != argc ? prelu : NULL;
      return tr(argv, shell, prelu); } }
