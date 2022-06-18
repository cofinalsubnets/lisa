#include "la.h"
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <getopt.h>

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

static ob preval(pt v, ob x) {
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
  k[10] = (ob) k;
  return (ob) k; }

static ob eval(pt v, ob x) {
  if (!(x = preval(v, x))) return 0;
  return imm(v, nil, (mo) x, v->hp, v->sp, v->fp); }

static ob ana_fd(ph v, FILE *in, ob k) { ob x; return
  with(k, x = parq(v, in)),
  !x ? feof(in) ? k : 0 :
    (with(x, k = ana_fd(v, in, k)), k) &&
    (with(k, x = (x = pair(v, x, nil)) ?
                 pair(v, v->lex[Eval], x) : 0), x) ?
    (ob) ana(v, x, k) : 0; }

static dt ana_p(la v, const char *path, ob k) {
  FILE *in; return
    !(in = fopen(path, "r")) ?
      (fprintf(stderr, "%s : %s", path, strerror(errno)),
       NULL) :
      (k = ana_fd(v, in, k),
       fclose(in),
       (mo) k);}

// read eval print loop. starts after all scripts as indicated
static Vm(repl) {
  for (Pack();;) {
    xp = parse(v, stdin);
    if (xp) {
      xp = eval(v, xp);
      if (xp) emit(v, xp, stdout),
              fputc('\n', stdout); }
    if (feof(stdin))
      return ApC(yield, nil); } }

// takes scripts and if we want a repl, gives a thread
static dt act(pt v, bool shell, const char **nfs) {
  dt k;
  const char *nf = *nfs;
  return !nf ?
    (k = cells(v, 3)) ?
     (k[0].ll = shell ? repl : yield,
      k[1].ll = 0,
      k[2].ll = (ll*) k,
      k) : 0 :
    (k = act(v, shell, nfs + 1)) ?
      ana_p(v, nf, (ob) k) : 0; }

int tr(char **argv, bool shell, const char *prelu) {
  pt v = t0();

  ob r = v &&
    (r = (ob) act(v, shell, (const char**) argv)) &&
    (!prelu || (r = (ob) ana_p(v, prelu, r))) &&
    (r = pair(v, r, nil)) ? eval(v, r) : 0;

  t1(v);

  return r ? EXIT_SUCCESS : EXIT_FAILURE; }

int main(int argc, char **argv) {
  static const char
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
      if (argc == optind && !shell) return EXIT_SUCCESS;
      else return tr(argv + optind, shell, prelu); } }
