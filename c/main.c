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

static ob preval(em v, ob x) {
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
  return k[10] = (ob) k; }

static ob eval(em v, ob x) {
  return !(x = preval(v, x)) ? 0 :
    imm(v, nil, (mo) x, v->hp, v->sp, v->fp); }

static ob ana_fd(ph v, FILE *in, ob k) {
  ob x; with(k, x = parq(v, in));
  return !x ? feof(in) ? k : 0 :
    (with(x, k = ana_fd(v, in, k)), k) &&
    (with(k, x =
      (x = pair(v, x, nil)) ?
      pair(v, v->lex[Eval], x) : 0), x) ?
    (ob) ana(v, x, k) : 0; }

static mo ana_p(la v, const char *path, ob k) {
  FILE *in = fopen(path, "r");
  return !in ?
    (fprintf(stderr, "%s : %s", path, strerror(errno)),
     NULL) :
    (k = ana_fd(v, in, k),
     fclose(in),
     (mo) k);}

// read eval print loop. starts after all scripts as indicated
static Vm(repl) { for (Pack();;) {
  if ((xp = parse(v, stdin))) {
    if ((xp = eval(v, xp))) 
      emit(v, xp, stdout), fputc('\n', stdout); }
  else if (feof(stdin)) return ApC(yield, nil); } }

// takes scripts and if we want a repl, gives a thread
static mo act(em v, bool shell, const char **paths) {
  const char *path = *paths; mo k; return
    !path ? !(k = cells(v, 3)) ? 0 :
               (k[0].ll = shell ? repl : yield,
                k[1].ll = 0, k[2].ll = (ll*) k, k) :
    (k = act(v, shell, paths + 1)) ?
    ana_p(v, path, (ob) k) : 0; }

int main(int argc, char **argv) {

  static const char
    *boot = PREF "/lib/" LANG "/" LANG "." SUFF,
    *help =
      "usage: %s [options and scripts]\n"
      "with no arguments, start a repl\n"
      "options:\n"
      "  -h print this message\n"
      "  -i start repl unconditionally\n"
      "  -_ don't bootstrap\n";

  for (bool shell = argc == 1;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return EXIT_FAILURE;
      case 'h': fprintf(stdout, help, *argv); continue;
      case 'i': shell = true; continue;
      case '_': boot = NULL; continue;
      case -1:
        if (argc == optind && !shell) return EXIT_SUCCESS;
        argv += optind;
        em v; ob r =
          (v = la0()) &&
          (r = (ob) act(v, shell, (const char**) argv)) &&
          (!boot || (r = (ob) ana_p(v, boot, r))) &&
          (r = pair(v, r, nil)) ?
          eval(v, r) : 0;

        return la1(v), r ?
         EXIT_SUCCESS : EXIT_FAILURE; } }
