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

static ob eval(la, ob), ana_fd(la, fd, ob);

// called after finishing successfully
static Ll(yield) { return Pack(), xp; }

// read eval print loop. starts after all scripts as indicated
static Vm(repl) { for (Pack();;) {
  if ((xp = parse(v, stdin))) {
    if ((xp = eval(v, xp))) 
      emit(v, xp, stdout), fputc('\n', stdout); }
  else if (feof(stdin)) return ApC(yield, nil); } }

static ob eval(em v, ob x) {
  ob *k; return
    !(Push(x) && (k = cells(v, 6))) ? 0 :
    (k[0] = (ob) call,
     k[1] = putnum(1),
     k[2] = (ob) yield,
     k[3] = (ob) ev_u,
     k[4] = 0,
     k[5] = (ob) k,
     call(v, (ob) (k + 3), (mo) k, v->hp, v->sp, v->fp)); }

// pull back k over a path
mo ana_p(la v, const char *path, ob k) {
  FILE *in = fopen(path, "r");
  return !in ?
    (mo) err(v, 0, "%s : %s", path, strerror(errno)) :
    (k = ana_fd(v, in, k),
     fclose(in),
     (mo) k);}

static ob ana_fd(ph v, fd in, ob k) {
  ob x; with(k, x = parse(v, in));
  return !x ? feof(in) ? k : 0 :
    (with(x, k = ana_fd(v, in, k)), k) &&
    (with(k, x =
      (x = pair(v, x, nil)) &&
      (x = pair(v, v->lex[Quote], x)) &&
      (x = pair(v, x, nil)) ?
      pair(v, v->lex[Eval], x) : 0), x) ?
    (ob) ana(v, x, k) : 0; }

// takes scripts and if we want a repl, gives a thread
static mo act(em v, bool shell, const char **paths) {
  const char *path = *paths; mo k; return
    !path ? !(k = cells(v, 3)) ? 0 :
               (k[0].ll = shell ? repl : yield,
                k[1].ll = 0, k[2].ll = (ll*) k, k) :
    (k = act(v, shell, paths + 1)) &&
    (k = ana_p(v, path, (ob) k)) ? k : 0; }

int main(int argc, char **argv) {

  static const char
    *boot = PREF "/lib/" LANG "/" LANG "." SUFF,
    *help =
      "usage: %s [options and scripts]\n"
      "with no arguments, start a repl\n"
      "options:\n"
      "  -h print this message\n"
      "  -i start repl unconditionally\n"
      "  -_ don't bootstrap\n"
    ;

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


