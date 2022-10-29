#include "la.h"
#include "vm.h"

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
static const struct mo go[] = { {call}, {(vm*) putnum(1)}, {yield} };

ob la_ev_x(la v, ob _) {
  if (!Push(_)) return 0;
  return call(v, (ob) primitives, (mo) go, v->hp, v->sp, v->fp); }

ob la_ev_f(la v, FILE *i) {
  ob _ = la_rx_f(v, i);
  return _ ? la_ev_x(v, _) : 0; }

ob la_ev_s(la v, const char **s) {
  ob _ = la_rx_s(v, s);
  return _ ? la_ev_x(v, _) : 0; }

static void repl(la v) {
  while (!feof(stdin)) {
    ob _ = rx(v, stdin);
    if (!_ && !feof(stdin)) errp(v, "# parse error\n");
    if (_ && (_ = la_ev_x(v, _))) tx(v, stdout, _), fputc('\n', stdout); } }

// takes scripts and if we want a repl, gives a thread
static mo act(la v, const char **nfs) {
  const char *nf = *nfs;
  mo k = nf ? act(v, nfs + 1) : (mo) go + 2;
  return !k || !nf ? k : ana_p(v, nf, (ob) k); }

static mo actn(la v, const char *prelu, const char **scripts) {
  mo k = act(v, scripts);
  return k && prelu ? ana_p(v, prelu, (ob) k) : k; }

static NoInline ob la_go(la v) {
  ob xp, *hp, *sp; fr fp; mo ip;
  return Unpack(), ApN(0, xp); }

static NoInline int la_main(bool shell, const char *prelu, const char **scripts) {
  la v = la_ini();
  if (!v) return EXIT_FAILURE;
  v->ip = actn(v, prelu, scripts);
  bool _ = v->ip && la_go(v);
  if (_ && shell) repl(v);
  la_fin(v);
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
