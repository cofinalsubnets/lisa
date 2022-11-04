#include "la.h"

#ifndef DESTDIR
#define DESTDIR
#endif

static void repl(la v) {
  while (!feof(stdin)) {
    ob _ = la_rx(v, stdin);
    if (!_ && !feof(stdin)) errp(v, "parse error");
    if (_ && (_ = la_ev(v, _)))
      la_tx(v, stdout, _), fputc('\n', stdout); } }

#include <string.h>
#include <errno.h>
bool la_script(la v, const char *path) {
  FILE *in = fopen(path, "r");
  if (!in) return
    errp(v, "%s : %s", path, strerror(errno)),
    false;
  bool ok = true;
  for (ob x; ok && !feof(in);
    x = la_rx(v, in),
    ok = x ? la_ev(v, x) : feof(in));
  if (!ok) errp(v, "%s : %s", path, "error");
  return fclose(in), ok; }

static NoInline int la_main(bool shell, const char *prelu, const char **scripts) {
  la v = la_ini();
  bool ok = v && (!prelu || la_script(v, prelu));
  while (ok && *scripts) ok = la_script(v, *scripts++);
  if (ok && shell) repl(v);
  return la_fin(v),
    ok ? EXIT_SUCCESS : EXIT_FAILURE; }

#include <getopt.h>
int main(int ac, char **av) {
  static const char
    *prelu = DESTDIR "/lib/lisa/lisa.la",
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
