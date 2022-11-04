#include "la.h"
#include <getopt.h>

int main(int ac, char **av) {
  const char
    *usage =
      "usage: %s [options and scripts]\n"
      "with no arguments, interact\n"
      "option:\n"
      "  -h show this message\n"
      "  -i interact\n"
      "  -_ don't bootstrap\n";

  bool boot = true, shell = ac == 1;

  for (;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, usage, *av); continue;
    case 'i': shell = true; continue;
    case '_': boot = false; continue;
    case -1: av += optind; goto out; } out:

  // exit now if there's nothing to do.
  if (ac == optind && !shell) return EXIT_SUCCESS;

  // init
  struct la V;
  bool ok = la_open(&V);

  if (ok && boot) {
    ok = la_lib(&V, "lisa");
    if (!ok) errp(&V, "couldn't locate boot script"); }


  // run scripts
  while (ok && *av) ok = la_script(&V, *av++);

  // repl
  if (ok && shell) while (!feof(stdin)) {
    ob _ = la_rx(&V, stdin);
    if (!_ && !feof(stdin)) errp(&V, "parse error");
    if (_ && (_ = la_ev(&V, _)))
      la_tx(&V, stdout, _), fputc('\n', stdout); }

  la_close(&V);

  return ok ? EXIT_SUCCESS : EXIT_FAILURE; }
