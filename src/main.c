#include "la.h"
#include <getopt.h>

#ifndef DESTDIR
#define DESTDIR
#endif

int main(int ac, char **av) {
  const char
    *prelu = DESTDIR "/lib/lisa/lisa.la",
    *usage =
      "usage: %s [options and scripts]\n"
      "with no arguments, interact\n"
      "option:\n"
      "  -h show this message\n"
      "  -i interact\n"
      "  -_ don't bootstrap\n";

  bool shell = ac == 1;

  for (;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, usage, *av); continue;
    case 'i': shell = true; continue;
    case '_': prelu = NULL; continue;
    case -1: av += optind; goto out; } out:

  // exit now if there's nothing to do.
  if (ac == optind && !shell) return EXIT_SUCCESS;

  // init
  struct la V;
  bool ok = la_open(&V) && (!prelu || la_script(&V, prelu));

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
