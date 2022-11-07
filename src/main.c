#include "la.h"
#include <getopt.h>
#include <string.h>
#include <errno.h>

static const char *usage =
  "usage: %s [options and scripts]\n"
  "with no arguments, interact\n"
  "option:\n"
  "  -h show this message\n"
  "  -i interact\n"
  "  -_ don't bootstrap\n";

int main(int ac, char **av) {
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
  struct la_carrier V;
  bool ok = la_open(&V) == LA_OK;

  if (ok && boot) {
    ok = la_lib(&V, "boot") == LA_OK;
    if (!ok) errp(&V, "bootstrap failed"); }

  // run scripts
  while (ok && *av) {
    const char *path = *av++;
    FILE *in = fopen(path, "r");
    if (!in)
      errp(&V, "%s : %s", path, strerror(errno)),
      ok = false;
    else {
      la_status s;
      do s = la_ev_f(&V, in);
      while (s == LA_OK);
      fclose(in);
      ok = s == LA_EOF;
      if (!ok) errp(&V, "%s : %s", path, "error"); } }

  // repl
  if (ok && shell) for (;;) {
    la_status s = la_ev_f(&V, stdin);
    if (s == LA_EOF) break;
    if (s == LA_OK) la_tx(&V, stdout, V.xp), fputc('\n', stdout); }
    // TODO indicate parse error

  la_close(&V);
  return ok ? EXIT_SUCCESS : EXIT_FAILURE; }
