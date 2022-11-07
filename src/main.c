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
  enum la_status s = la_open(&V);

  if (s == LA_OK && boot) {
    s = la_lib(&V, "boot");
    if (s != LA_OK) errp(&V, "bootstrap failed"); }

  // run scripts
  while (s == LA_OK && *av) {
    const char *path = *av++;
    FILE *in = fopen(path, "r");
    if (!in)
      errp(&V, "%s : %s", path, strerror(errno)),
      s = LA_XSYS;
    else {
      do s = la_ev_f(&V, in);
      while (s == LA_OK);
      fclose(in);
      s = s == LA_EOF ? LA_OK : s;
      if (s != LA_OK) errp(&V, "%s : %s", path, "error"); } }

  // repl
  if (s == LA_OK && shell) for (;;) {
    enum la_status t = la_ev_f(&V, stdin);
    if (t == LA_EOF) break;
    if (t == LA_OK) la_tx(&V, stdout, V.xp), fputc('\n', stdout); }
    // TODO indicate parse error

  la_close(&V);
  return s; }
