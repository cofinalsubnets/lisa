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

  if (s == LA_OK && boot)
    s = la_ld_lib(&V, "boot"),
    la_perror(&V, s, stderr);

  // run scripts
  while (s == LA_OK && *av) {
    const char *path = *av++;
    FILE *in = fopen(path, "r");
    if (!in) {
      la_perror(&V, s = LA_XSYS, stderr);
      break; }
    s = la_ev_stream(&V, in);
    fclose(in);
    la_perror(&V, s, stderr); }

  // repl
  if (s == LA_OK && shell) for (;;) {
    enum la_status t = la_ev_f(&V, stdin);
    if (t == LA_EOF) break;
    if (t == LA_OK) la_tx(&V, stdout, V.xp), fputc('\n', stdout);
    else la_perror(&V, t, stderr), la_reset(&V); }

  return la_close(&V), s; }
