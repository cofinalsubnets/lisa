#include "la.h"

static FILE *la_ld_boot_seek(la_carrier v) {
  FILE *b;
  char *home, buf[256];
  if ((home = getenv("HOME"))) {
   if (snprintf(buf, sizeof(buf), "%s/.local/lib/lisa/boot.la", home) < sizeof(buf) &&
       (b = fopen(buf, "r")))
     return b; }
  if ((b = fopen("/usr/local/lib/lisa/boot.la", "r"))) return b;
  if ((b = fopen("/usr/lib/lisa/boot.la", "r"))) return b;
  return fopen("/lib/lisa/boot.la", "r"); }

static enum la_status la_ld_boot(la_carrier v) {
  FILE *in = la_ld_boot_seek(v);
  if (!in) return LA_XSYS;
  enum la_status s = la_ev_fs(v, in);
  return fclose(in), s; }

static const char *usage =
  "usage: %s [options] [scripts]\n"
  "with no arguments, interact\n"
  "options:\n"
  "  -h show this message\n"
  "  -i interact\n"
  "  -_ don't bootstrap\n";

#include <getopt.h>
#include <unistd.h>
int main(int ac, char **av) {
  bool boot = true, shell = ac == 1 && isatty(STDIN_FILENO);
  for (;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(la_stdout, usage, *av); continue;
    case 'i': shell = true; continue;
    case '_': boot = false; continue;
    case -1: av += optind; goto out; } out:

  // exit now if there's nothing to do.
  if (ac == optind && !shell) return EXIT_SUCCESS;

  // init
  struct la_carrier V;
  enum la_status s = la_open(&V);

  if (s == LA_OK && boot) s = la_ld_boot(&V);

  // run scripts
  while (s == LA_OK && *av) {
    const char *path = *av++;
    FILE *in = fopen(path, "r");
    if (!in) s = LA_XSYS;
    else s = la_ev_fs(&V, in), fclose(in); }

  // repl
  if (s == LA_OK && shell) for (;;) {
    enum la_status t = la_ev_f(&V, la_stdin);
    if (t == LA_EOF) break;
    if (t == LA_OK) la_tx(&V, la_stdout, V.xp), la_putc('\n', la_stdout);
    else la_perror(&V, t), la_reset(&V); }

  return la_perror(&V, s), la_close(&V), s; }
