#include "la.h"
#include <getopt.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>

static la_status la_ev_stream(la_carrier v, la_io in) {
  la_status r;
  do r = la_ev_f(v, in); while (r == LA_OK);
  return r == LA_EOF ? LA_OK : r; }

static FILE *la_ld_boot_seek_sys(la_carrier v) {
  FILE *b;
  if ((b = fopen("/usr/local/lib/lisa/boot.la", "r"))) return b;
  if ((b = fopen("/usr/lib/lisa/boot.la", "r"))) return b;
  return fopen("/lib/lisa/boot.la", "r"); }

static FILE *la_ld_boot_seek(la_carrier v) {
  FILE *b;
  char *home, buf[256];
  if ((home = getenv("HOME")) &&
      snprintf(buf, sizeof(buf), "%s/.local/lib/lisa/boot.la", home) < sizeof(buf) &&
      (b = fopen(buf, "r")))
    return b;
  return la_ld_boot_seek_sys(v); }

enum la_status la_ld_boot(la_carrier v) {
  FILE *in = la_ld_boot_seek(v);
  if (!in) return LA_XSYS;
  la_status s = la_ev_stream(v, in);
  fclose(in);
  return s; }

static const char *usage =
  "usage: %s [options and scripts]\n"
  "with no arguments, interact\n"
  "option:\n"
  "  -h show this message\n"
  "  -i interact\n"
  "  -_ don't bootstrap\n";

int main(int ac, char **av) {
  bool boot = true, shell = ac == 1 && isatty(STDIN_FILENO);
  for (;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(la_io_out, usage, *av); continue;
    case 'i': shell = true; continue;
    case '_': boot = false; continue;
    case -1: av += optind; goto out; } out:

  // exit now if there's nothing to do.
  if (ac == optind && !shell) return EXIT_SUCCESS;

  // init
  struct la_carrier V;
  enum la_status s = la_open(&V);
  la_perror(&V, s);

  if (s == LA_OK && boot) s = la_ld_boot(&V);

  // run scripts
  while (s == LA_OK && *av) {
    const char *path = *av++;
    FILE *in = fopen(path, "r");
    if (!in) s = LA_XSYS;
    else s = la_ev_stream(&V, in), fclose(in); }

  // repl
  if (s == LA_OK && shell) for (;;) {
    enum la_status t = la_ev_f(&V, la_io_in);
    if (t == LA_EOF) break;
    if (t == LA_OK) la_tx(&V, la_io_out, V.xp), la_putc('\n', la_io_out);
    else la_perror(&V, t), la_reset(&V); }

  la_perror(&V, s);
  return la_close(&V), s; }
