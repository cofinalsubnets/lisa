#include "la.h"
#include <getopt.h>
#include <unistd.h>

static NoInline int la_run(bool, bool, char**);

int main(int ac, char **av) {
  static const char *help =
    "usage: %s [options] [scripts]\n"
    "with no arguments, interact\n"
    "options:\n"
    "  -h show this message\n"
    "  -i interact\n"
    "  -_ don't bootstrap\n";
  bool boot = true, shell = ac == 1 && isatty(STDIN_FILENO);
  for (;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(la_stdout, help, *av); continue;
    case 'i': shell = true; continue;
    case '_': boot = false; continue;
    case -1: return ac == optind && !shell ? EXIT_SUCCESS :
     la_run(boot, shell, av + optind); } }

static enum la_status bootstrap(la);
static NoInline int la_run(bool boot, bool shell, char **av) {
  // init
  struct la_carrier V;
  enum la_status s = la_ini(&V);

  if (s == LA_OK && boot) s = bootstrap(&V);

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

  return la_perror(&V, s), la_fin(&V), s; }


static FILE *find_boot_script(la_carrier v) {
  FILE *b;
  char *home, buf[256];
  if ((home = getenv("HOME"))) {
   if (snprintf(buf, sizeof(buf), "%s/.local/lib/lisa/boot.la", home) < sizeof(buf) &&
       (b = fopen(buf, "r")))
     return b; }
  if ((b = fopen("/usr/local/lib/lisa/boot.la", "r"))) return b;
  if ((b = fopen("/usr/lib/lisa/boot.la", "r"))) return b;
  return fopen("/lib/lisa/boot.la", "r"); }

static enum la_status bootstrap(la_carrier v) {
  FILE *in = find_boot_script(v);
  if (!in) return LA_XSYS;
  enum la_status s = la_ev_fs(v, in);
  return fclose(in), s; }
