#include "la.h"
#include <getopt.h>
#include <unistd.h>

static int main_process(u1, u1, char**);
int main(int ac, char **av) {
  static const char *help =
    "usage: %s [options] [scripts]\n"
    "with no arguments, interact\n"
    "options:\n"
    "  -h show this message\n"
    "  -i interact\n"
    "  -_ don't bootstrap\n";
  for (u1 boot = true, repl = ac == 1 && isatty(STDIN_FILENO);;)
    switch (getopt(ac, av, "hi_")) {
      default: return EXIT_FAILURE;
      case 'i': repl = true; continue;
      case '_': boot = false; continue;
      case 'h': fprintf(stdout, help, *av); continue;
      case -1: return ac == optind && !repl ? EXIT_SUCCESS :
        main_process(boot, repl, av + optind); } }

static enum status ev_src(struct carrier *v, FILE *in) {
  if (!in) return LA_XSYS;
  for (enum status r;;)
    if ((r = receive(v, in)) != LA_OK ||
        (r = la_ev_x(v, v->xp)) != LA_OK)
    return fclose(in),
           r == LA_EOF ? LA_OK : r; }

static FILE *boot_src(void) {
  FILE *b; char *home, buf[256]; return
  (home = getenv("HOME")) &&
  snprintf(buf, sizeof(buf), "%s/.local/lib/lisa/boot.la", home) < sizeof(buf) &&
  ((b = fopen(buf, "r")) ||
   (b = fopen("/usr/local/lib/lisa/boot.la", "r")) ||
   (b = fopen("/usr/lib/lisa/boot.la", "r"))) ? b :
   fopen("/lib/lisa/boot.la", "r"); }

static int main_process(u1 boot, u1 repl, char **av) {
  struct carrier V;
  enum status s = la_ini(&V);
  if (s == LA_OK && boot) s = ev_src(&V, boot_src());
  while (s == LA_OK && *av) s = ev_src(&V, fopen(*av++, "r"));
  if (s == LA_OK && repl)
    while ((s = receive(&V, stdin)) == LA_OK &&
           (s = la_ev_x(&V, V.xp)) != LA_EOF) {
      if (s != LA_OK) la_perror(&V, s), la_reset(&V);
      else transmit(&V, stdout, V.xp), putc('\n', stdout); }
  return la_perror(&V, s),
         la_fin(&V),
         s == LA_EOF ? LA_OK : s; }
