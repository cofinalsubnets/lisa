#include "la.h"
#include <getopt.h>
#include <unistd.h>

static enum status la_ev_f(struct carrier *v, void *in) {
  enum status s = la_rx(v, in);
  return s != LA_OK ? s : la_ev_x(v, v->xp); }

static enum status la_ev_fs(struct carrier *v, void *in) {
  enum status r;
  do r = la_ev_f(v, in); while (r == LA_OK);
  return r == LA_EOF ? LA_OK : r; }

static NoInline int main2(bool, bool, char**);

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
    case 'h': fprintf(stdout, help, *av); continue;
    case 'i': shell = true; continue;
    case '_': boot = false; continue;
    case -1: return ac == optind && !shell ? EXIT_SUCCESS :
      main2(boot, shell, av + optind); } }

static enum status bootstrap(la);
static NoInline int main2(bool boot, bool shell, char **av) {
  // init
  static struct carrier V;
  enum status s = la_ini(&V);

  if (s == LA_OK && boot) s = bootstrap(&V);

  // run scripts
  while (s == LA_OK && *av) {
    const char *path = *av++;
    FILE *in = fopen(path, "r");
    if (!in) s = LA_XSYS;
    else s = la_ev_fs(&V, in), fclose(in); }

  // repl
  if (s == LA_OK && shell) for (;;) {
    enum status t = la_ev_f(&V, stdin);
    if (t == LA_EOF) break;
    if (t == LA_OK) la_tx(&V, stdout, V.xp), putc('\n', stdout);
    else la_perror(&V, t), la_reset(&V); }

  return la_perror(&V, s), la_fin(&V), s; }


static FILE *find_boot_script(struct carrier *v) {
  FILE *b;
  char *home, buf[256];
  if ((home = getenv("HOME"))) {
   if (snprintf(buf, sizeof(buf), "%s/.local/lib/lisa/boot.la", home) < sizeof(buf) &&
       (b = fopen(buf, "r")))
     return b; }
  if ((b = fopen("/usr/local/lib/lisa/boot.la", "r"))) return b;
  if ((b = fopen("/usr/lib/lisa/boot.la", "r"))) return b;
  return fopen("/lib/lisa/boot.la", "r"); }

static enum status bootstrap(struct carrier *v) {
  FILE *in = find_boot_script(v);
  if (!in) return LA_XSYS;
  enum status s = la_ev_fs(v, in);
  return fclose(in), s; }
