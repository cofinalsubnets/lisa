#include "i.h"
#include <getopt.h>
#include <unistd.h>

static void interact(struct V*);
static FILE *boot_src(void);
static enum status source(struct V*, FILE*);

int main(int ac, char **av) {

  static const char *help =
    "usage: %s [options] [scripts]\n"
    "with no arguments, interact\n"
    "options:\n"
    "  -h show this message\n"
    "  -i interact\n"
    "  -_ don't bootstrap\n";

  bool boot = true,
       repl = ac == 1 && isatty(STDIN_FILENO);

  for (;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'i': repl = true; continue;
    case '_': boot = false; continue;
    case 'h': fprintf(stdout, help, *av); continue;
    case -1:
      if (ac == optind && !repl) return EXIT_SUCCESS;
      av += optind;
      struct V v;
      enum status s = la_ini(&v);
      if (s == Ok && boot) s = source(&v, boot_src());
      while (s == Ok && *av) s = source(&v, fopen(*av++, "r"));
      if (s == Ok && repl) interact(&v);
      return report(&v, s), la_fin(&v), s; } }

static enum status source(struct V *v, FILE *in) {
  if (!in) return SystemError;
  for (enum status r;;)
    if ((r = receive(v, in)) != Ok ||
        (r = la_ev_x(v, v->xp)) != Ok)
      return fclose(in), r == Eof ? Ok : r; }

#define NOM "li"
#define SUFF "la"
static FILE *boot_src(void) {
  FILE *b; char *home, buf[256]; return
  (home = getenv("HOME")) &&
  snprintf(buf, sizeof(buf), "%s/.local/lib/" NOM "/boot." SUFF, home) < sizeof(buf) &&
  ((b = fopen(buf, "r")) ||
   (b = fopen("/usr/local/lib/" NOM "/boot." SUFF, "r")) ||
   (b = fopen("/usr/lib/" NOM "/boot." SUFF, "r"))) ? b :
   fopen("/lib/" NOM "/boot." SUFF, "r"); }

static void interact(struct V *v) {
  enum status s;
  while ((s = receive(v, stdin)) == Ok &&
         (s = la_ev_x(v, v->xp)) != Eof)
    if (s == Ok) transmit(v, stdout, v->xp),
                 putc('\n', stdout);
    else report(v, s),
         v->fp = (frame) (v->sp = v->pool + v->len); }

