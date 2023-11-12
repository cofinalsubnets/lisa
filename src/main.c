#include "i.h"
#include <stdarg.h>
#include <getopt.h>
#include <unistd.h>
#include <errno.h>

static status go(state, char**, bool);
static const char *help =
  "usage: %s [options] [scripts]\n"
  "with no arguments, interact\n"
  "options:\n"
  "  -h show this message\n"
  "  -i interact\n";

int main(int ac, char **av) {
  bool repl = ac == 1 && isatty(STDIN_FILENO);
  for (;;) switch (getopt(ac, av, "hi")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, help, *av); continue;
    case 'i': repl = true; continue;
    case -1: goto out; } out:
//  if (ac == optind && !repl) return EXIT_SUCCESS;
  state f = &((struct gwen){});
  status s = l_ini(f);
  if (s == Ok) s = go(f, av + optind, repl), l_fin(f);
  return s; }

static status go(state f, char **av, bool repl) {
#ifdef testing
  self_test(f);
#endif
  for (status s; *av; av++) {
    FILE *i = fopen(*av, "r");
    if (!i) return
      fprintf(stderr, "# error opening %s: %s\n", *av, strerror(errno)),
      Dom;
    while ((s = read1(f, i)) != Eof &&
           (s = eval(f, pop1(f))) == Ok) pop1(f);
    fclose(i);
    if (s == Eof) s = Ok;
    if (s != Ok) return report(f, s); }

  // repl
  if (repl) for (status s; (s = read1(f, stdin)) != Eof;) {
    if (s == Ok && (s = eval(f, pop1(f))) == Ok)
      transmit(f, stdout, pop1(f)),
      fputc('\n', stdout);
    else // there was an error
      report(f, s),
      f->sp = f->pool + f->len; }
  return Ok; }
