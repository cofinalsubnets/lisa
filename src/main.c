#include "i.h"
#include <stdarg.h>
#include <getopt.h>
#include <unistd.h>
#include <errno.h>

static status report(core, status);
static const char *help = // help message
  "usage: %s [options] [scripts]\n"
  "with no arguments, interact\n"
  "options:\n"
  "  -h show this message\n"
  "  -i interact\n";

int main(int ac, char **av) {
  // by default start a repl if in a terminal and no arguments
  bool repl = ac == 1 && isatty(STDIN_FILENO);

  // read command line flags
  for (;;) switch (getopt(ac, av, "hi")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, help, *av); continue;
    case 'i': repl = true; continue;
    case -1: goto out; } out:

  // exit if nothing to do
  if (ac == optind && !repl) return EXIT_SUCCESS;

  // initialize
  state f = &((struct l_core){});
  status s = l_ini(f);
  if (s != Ok) return s;

  // run files from command line
  for (av += optind; s == Ok && *av; av++) {
    FILE *i = fopen(*av, "r");
    if (!i) return
      fprintf(stderr, "# error opening %s: %s\n", *av, strerror(errno)),
      l_fin(f),
      Eof;
    while ((s = read1(f, i)) != Eof &&
           (s = eval(f, pop1(f))) == Ok) f->sp++;
    if (s == Eof) s = Ok;
    if (s != Ok) report(f, s);
    fclose(i); }

  // interact if needed
  if (s == Ok && repl) while ((s = read1(f, stdin)) != Eof) {
    if (s == Ok && (s = eval(f, pop1(f)) == Ok))
      transmit(f, stdout, pop1(f)),
      fputc('\n', stdout);
    else report(f, s),
      f->sp = f->pool + f->len; }

  // finalize and exit
  return l_fin(f), s; }

static status report(core f, status s) {
  switch (s) {
    case Oom:
      fprintf(stderr, "# oom@2*%ldB\n", f->len * sizeof(word));
    default: }
  return s; }
