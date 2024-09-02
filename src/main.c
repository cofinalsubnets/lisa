#include "i.h"
#include <stdarg.h>
#include <getopt.h>
#include <unistd.h>
#include <errno.h>

static status report(core, status, FILE*);
static const char *help = // help message
  "usage: %s [options] [scripts]\n"
  "with no arguments, interact\n"
  "options:\n"
  "  -h show this message\n"
  "  -i interact\n";

static status repl(core f, FILE *in, FILE *out, FILE *err) {
  for (status s; (s = read1(f, in)) != Eof; ) {
    if (s == Ok && (s = eval(f, pop1(f)) == Ok))
      transmit(f, out, pop1(f)),
      fputc('\n', out);
    else report(f, s, err),
      f->sp = f->pool + f->len; }
  return Ok; }

static status run_files(core f, char **av) {
  for (status s; s == Ok && *av; av++) {
    FILE *i = fopen(*av, "r");
    if (!i) return
      fprintf(stderr, "# error opening %s: %s\n", *av, strerror(errno)),
      Eof;
    while ((s = read1(f, i)) != Eof &&
           (s = eval(f, pop1(f))) == Ok) f->sp++;
    if (s == Eof) s = Ok;
    if (s != Ok) report(f, s, stderr);
    fclose(i); } }

int main(int ac, char **av) {
  // by default start a repl if in a terminal and no arguments
  bool interact = ac == 1 && isatty(STDIN_FILENO);

  // read command line flags
  for (;;) switch (getopt(ac, av, "hi")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, help, *av); continue;
    case 'i': interact = true; continue;
    case -1: goto out; } out:

  // exit if nothing to do
  if (ac == optind && !interact) return EXIT_SUCCESS;

  // initialize
  const size_t len0 = 1;
  word *pool = malloc(2 * len0 * sizeof(word)),
       *loop = pool + len0;
  if (!pool) return Oom;
  state f = &((struct l_core){});
  status s = initialize(f, libc_please, 1, pool, loop);
  s = s != Ok ? s : run_files(f, av + optind);
  s = s != Ok || !interact ? s : repl(f, stdin, stdout, stderr);
  l_fin(f);
  return s; }


static status report(core f, status s, FILE *err) {
  switch (s) {
    case Oom:
      fprintf(err, "# oom@2*%ldB\n", f->len * sizeof(word));
    default: }
  return s; }
