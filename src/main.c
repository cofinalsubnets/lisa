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

static status repl(core f) {
  for (status s; (s = read1(f, stdin)) != Eof; ) {
    if (s == Ok && (s = eval(f) == Ok))
      transmit(f, stdout, pop1(f)),
      fputc('\n', stdout);
    else report(f, s, stderr),
      f->sp = f->pool + f->len; }
  return Ok; }

static status run_files(core f, char **av) {
  for (status s; *av; av++) {
    FILE *i = fopen(*av, "r");
    if (!i) return
      fprintf(stderr, "# error opening %s: %s\n", *av, strerror(errno)),
      Eof;
    while ((s = read1(f, i)) != Eof &&
           (s = eval(f)) == Ok) f->sp++;
    fclose(i);
    if (s == Eof) s = Ok;
    if (s != Ok) return report(f, s, stderr); }
  return Ok; }

int main(int ac, char **av) {
  // by default start a repl if in a terminal and no arguments
  bool interact = ac == 1 && isatty(STDIN_FILENO);
  // read command line flags
  for (;;) switch (getopt(ac, av, "hi")) {
    default: return EXIT_FAILURE;
    case 'h': fprintf(stdout, help, *av); continue;
    case 'i': interact = true; continue;
    case -1: goto out; } out:
  if (!*av && !interact) return EXIT_SUCCESS;
  struct l_core F;
  state f = &F;
  word *pool = malloc(sizeof(word) * 2), *loop = pool + 1;
  if (!pool) return Oom;
  status s = initialize(f, libc_please, 1, pool, loop);
  s = s != Ok ? s : run_files(f, av + optind);
  s = s != Ok || !interact ? s : repl(f);
  free(min(f->pool, f->loop));
  return s; }

static status report(core f, status s, FILE *err) {
  switch (s) {
    case Oom:
      fprintf(err, "# oom@2*%ldB\n", f->len * sizeof(word));
    default: }
  return s; }
