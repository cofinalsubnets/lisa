#include "gwen.h"
#include <getopt.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define Ok GwenStatusOk
#define Eof GwenStatusEof
#define Oom GwenStatusOom

static const char *help = // help message
  "usage: %s [options] [scripts]\n"
  "with no arguments, interact\n"
  "options:\n"
  "  -h show this message\n"
  "  -i interact\n";

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

  gwen_core f = gwen_open();
  gwen_status s = f ? Ok : Oom;

  for (av += optind; s == Ok && *av; av++) {
    FILE *file = fopen(*av, "r");
    if (!file) {
      fprintf(stderr, "# error opening %s: %s\n", *av, strerror(errno));
      s = Eof;
      break; }
    // evaluate expressions for side effects
    while ((s = read1f(f, file)) != Eof && (s = eval(f)) == Ok) pop1(f);
    fclose(file);
    if (s == Eof) s = Ok;
    if (s != Ok) report(f, &std_error, s); }

  // repl
  if (s == Ok && interact)
    for (gwen_status t; (t = read1i(f, &std_input)) != Eof;) {
      if (t == Ok && (t = eval(f)) == Ok)
        transmit(f, &std_output, pop1(f)),
        puts("");
      else report(f, &std_error, t), reset_stack(f); }

  return gwen_close(f), s; }
