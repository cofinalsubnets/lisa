#include "i.h"
#include <stdarg.h>
#include <getopt.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

// input methods to read from source files
static int file_getc(core f, input i) { return getc((FILE*) i->data[0]); }
static void file_ungetc(core f, input i, char c) { ungetc(c, (FILE*) i->data[0]); }
static bool file_eof(core f, input i) { return feof((FILE*) i->data[0]); }

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

  state f = l_open();
  status s = f ? Ok : Oom;

  for (av += optind; s == Ok && *av; av++) {
    FILE *file = fopen(*av, "r");
    if (!file) {
      fprintf(stderr, "# error opening %s: %s\n", *av, strerror(errno));
      s = Eof;
      break; }
    void *_i[] = { file_getc, file_ungetc, file_eof, file };
    input i = (input) _i;
    // evaluate expressions for side effects
    while ((s = read1i(f, i)) != Eof && (s = eval(f)) == Ok) f->sp++;
    fclose(file);
    if (s == Eof) s = Ok;
    if (s != Ok) report(f, &std_error, s); }

  // repl
  if (s == Ok && interact)
    for (status t; (t = read1i(f, &std_input)) != Eof;) {
      if (t == Ok && (t = eval(f)) == Ok)
        transmit(f, &std_output, pop1(f)),
        std_output.putc(f, &std_output, '\n');
      else report(f, &std_error, t), f->sp = f->pool + f->len; }

  return l_close(f), s; }
