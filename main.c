#include "i.h"
#include <stdarg.h>
#include <getopt.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

static void report(core, status);
static const char *help = // help message
  "usage: %s [options] [scripts]\n"
  "with no arguments, interact\n"
  "options:\n"
  "  -h show this message\n"
  "  -i interact\n";

static status repl(core), run_files(core, char**);

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
  if (!f) return Oom;
  status s = run_files(f, av + optind);
  if (s == Ok && interact) // repl
    for (status t; (t = read1i(f, &std_input)) != Eof;) {
      if (t == Ok && (t = eval(f)) == Ok)
        transmit(f, &std_output, pop1(f)),
        std_output.putc(f, &std_output, '\n');
      else report(f, t), f->sp = f->pool + f->len; }
  l_close(f);
  return s; }

static void report(core f, status s) {
  switch (s) {
    case Ok: case Eof: return;
    case Oom:
      outputs(f, &std_error, "# oom@2*");
      print_num(f, &std_error, f->len * sizeof(word), 10);
      outputs(f, &std_error, "B\n"); } }

static int file_getc(core f, input i) { return getc((FILE*) i->data[0]); }
static void file_ungetc(core f, input i, char c) { ungetc(c, (FILE*) i->data[0]); }
static bool file_eof(core f, input i) { return feof((FILE*) i->data[0]); }
static status run_files(core f, char **av) {
  for (status s; *av; av++) {
    FILE *file = fopen(*av, "r");
    if (!file) return
      fprintf(stderr, "# error opening %s: %s\n", *av, strerror(errno)),
      Eof;
    void *_i[] = { file_getc, file_ungetc, file_eof, file };
    input i = (input) _i;
    // evaluate expressions for side effects
    while ((s = read1i(f, i)) != Eof && (s = eval(f)) == Ok) f->sp++;
    fclose(file);
    if (s == Eof) s = Ok;
    if (s != Ok) return report(f, s), s; }
  return Ok; }
