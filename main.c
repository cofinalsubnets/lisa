#include "gwen.h"
#include <stdarg.h>
#include <getopt.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// input methods to read from source files
static int file_getc(gwen_core f, gwen_input i) {
  return getc((FILE*) i->data[0]); }
static void file_ungetc(gwen_core f, gwen_input i, char c) {
  ungetc(c, (FILE*) i->data[0]); }
static bool file_eof(gwen_core f, gwen_input i) {
  return feof((FILE*) i->data[0]); }

static int stdin_getc(gwen_core f, gwen_input i) { return getc(stdin); }
static void stdin_ungetc(gwen_core f, gwen_input i, char c) { ungetc(c, stdin); }
static bool stdin_eof(gwen_core f, gwen_input i) { return feof(stdin); }
static void stdout_putc(gwen_core f, gwen_output o, char c) { putc(c, stdout); }
static void stderr_putc(gwen_core f, gwen_output o, char c) { putc(c, stderr); }
struct gwen_char_in
  std_input = { .getc = stdin_getc, .ungetc = stdin_ungetc, .eof = stdin_eof };
struct gwen_char_out
  std_output = { .putc = stdout_putc },
  std_error = { .putc = stderr_putc };

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
    void *_i[] = { file_getc, file_ungetc, file_eof, file };
    input i = (input) _i;
    // evaluate expressions for side effects
    while ((s = read1i(f, i)) != Eof && (s = eval(f)) == Ok) pop1(f);
    fclose(file);
    if (s == Eof) s = Ok;
    if (s != Ok) report(f, &std_error, s); }

  // repl
  if (s == Ok && interact)
    for (gwen_status t; (t = read1i(f, &std_input)) != Eof;) {
      if (t == Ok && (t = eval(f)) == Ok)
        transmit(f, &std_output, pop1(f)),
        std_output.putc(f, &std_output, '\n');
      else report(f, &std_error, t), reset_stack(f); }

  return gwen_close(f), s; }
