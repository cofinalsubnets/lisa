#include "gwen.h"
#include <getopt.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#define Ok GwenStatusOk
#define Eof GwenStatusEof
#define Oom GwenStatusOom

static gwen_status catf(gwen_core f, gwen_file in, gwen_file out, bool *y) {
  for (;;) {
    gwen_status s = gwen_read1f(f, in);
    if (s == Eof) return Ok;
    if (s != Ok) return s;
    if (*y) putchar(' ');
    else *y = true;
    gwen_write1f(f, out); } }

static gwen_status expcat(gwen_core f, char **av, bool usestdin) {
  gwen_status s = Ok;
  bool y = false;
  while (s == Ok && *av) {
    gwen_file in = fopen(*av++, "r");
    if (!in) s = Oom;
    else s = catf(f, in, stdout, &y), fclose(in); }
  if (s == Ok && usestdin)
    s = catf(f, stdin, stdout, &y);
  return s; }

static const char *help = // help message
  "usage: %s [options] [files]\n"
  "options:\n"
  "  -h show this message and exit\n"
  "  -i read from stdin (default if no files given)\n"
  "  -c cat s-expressions from files and/or stdin\n"
  ;

static FILE *try_open(char *nom) {
  FILE *file = fopen(nom, "r");
  if (!file) fprintf(stderr, "# error opening %s: %s\n", nom, strerror(errno));
  return file; }

static gwen_status gwen_run_file(gwen_core f, gwen_file in) {
  gwen_status s;
  // evaluate expressions for side effects
  while ((s = gwen_read1f(f, in)) == Ok && (s = gwen_eval(f)) == Ok)
    gwen_pop1(f);
  return s == Eof ? Ok : s; }

static gwen_status gwen_repl(gwen_core f, gwen_file in, gwen_file out) {
  for (gwen_status t; (t = gwen_read1f(f, in)) != Eof;)
    if (t == Ok && (t = gwen_eval(f)) == Ok)
      gwen_write1f(f, out),
      puts(""),
      gwen_pop1(f);
  return Ok; }

static gwen_status gwen_run(gwen_core f, char **av, bool usestdin) {
  gwen_status s = Ok;
  for (; s == Ok && *av; av++) {
    FILE *file = try_open(*av);
    if (!file) return Eof;
    s = gwen_run_file(f, file);
    fclose(file); }
  if (s != Ok || !usestdin) return s;
  if (isatty(STDIN_FILENO)) return gwen_repl(f, stdin, stdout);
  return gwen_run_file(f, stdin); }

int main(int ac, char **av) {
  bool usestdin = false, cat = false;
  // read command line flags
  for (;;) switch (getopt(ac, av, "chi")) {
    default: return EXIT_FAILURE;
    case 'c': cat = true; continue;
    case 'h': fprintf(stdout, help, *av); return EXIT_SUCCESS;
    case 'i': usestdin = true; continue;
    case -1: goto out; } out:
  av += optind;
  usestdin = usestdin || ac == optind;
  gwen_core f = gwen_open();
  gwen_status s = f ? (cat ? expcat : gwen_run)(f, av, usestdin) : Oom;
  gwen_close(f);
  return s; }
