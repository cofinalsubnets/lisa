#include "lips.h"
#define OK EXIT_SUCCESS
#define NO EXIT_FAILURE

static int repl(rt v, FILE *i, FILE *o) {
  obj x;
  for (setjmp(v->restart);;)
    if ((x = parse(v, i))) emsep(v, eval(v, x), o, '\n');
    else if (feof(i)) break;
  return OK; }

void scr(vm v, FILE *f) {
  for (obj x; (x = parse(v, f)); eval(v, x)); }

static int scripts(rt v, char**argv) {
  for (char *q; (q = *argv++);) {
    FILE *f = fopen(q, "r");
    if (!f) return errp(v, 0, "[%s] %s", q, strerror(errno)), NO;
    if (setjmp(v->restart)) return errp(v, 0, "[%s] fail"),
                                   fclose(f),
                                   NO;
    scr(v, f);
    int ok = feof(f);
    fclose(f);
    if (!ok) return NO; }
  return OK; }

int main(int argc, char**argv) {
  vm v;
  int r = OK, i = argc == 1 ? 1 : 0, opt, args;
  const char
    opts[] = "hrv",
    help[] =
      "usage: %s [options and scripts]\n"
      " with scripts, run them and exit unless -r is given\n"
      " with no scripts, start a repl\n"
      "options:\n"
      " -r  start repl unconditionally\n"
      " -h  print this message\n"
      " -v  print version\n";
args:
  switch (opt = getopt(argc, argv, opts)) {
    case -1: break;
    case '?': return NO;
    case 'r': i = 1; goto args;
    case 'v': fprintf(stdout, "%s %s\n",argv[0],VN); goto args;
    case 'h': fprintf(stdout, help, argv[0]); goto args; }

  args = argc - optind;
  if (args == 0 && !i) return OK;
  if (!(v = initialize())) return NO;

  if (args) r = scripts(v, argv + optind);
  if (r == OK && i) repl(v, stdin, stdout);
  return finalize(v), r; }
