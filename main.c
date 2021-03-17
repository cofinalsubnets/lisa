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
    if (setjmp(v->restart)) return errp(v, 0, "[%s] fail", q),
                                   fclose(f),
                                   NO;
    scr(v, f);
    int ok = feof(f);
    fclose(f);
    if (!ok) return NO; }
  return OK; }

int main(int argc, char**argv) {
  vm v;
#define takka 1
#define nprel 2
  int opt, args,
      F = argc == 1 ? takka : 0; // as in you get a
  const char
    opts[] = "hi_",
    help[] =
      "usage: %s [options and scripts]\n"
      "options:\n"
      "  -_ don't bootstrap\n"
      "  -i interact unconditionally\n"
      "  -h print this message\n";
  while ((opt = getopt(argc, argv, opts)) != -1) switch (opt) {
    case '?': return NO;
    case '_': F|=nprel; break;
    case 'i': F|=takka; break;
    case 'h': fprintf(stdout, help, argv[0]); break; }

  args = argc - optind;
  if (args == 0 && !F&takka) return OK;

  v = initialize();
  v = F&nprel ? v : bootstrap(v);
  if (!v) return NO;

  int r = OK;
  if (args) r = scripts(v, argv + optind);
  if (r == OK && F&takka) repl(v, stdin, stdout);
  return finalize(v), r; }
