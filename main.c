#include "lips.h"
#include <errno.h>
#include <unistd.h>

static int repl(rt v, FILE *i, FILE *o) {
  obj x;
  for (setjmp(v->restart); (x = parse(v, i));
       emsep(v, eval(v, x), o, '\n'));
  return EXIT_SUCCESS; }

static int scripts(rt v, char**argv) {
  obj x;
  FILE *f;
  for (const char *q; (q = *argv++);)
    if (setjmp(v->restart)) return
      errp(v, "main", 0, "%s : error", q),
      EXIT_FAILURE;
    else if (!(f = fopen(q, "r")))
      err(v, "main", 0, "%s : %s", q, strerror(errno));
    else { while ((x = parse(v, f))) eval(v, x);
           fclose(f); }
  return EXIT_SUCCESS; }

int main(int argc, char**argv) {
  int r, i = 0, opt;
  const char
    opts[] = "hpv",
    usage[] =
      "usage: " NOM " [options and files]\n"
      "with no files, start a repl.\n"
      "options:\n"
      "  -h   print this message and exit\n"
      "  -v   print version and exit\n"
      "  -p   start repl after loading files\n"
      ;
loop:
  switch (opt = getopt(argc, argv, opts)) {
    case -1: break;
    case '?': return EXIT_FAILURE;
    case 'p': i = 1; goto loop;
    case 'v': puts(VN); return EXIT_SUCCESS;
    case 'h': fputs(usage, stdout); return EXIT_SUCCESS; }


  vm v = initialize();
  if (v == NULL) return EXIT_FAILURE;
  if (argv[optind] == NULL ||
      ((r = scripts(v, argv+optind)) == EXIT_SUCCESS && i))
    r = repl(v, stdin, stdout);
  return finalize(v), r; }
