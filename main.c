#include "lips.h"
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

static int repl(rt v, FILE *i, FILE *o) {
  obj x;
  for (setjmp(v->restart);;)
    if ((x = parse(v, i))) emsep(v, eval(v, x), o, '\n');
    else if (feof(i)) break;
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

static int seekpl(const char *p) {
#define USR_PATH ".local/lib/"NOM"/"
  const char *h = getenv("HOME");
  if (!h) return -1;
  int a = open(h, O_RDONLY);
  if (a == -1) return a;
  int b = openat(a, p, O_RDONLY);
  return close(a), b; }

static int seekp(const char *p) {
#define SYS_PATH "/usr/lib/"NOM"/"
  int a = seekpl(p);
  if (-1 < a) return a;
  a = open(SYS_PATH, O_RDONLY);
  if (-1 == a) return a;
  int b = openat(a, p, O_RDONLY);
  return close(a), b; }

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
args:
  switch (opt = getopt(argc, argv, opts)) {
    case -1: break;
    case '?': return EXIT_FAILURE;
    case 'p': i = 1; goto args;
    case 'v': puts(VN); return EXIT_SUCCESS;
    case 'h': fputs(usage, stdout); return EXIT_SUCCESS; }

  vm v = initialize(NULL);
  if (v == NULL) return EXIT_FAILURE;

  if (argv[optind] == NULL ||
      ((r = scripts(v, argv+optind)) == EXIT_SUCCESS && i))
    r = repl(v, stdin, stdout);
  return finalize(v), r; }
