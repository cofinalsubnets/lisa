#include "lips.h"
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#define OK EXIT_SUCCESS
#define NO EXIT_FAILURE

static int repl(lips v, FILE *i, FILE *o) {
 obj x;
 for (setjmp(v->restart);;)
  if ((x = parse(v, i))) emsep(v, eval(v, x), o, '\n');
  else if (feof(i)) break;
 return OK; }

static int scripts(lips v, char** argv) {
 for (char *q; (q = *argv++);) {
  FILE *f = fopen(q, "r");
  if (!f) return errp(v, "%s : %s", q, strerror(errno)), NO;
  if (setjmp(v->restart)) return
   errp(v, "%s : fail", q),
   fclose(f),
   NO;
  script(v, f);
  int ok = feof(f);
  fclose(f);
  if (!ok) return NO; }
 return OK; }

#define takka 1
#define aubas 2
#define help \
 "usage: %s [options and scripts]\n"\
 "with no arguments, start a repl\n"\
 "options:\n"\
 " -_ don't bootstrap\n"\
 " -i start repl\n"\
 " -h print this message\n"
int main(int argc, char** argv) {
 int opt, args, flag = argc == 1 ? takka : 0, r = OK;

 while ((opt = getopt(argc, argv, "hi_")) != -1) switch (opt) {
  case '_': flag |= aubas; break;
  case 'i': flag |= takka; break;
  case 'h': fprintf(stdout, help, argv[0]); break;
  default: return NO; }

 args = argc - optind;
 if (args == 0 && !(flag & takka)) return r;

 struct lips V;
 lips_init(&V);
 if (!(flag & aubas)) lips_boot(&V);

 if (args) r = scripts(&V, argv + optind);
 if (r == OK && flag & takka) r = repl(&V, stdin, stdout);
 return lips_fin(&V), r; }
