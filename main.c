#include "lips.h"
#include <errno.h>
#include <string.h>
#include <unistd.h>

static int repl(lips v) {
 obj x;
 jmp_buf re;
 for (v->restart = &re, setjmp(re);;)
  if ((x = parse(v, stdin))) emsep(v, eval(v, x), stdout, '\n');
  else if (feof(stdin)) break;
 return OK; }

static int run_script(lips v, char *path) {
  FILE *f = fopen(path, "r");
  if (!f) return
   errp(v, "%s : %s", path, strerror(errno)),
   NO;

  jmp_buf re;
  v->restart = &re;
  if (setjmp(re)) return
   errp(v, "%s : fail", path),
   fclose(f),
   NO;

  return script(v, f); }

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
 int opt, flag = argc == 1 ? takka : 0, r = OK;

 while ((opt = getopt(argc, argv, "hi_")) != -1) switch (opt) {
  case '_': flag |= aubas; break;
  case 'i': flag |= takka; break;
  case 'h': fprintf(stdout, help, argv[0]); break;
  default: return NO; }

 if (optind < argc || flag & takka) {
  struct lips V;
  lips_init(&V);
  if (!(flag & aubas)) r = lips_boot(&V);
  while (r == OK && optind < argc) r = run_script(&V, argv[optind++]);
  if (r == OK && flag & takka) r = repl(&V);
  lips_fin(&V); }
 return r; }
