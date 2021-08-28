#include "lips.h"
#include <unistd.h>

static int repl(lips v) {
 jmp_buf re;
 v->restart = &re;
 setjmp(re);
 for (obj x;;)
  if ((x = parse(v, stdin)))
    emsep(v, eval(v, x), stdout, '\n');
  else if (feof(stdin)) break;
 return OK; }

#define takka 1
#define aubas 2
#define help \
 "usage: %s [options and scripts]\n"\
 "with no arguments, start a repl\n"\
 "options:\n"\
 " -h print this message\n"\
 " -i start repl unconditionally\n"\
 " -_ don't bootstrap\n"

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
  if (!(flag & aubas))
    r = lips_boot(&V);
  while (r == OK && optind < argc) {
    const char *path = argv[optind++];
    r = xval(script(&V, path, fopen(path, "r"))); }
  if (r == OK && flag & takka)
    r = repl(&V);
  lips_fin(&V); }
 return r; }
