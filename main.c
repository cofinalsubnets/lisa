#include "lips.h"
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <locale.h>
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
#define nprel 2
#define help \
   "usage: %s [options and scripts]\n"\
   "options:\n"\
   "  -_ don't bootstrap\n"\
   "  -i interact unconditionally\n"\
   "  -h print this message\n"
struct lips klips;
int main(int argc, char** argv) {
 int opt, args,
  F = argc == 1 ? takka : 0;

 setlocale(LC_ALL, "");
 while ((opt = getopt(argc, argv, "hi_")) != -1) switch (opt) {
  case '_': F|=nprel; break;
  case 'i': F|=takka; break;
  case 'h': fprintf(stdout, help, argv[0]); break;
  default: return NO; }

 args = argc - optind;
 if (args == 0 && !(F&takka)) return OK;

 lips v = &klips;
 lips_init(v);
 if (!(F&nprel)) lips_boot(v);

 // set up argv
 obj z, c = argc, a = nil;
 for (mm(&a); c--; z = string(v, argv[c]), a = pair(v, z, a));
 z = intern(v, string(v, "argv")), um;
 tblset(v, Top, z, a);

 int r = OK;
 if (args) r = scripts(v, argv + optind);
 if (r == OK && F&takka) repl(v, stdin, stdout);
 return lips_fin(v), r; }
