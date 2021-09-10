#include "lips.h"
#include "terp.h"
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <errno.h>

#ifndef PREFIX
#define PREFIX "/usr/local"
#endif

#define OK EXIT_SUCCESS
#define NO EXIT_FAILURE
static Inline int xval(obj x) { return x ? OK : NO; }

static obj script(lips v, const char *path, FILE *f) {
 if (!f) return
  errp(v, "%s : %s", path, strerror(errno)), 0;
 jmp_buf re;
 v->restart = &re;
 if (setjmp(re)) return
  errp(v, "%s : fail", path),
  fclose(f),
  0;
 obj x;
 while ((x = parse(v, f))) eval(v, x);
 return x = feof(f) ? (x ? x : nil) : 0, fclose(f), x; }

#define PATH PREFIX "/lib/lips/prelude.lips"
static int lips_boot(lips v) { return xval(script(v, PATH, fopen(PATH, "r"))); }
#undef PATH

static lips lips_fin(lips v) { return
 free(v->mem_pool), (lips) (v->mem_pool = NULL); }

static NoInline u0 rin(lips v, const char *a, terp *b) {
 obj z = interns(v, a);
 tblset(v, Top, z, Pn(b)); }

static lips lips_init(lips v) {
 v->seed = v->t0 = clock(),
 v->ip = v->xp = v->syms = nil,
 v->fp = v->hp = v->sp = (mem) W,
 v->count = 0, v->mem_len = 1, v->mem_pool = NULL,
 v->mem_root = NULL;
 set64(v->glob, nil, NGlobs);
 jmp_buf re;
 v->restart = &re;
 if (setjmp(re)) return lips_fin(v);
 Top = table(v), Mac = table(v);
#define repr(a, b) if (b) defprim(v,b,a);
#define rein(a, b) if (!b) rin(v, "i-"#a,a);
 insts(repr) insts(rein)
#define bsym(i,s)(Glob[i]=interns(v,s))
 bsym(Eval, "ev"), bsym(Apply, "ap"),
 bsym(Def, ":"),   bsym(Cond, "?"), bsym(Lamb, "\\"),
 bsym(Quote, "`"), bsym(Seq, ","),  bsym(Splat, ".");
 obj y;
#define def(s, x) (y=interns(v,s),tblset(v,Top,y,x))
 def("ns", Top), def("macros", Mac);
 return v; }

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
