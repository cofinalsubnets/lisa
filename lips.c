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

static lips lips_fin(lips v) { return
 free(v->mem_pool), (lips) (v->mem_pool = NULL); }

static NoInline u0 rin(lips v, const char *a, terp *b) {
 obj z = interns(v, a);
 tblset(v, Top, z, Pn(b)); }

static lips lips_init(lips v) {
 v->t0 = clock();
 v->seed = LCPRNG(v->t0);
 v->ip = v->xp = v->syms = nil,
 v->fp = v->hp = v->sp = (mem) W,
 v->count = 0, v->mem_len = 1, v->mem_pool = NULL,
 v->mem_root = NULL;
 set64(v->glob, nil, NGlobs);
 jmp_buf re;
 v->restart = &re;
 if (setjmp(re)) return errp(v, "init : fail "),
                        lips_fin(v);
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

int repl(lips v, FILE *in, FILE *out) {
 jmp_buf re;
 v->restart = &re;
 setjmp(re);
 for (obj x;;) {
  if ((x = parse(v, in)))
   emsep(v, eval(v, x), out, '\n');
  else if (feof(in)) break; }
 return OK; }

#define BOOT PREFIX "/lib/lips/prelude.lips"
#define TAKKA 1
#define AUBAS 2
#define HELP \
 "usage: %s [options and scripts]\n"\
 "with no arguments, start a repl\n"\
 "options:\n"\
 " -h print this message\n"\
 " -i start repl unconditionally\n"\
 " -_ don't bootstrap\n"

int main(int argc, char** argv) {
 int opt, flag = argc == 1 ? TAKKA : 0, r = OK;

 while ((opt = getopt(argc, argv, "hi_")) != -1) switch (opt) {
  case '_': flag |= AUBAS; break;
  case 'i': flag |= TAKKA; break;
  case 'h': fprintf(stdout, HELP, *argv); break;
  default: return NO; }

 if (optind < argc || flag & TAKKA) {
  struct lips V;
  lips_init(&V);

  if (!(flag & AUBAS)) r = xval(script(&V, BOOT, fopen(BOOT, "r")));
  while (r == OK && optind < argc) {
    const char *path = argv[optind++];
    r = xval(script(&V, path, fopen(path, "r"))); }
  if (r == OK && flag & TAKKA) r = repl(&V, stdin, stdout);

  lips_fin(&V); }

 return r; }
