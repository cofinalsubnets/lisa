#include "lips.h"
#include "terp.h"
#include "io.h"
#include "tbl.h"
#include "err.h"
#include "hom.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>

#ifndef PREFIX
#define PREFIX "/usr/local"
#endif
#define OK EXIT_SUCCESS
#define NO EXIT_FAILURE

static Inline int xval(obj x) { return x ? OK : NO; }

static obj script(lips v, const char *path) {
 FILE *f = fopen(path, "r");
 if (!f) return errp(v, "%s : %s", path, strerror(errno)), 0;
 obj x;
 if (setjmp(v->restart)) return errp(v, "%s : fail", path), fclose(f), 0;
 while ((x = parse(v, f))) eval(v, x);
 return x = feof(f) ? (x ? x : nil) : 0, fclose(f), x; }

static lips lips_fin(lips v) { return
 free(v->pool), (lips) (v->pool = NULL); }

static u0 rin(lips v, const char *a, terp *b) {
 obj z = interns(v, a);
 tbl_set(v, Top, z, _N(b)); }

static NoInline u0 defprim(lips v, const char *a, terp *inst) {
  hom prim;
  obj nom = pair(v, interns(v, a), nil);
  with(nom, prim = cells(v, 4));
  prim[0] = inst;
  prim[1] = (terp*) nom;
  prim[2] = NULL;
  prim[3] = (terp*) prim;
  tbl_set(v, Top, A(nom), _H(prim)); }

static lips lips_init(lips v) {
 const num ini_len = 1;
 v->seed = LCPRNG(v->t0 = clock());
 v->ip = v->xp = v->syms = nil;
 v->fp = v->hp = v->sp = (mem) (W * ini_len),
 v->count = 0, v->len = ini_len;
 v->pool = (mem) (v->root = NULL);
 set64(v->glob, nil, NGlobs);
 Top = table(v), Mac = table(v);
#define repr(a, b) if (b) defprim(v,b,a);
#define rein(a, b) if (!b) rin(v, "i-"#a,a);
 insts(repr) insts(rein)
#define bsym(i,s)(v->glob[i]=interns(v,s))
 bsym(Eval, "ev");
 bsym(Apply, "ap");
 bsym(Def, ":");
 bsym(Cond, "?");
 bsym(Lamb, "\\");
 bsym(Quote, "`");
 bsym(Seq, ",");
 bsym(Splat, ".");
 obj y;
#define def(s, x) (y=interns(v,s),tbl_set(v,Top,y,x))
 def("ns", Top), def("macros", Mac);
 return v; }

static u0 repl(lips v) {
 setjmp(v->restart);
 for (obj x;;)
  if ((x = parse(v, stdin)))
    emsep(v, eval(v, x), stdout, '\n');
  else if (feof(stdin)) return; }

#define BOOT PREFIX "/lib/lips/prelude.lips"
#define HELP \
  "usage: %s [options and scripts]\n"\
  "with no arguments, start a repl\n"\
  "options:\n"\
  "  -h print this message\n"\
  "  -i start repl unconditionally\n"\
  "  -_ don't bootstrap\n"

int main(int argc, char** argv) {
  for (int r = OK, shell = argc == 1 , boot = true;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return EXIT_FAILURE;
      case '_': boot = false; break;
      case 'i': shell = true; break;
      case 'h': fprintf(stdout, HELP, *argv); break;
      case -1: {
        argc -= optind, argv += optind;
        if (argc || shell) {
          lips v = lips_init(&((struct lips){}));
          if (boot) r = xval(script(v, BOOT));
          while (r == OK && argc--) {
            const char *path = *argv++;
            r = xval(script(v, path)); }
          if (r == OK && shell) repl(v);
          lips_fin(v); }
        return r; } } }
