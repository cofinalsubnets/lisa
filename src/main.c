#include "lips.h"
#include "terp.h"
#include "read.h"
#include "write.h"
#include "tbl.h"
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

#ifndef PREFIX
#define PREFIX "/usr/local"
#endif
#define OK EXIT_SUCCESS
#define NO EXIT_FAILURE

static int script(lips v, const char *path) {
 FILE *f = fopen(path, "r");
 if (!f) return errp(v, "%s : %s", path, strerror(errno)), NO;
 obj x;
 if (setjmp(v->restart)) return errp(v, "%s : fail", path), fclose(f), NO;
 while ((x = parse(v, f))) eval(v, x);
 return x = feof(f) ? (x ? x : nil) : NO, fclose(f), OK; }

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
 const u64 ini_len = 1;
 v->seed = LCPRNG(v->t0 = clock());
 v->ip = v->xp = v->syms = nil;
 v->fp = v->hp = v->sp = (mem) (W * ini_len),
 v->count = 0, v->len = ini_len;
 v->pool = (mem) (v->root = NULL);
 set64(v->glob, nil, NGlobs);
 Top = table(v);
 Mac = table(v);
#define repr(a, b) if (b) defprim(v,b,a);
#define rein(a, b) if (!b) rin(v, "i-"#a,a);
 insts(repr)
 insts(rein)
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
 def("_ns", Top);
 def("_macros", Mac);
 return v; }

#define BOOT PREFIX "/lib/lips/prelude.lips"

int main(int argc, char** argv) {
  for (int r = OK, shell = argc == 1 , boot = true;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return EXIT_FAILURE;
      case '_': boot = false; break;
      case 'i': shell = true; break;
      case 'h': {
        const char *help =
          "usage: %s [options and scripts]\n"
          "with no arguments, start a repl\n"
          "options:\n"
          "  -h print this message\n"
          "  -i start repl unconditionally\n"
          "  -_ don't bootstrap\n";
        fprintf(stdout, help, *argv);
        break; }
      case -1: {
        obj x;
        argc -= optind, argv += optind;
        if (argc || shell) {
          lips v = lips_init(&((struct lips){}));
          if (boot) r = script(v, BOOT);
          while (r == OK && argc--) {
            const char *path = *argv++;
            r = script(v, path); }
          if (r == OK && shell)
            for (setjmp(v->restart);;) {
              if ((x = parse(v, stdin)))
                emsep(v, eval(v, x), stdout, '\n');
              else if (feof(stdin)) break; }
          lips_fin(v); }
        return r; } } }
