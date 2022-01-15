#include "lips.h"

static lips lips_fin(lips v) { return
 free(v->pool), (lips) (v->pool = NULL); }

#include "terp.h"
#include "read.h"
#include "hom.h"
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

Vm(fin) {
  free(v->pool);
  free(v);
  return 0; }

Vm(scrr) {
  const char *path = (const char *) H(ip)[1];
  FILE *f = fopen(path, "r");
  if (!f) return
    errp(v, "%s : %s", path, strerror(errno)),
    EXIT_FAILURE;
  if (setjmp(v->restart)) return EXIT_FAILURE;
  for (obj x; (x = parse(v, f)); eval(v, x));
  fclose(f);
  Next(2); }

static u1 script(lips v, const char *path) {
  FILE *f = fopen(path, "r");

  if (!f) return
    errp(v, "%s : %s", path, strerror(errno)),
    false;

  if (setjmp(v->restart)) return false;

  for (obj x; (x = parse(v, f)); eval(v, x));

  u1 r = feof(f);
  fclose(f);
  return r; }

#include "sym.h"
#include "tbl.h"
static NoInline u0 rin(lips v, const char *a, terp *b) {
  obj z = interns(v, a);
  tbl_set(v, Mac, z, _N((i64) b)); }

#include "mem.h"
#include "two.h"
static NoInline u0 defprim(lips v, const char *a, terp *i) {
  obj nom = pair(v, interns(v, a), nil);
  hom prim;
  with(nom, prim = cells(v, 4));
  prim[0] = i;
  prim[1] = (terp*) nom;
  prim[2] = NULL;
  prim[3] = (terp*) prim;
  tbl_set(v, Top, A(nom), _H(prim)); }

static lips lips_init(lips v) {
 v->t0 = clock();
 v->rand = LCPRNG(v->t0 * mix);
 v->count = 0;
 v->ip = v->xp = v->syms = nil;
 v->fp = v->hp = v->sp = (mem) W, v->len = 1;
 v->pool = (mem) (v->root = NULL);
 set64(v->glob, nil, NGlobs);

 if (setjmp(v->restart)) {
   errp(v, "fail");
   return NULL; }

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
const char *help =
  "usage: %s [options and scripts]\n"
  "with no arguments, start a repl\n"
  "options:\n"
  "  -h print this message\n"
  "  -i start repl unconditionally\n"
  "  -_ don't bootstrap\n";

#undef ok
#include "write.h"
int main(int argc, char** argv) {
  for (u1 ok = true, shell = argc == 1, boot = true;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return EXIT_FAILURE;
      case '_': boot = false; break;
      case 'i': shell = true; break;
      case 'h': fprintf(stdout, help, *argv); break;
      case -1:
        argc -= optind, argv += optind;
        if (!argc && !shell) return EXIT_SUCCESS;

        lips v = lips_init(&((struct lips){}));
        if (boot) ok = script(v, BOOT);

        while (ok && argc--) ok = script(v, *argv++);

        if (ok && shell)
          for (setjmp(v->restart);;) {
            obj x = parse(v, stdin);
            if (x) emsep(v, eval(v, x), stdout, '\n');
            else if (feof(stdin)) break; }

        lips_fin(v);
        return ok ? EXIT_SUCCESS : EXIT_FAILURE; } }
