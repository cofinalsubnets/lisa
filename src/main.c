#include "lips.h"
#include "terp.h"
#include "read.h"
#include "hom.h"
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <string.h>
#include <errno.h>
#include "sym.h"
#include "tbl.h"
#include "mem.h"
#include "two.h"
#include "write.h"

#undef ok
#ifndef PREFIX
#define PREFIX "/usr/local"
#endif
#define BOOT PREFIX "/lib/lips/prelude.lips"

lips li_ini(void);
u0 li_fin(lips);
static u1 script(lips, const char *);
static u0 repl(lips);
static u1 scris(lips, const char*, const char **);

const char *help =
  "usage: %s [options and scripts]\n"
  "with no arguments, start a repl\n"
  "options:\n"
  "  -h print this message\n"
  "  -i start repl unconditionally\n"
  "  -_ don't bootstrap\n";

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
        lips v = li_ini();
        if ((ok = !!v)) {
          ok = scris(v, boot ? BOOT : NULL, (const char**) argv);
          if (ok && shell) repl(v);
          li_fin(v); }
        return ok ? EXIT_SUCCESS : EXIT_FAILURE; } }

static obj scr_(lips v, FILE *in) {
  obj y, x = parse(v, in);
  if (!x) return feof(in) ? nil : 0;
  // lol
  x = pair(v, x, nil);
  x = pair(v, Qt, x);
  x = pair(v, x, nil);
  x = pair(v, Eva, x);
  with(x, y = scr_(v, in));
  if (!y) return y;
  return pair(v, x, y); }

static obj scrp(lips v, const char *path) {
  FILE *in = fopen(path, "r");
  if (!in) {
    errp(v, "%s : %s", path, strerror(errno));
    return 0; }

  if (setjmp(v->restart)) return fclose(in), 0;
  obj x = scr_(v, in);
  fclose(in);
  if (!x) return x;
  return pair(v, Se, x); }

static obj scrr(lips v, const char **paths) {
  const char *path = *paths;
  if (!path) return nil;
  obj x, y = scrr(v, paths+1);
  if (!y) return y;
  with(y, x = scrp(v, path));
  if (!x) return 0;
  return pair(v, x, y); }

static u1 scris(lips v, const char *boot, const char **paths) {
  obj y, x = scrr(v, paths);
  if (!x) return x;
  if (boot) {
    with(x, y = scrp(v, boot));
    if (!y) return false;
    x = pair(v, y, x); }
  return !!eval(v, pair(v, Se, x)); }

  /*
static u1 script(lips v, const char *path) {
  FILE *f;
  if (!(f = fopen(path, "r"))) {
    errp(v, "%s : %s", path, strerror(errno));
    return false; }

  if (setjmp(v->restart)) return false;

  for (obj x; (x = parse(v, f)); eval(v, x));

  u1 done = feof(f);
  return fclose(f), done; }*/

static u0 repl(lips v) {
  setjmp(v->restart);
  for (obj x;;) if ((x = parse(v, stdin)))
                  emsep(v, eval(v, x), stdout, '\n');
                else if (feof(stdin)) return; }

static NoInline u0 rin(lips v, const char *a, terp *b) {
  obj z = interns(v, a);
  tbl_set(v, Top, z, _N((i64) b)); }

static NoInline u0 defprim(lips v, const char *a, terp *i) {
  obj nom = pair(v, interns(v, a), nil);
  hom prim;
  with(nom, prim = cells(v, 4));
  prim[0] = i;
  prim[1] = (terp*) nom;
  prim[2] = NULL;
  prim[3] = (terp*) prim;
  tbl_set(v, Top, A(nom), _H(prim)); }

u0 li_fin(lips v) { if (v) { if (v->pool) free(v->pool);
                             free(v); } }

lips li_ini(void) {
  lips v;
  if (!(v = malloc(sizeof(struct lips)))) return v;
  v->t0 = clock();
  v->rand = LCPRNG(v->t0 * mix);
  v->count = 0;
  v->ip = v->xp = v->syms = nil;
  v->fp = v->hp = v->sp = (mem) W, v->len = 1;
  v->pool = (mem) (v->root = NULL);
  set64(v->glob, nil, NGlobs);

  if (setjmp(v->restart)) return li_fin(v), NULL;

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
