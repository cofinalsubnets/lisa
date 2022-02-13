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

static lips args(int, char**);
int main(int argc, char **argv) {
  lips v = args(argc, argv);
  if (!v) return EXIT_FAILURE;
  obj xp, ip;
  mem sp, hp, fp;
  Unpack();
  Next(0); }

const char *help =
  "usage: %s [options and scripts]\n"
  "with no arguments, start a repl\n"
  "options:\n"
  "  -h print this message\n"
  "  -i start repl unconditionally\n"
  "  -_ don't bootstrap\n";

lips li_ini(void);
u0 li_fin(lips);
static obj scri(lips, u1, const char*, const char **);
static lips args(int argc, char **argv) {
  for (u1 shell = argc == 1, boot = true;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return NULL;
      case '_': boot = false; break;
      case 'i': shell = true; break;
      case 'h': fprintf(stdout, help, *argv); break;
      case -1: {
        lips v = li_ini();
        if (!v || !(v->ip = scri(v, shell, boot ? BOOT : NULL, (const char**) argv + optind)))
          return li_fin(v), NULL;
        return v; } } }

static obj scr_(lips v, FILE *in) {
  obj y, x = parse(v, in);
  if (!x) return feof(in) ? nil : 0;
  // lol :(
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

  jmp_buf re;
  v->restart = &re;
  if (setjmp(re)) return v->restart = NULL, fclose(in), 0;
  obj x = scr_(v, in);
  v->restart = NULL, fclose(in);
  if (!x) return x;
  return analyze(v, pair(v, Se, x)); }

static Vm(li_exit);
static Vm(li_repl) {
  jmp_buf re;
  v->restart = &re;
  setjmp(re);
  for (obj x;;) if ((x = parse(v, stdin)))
                  emsep(v, eval(v, x), stdout, '\n');
                else if (feof(stdin)) Jump(li_exit); }

static Vm(li_exit) {
  li_fin(v);
  exit(EXIT_SUCCESS); }

static obj scrr(lips v, u1 shell, const char **paths) {
  const char *path = *paths;
  if (!path) {
    hom h = cells(v, 3);
    h[0] = shell ? li_repl : li_exit;
    h[1] = NULL;
    h[2] = (terp*) h;
    return _H(h); }
  obj x, y = scrr(v, shell, paths+1);
  if (!y) return y;
  with(y, x = scrp(v, path));
  if (!x) return 0;
  return sequence(v, x, y); }

static obj scri(lips v, u1 shell, const char *boot, const char **paths) {
  obj y, x = scrr(v, shell, paths);
  if (x && boot) {
    with(x, y = scrp(v, boot));
    if (!y) return y;
    x = sequence(v, y, x); }
  return x; }

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

  jmp_buf re;
  v->restart = &re;
  if (setjmp(re)) return li_fin(v), NULL;

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
  v->restart = NULL;
  return v; }
