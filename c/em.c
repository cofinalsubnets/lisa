#include "em.h"
#include <time.h>
#include <stdlib.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <string.h>
#include <errno.h>

// initialization helpers
static NoInline bool inst(em v, const char *a, ll *b) {
  ob z;
  bind(z, interns(v, a));
  return !!tbl_set(v, v->glob[Topl], z, putnum((ob) b)); }

static NoInline bool prim(em v, const char *a, ll *i) {
  ob nom;
  yo prim;
  bind(nom, pair(v, interns(v, a), nil));
  with(nom, prim = cells(v, 4));
  bind(prim, prim);
  prim[0].ll = i;
  prim[1].ll = (ll*) nom;
  prim[2].ll = NULL;
  prim[3].ll = (ll*) prim;
  return !!tbl_set(v, v->glob[Topl], A(nom), (ob) prim); }

// lips destructor
void fin(em v) { if (v) free(v->pool), free(v); }

// lips constructor
em ini(void) {
  em v;
  ob _;
  bind(v, malloc(sizeof(struct em)));

  v->t0 = clock();
  v->rand = lcprng(v->t0);
  v->len = 1;
  v->pool = NULL;
  v->mm = NULL;
  v->hp = v->sp = (ob*)sizeof(ob);
  v->fp = (fr) sizeof(ob);
  v->xp = v->syms = nil;
  v->ip = (yo) nil;
  setptr(v->glob, nil, NGlobs);

#define Bind(x) if(!(x))goto fail
  Bind(v->glob[Topl] = table(v));
  Bind(v->glob[Macs] = table(v));
#define register_inst(a, b)if(b){Bind(prim(v,b,a));}else{Bind(inst(v, "i-"#a,a));}
  insts(register_inst)
#define bsym(i,s) Bind(v->glob[i]=interns(v,s))
  bsym(Eval, "ev");
  bsym(Apply, "ap");
  bsym(Def, ":");
  bsym(Cond, "?");
  bsym(Lamb, "\\");
  bsym(Quote, "`");
  bsym(Seq, ",");
  bsym(Splat, ".");
#define def(s, x) Bind(_=interns(v,s));Bind(tbl_set(v,v->glob[Topl],_,x))
  def("_ns", v->glob[Topl]);
  def("_macros", v->glob[Macs]);
  return v; fail:
  return fin(v), NULL; }

#ifndef PREFIX
#define PREFIX
#endif
#ifndef NOM
#define NOM
#endif
#ifndef SUFF
#define SUFF
#endif

#define BOOT PREFIX "/lib/" NOM "/" NOM "." SUFF
static const char *help =
  "usage: %s [options and scripts]\n"
  "with no arguments, start a repl\n"
  "options:\n"
  "  -h print this message\n"
  "  -i start repl unconditionally\n"
  "  -_ don't bootstrap\n";

static ob scrp(em, const char*), scrr(em, bool, const char**);
static em init(bool, const char*, char **);

// unpack state & jump into thread
static NoInline ob go(em v) {
  yo ip;
  ob xp, *sp, *hp, *fp;
  Unpack();
  return ApY(ip, xp); }

int main(int argc, char **argv) {
  for (bool shell = argc == 1, boot = true;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return EXIT_FAILURE;
      case '_': boot = false; continue;
      case 'i': shell = true; continue;
      case 'h': fprintf(stdout, help, *argv); continue;
      case -1:
        if (argc == optind && !shell) return EXIT_SUCCESS;
        em v = init(shell, boot ? BOOT : NULL,  argv + optind);
        return v && go(v) ? EXIT_SUCCESS : EXIT_FAILURE; } }

// init : mo bool string? strings
static em init(bool shell, const char *boot, char **paths) {
  em v;
  ob y, x;
  bind(v, ini());
  bind(x, scrr(v, shell, (const char **) paths));
  if (boot) {
    with(x, y = scrp(v, boot));
    bind(y, y);
    bind(x, sequence(v, y, x)); }
  v->ip = (yo) x;
  return v; }

// vm functions to yield from the main thread
//
// fin_ok : nil lips
static Vm(fin_ok) { return fin(v), nil; }
// repl : nil lips
static Vm(repl) {
  for (Pack();;) {
    if ((xp = parse(v, stdin))) {
      if ((xp = eval(v, xp))) emsep(v, xp, stdout, '\n'); }
    else if (feof(stdin)) return fin(v), nil; } }

// functions to compile scripts into a program
//
// scr_ : two em stream
static ob scr_(em v, FILE *in) {
  ob y, x = parse(v, in);
  if (!x) return feof(in) ? nil : 0;
  bind(x, pair(v, x, nil));
  bind(x, pair(v, v->glob[Quote], x));
  bind(x, pair(v, x, nil));
  bind(x, pair(v, v->glob[Eval], x));
  with(x, y = scr_(v, in));
  bind(y, y);
  return pair(v, x, y); }

// scrp : yo em str
static ob scrp(em v, const char *path) {
  FILE *in = fopen(path, "r");
  if (!in) return err(v, "%s : %s", path, strerror(errno));
  ob x = scr_(v, in);
  fclose(in);
  bind(x, x);
  bind(x, pair(v, v->glob[Seq], x));
  return analyze(v, x); }

// scrr : yo em u1 strs
static ob scrr(em v, bool shell, const char **paths) {
  yo k;
  ob x, y;
  const char *path = *paths;
  if (!path) {
    bind(k, cells(v, 3));
    k[0].ll = shell ? repl : fin_ok;
    k[1].ll = NULL;
    k[2].ll = (ll*) k;
    return (ob) k; }
  bind(y, scrr(v, shell, paths+1));
  with(y, x = scrp(v, path));
  bind(x, x);
  return sequence(v, x, y); }
