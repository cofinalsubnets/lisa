#include "em.h"
#include <time.h>
#include <stdlib.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <string.h>
#include <errno.h>

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
static int init(bool, const char*, const char **);

// unpack state & jump into thread
static NoInline ob go(em v) {
  yo ip; ob *fp; ob xp, *sp, *hp;
  return Unpack(), ApY(ip, xp); }

int main(int argc, char **argv) {
  const char *help =
    "usage: %s [options and scripts]\n"
    "with no arguments, start a repl\n"
    "options:\n"
    "  -h print this message\n"
    "  -i start repl unconditionally\n"
    "  -_ don't bootstrap\n";
  for (bool shell = argc == 1, boot = true;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return EXIT_FAILURE;
      case '_': boot = false; continue;
      case 'i': shell = true; continue;
      case 'h': fprintf(stdout, help, *argv); continue;
      case -1: return argc == optind && !shell ? EXIT_SUCCESS :
        init(shell, boot ? BOOT : NULL,  (const char **) (argv + optind)); } }

static ob scrp(em, const char*),
          compose_process(em, bool, const char**);
// init : mo u1 str? strs
static int init(bool shell, const char *boot, const char **paths) {
  em v; ob y, x; return 
    (v = ini()) &&
    // compose scripts and repl
    (x = compose_process(v, shell, (const char **) paths)) &&
    (!boot ||
     // pull back with boot script
     ((with(x, y = scrp(v, boot)), y) &&
      (x = sequence(v, y, x)))) &&
    // got a value?
    (v->ip = (yo) x, go(v)) ?
      EXIT_SUCCESS :
      EXIT_FAILURE; }

// vm functions to yield from the main thread
//
// fin_ok : nil lips
static Vm(fin_ok) { return fin(v), nil; }
// repl : nil lips
static Vm(repl) { for (Pack();;) {
  if ((xp = parse(v, stdin))) {
    if ((xp = eval(v, xp))) emsep(v, xp, stdout, '\n'); }
  else if (feof(stdin)) return fin(v), nil; } }

// functions to compile scripts into a program
//
// scr_ : two em stream
static ob scr_(em v, FILE *in) {
  ob y, x = parse(v, in);
  return !x ? feof(in) ? nil : 0 :
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->glob[Quote], x)) &&
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->glob[Eval], x)) &&
    (with(x, y = scr_(v, in)), y) ?
    pair(v, x, y) :
    0; }

// scrp : yo em str
static ob scrp(em v, const char *path) {
  ob x;
  FILE *in = fopen(path, "r");
  return !in ?  err(v, "%s : %s", path, strerror(errno)) :
    (x = scr_(v, in), fclose(in), x) &&
    (x = pair(v, v->glob[Seq], x)) ?
      analyze(v, x) :
      0; }

// compose_process : yo em u1 strs
static ob compose_process(em v, bool shell, const char **paths) {
  const char *path = *paths;
  yo k; ob x, y; return !path ?
    !(k = cells(v, 3)) ? 0 :
      (k[0].ll = shell ? repl : fin_ok,
       k[1].ll = NULL,
       k[2].ll = (ll*) k,
       (ob) k) :

    (y = compose_process(v, shell, paths+1)) &&
    (with(y, x = scrp(v, path)), x) ?
      sequence(v, x, y) :
      0; }

void fin(em v) { if (v) free(v->pool), free(v); }
// initialization helpers
#define register_inst(a, b) ((b)?prim(v,b,a):inst(v, "i-"#a,a)) &&
#define def(s, x) (_=interns(v,s)) && tbl_set(v,v->glob[Topl],_,x)
#define bsym(i,s) (v->glob[i]=interns(v,s))
static NoInline bool inst(em v, const char *a, ll *b) {
  ob z; return !(z = interns(v, a)) ? 0 :
    !!tbl_set(v, v->glob[Topl], z, putnum((ob) b)); }

static NoInline bool prim(em v, const char *a, ll *i) {
  ob nom; yo prim; return
    !(nom = interns(v, a)) ||
    !(nom = pair(v, nom, nil)) ||
    !(with(nom, prim = cells(v, 4)), prim) ? 0 :
      (prim[0].ll = i,
       prim[1].ll = (ll*) nom,
       prim[2].ll = NULL,
       prim[3].ll = (ll*) prim,
       !!tbl_set(v, v->glob[Topl], A(nom), (ob) prim)); }

em ini(void) {
  em v = malloc(sizeof(struct em));
  ob _; return !v ? 0 :
    (v->t0 = clock(),
     v->rand = lcprng(v->t0),
     v->len = 1,
     v->pool = NULL,
     v->mm = NULL,
     v->hp = v->sp = (ob*) sizeof(ob),
     v->fp = (fr) sizeof(ob),
     v->xp = v->syms = nil,
     v->ip = (yo) nil,
     setptr(v->glob, nil, NGlobs),
     (v->glob[Topl] = table(v)) &&
     (v->glob[Macs] = table(v)) &&
     insts(register_inst)
     bsym(Eval, "ev") &&
     bsym(Apply, "ap") &&
     bsym(Def, ":") &&
     bsym(Cond, "?") &&
     bsym(Lamb, "\\") &&
     bsym(Quote, "`") &&
     bsym(Seq, ",") &&
     bsym(Splat, ".") &&
     def("_ns", v->glob[Topl]) &&
     def("_macros", v->glob[Macs]) ? v : (fin(v), NULL)); }
