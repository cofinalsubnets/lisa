#include "em.h"
#include <time.h>
#include <stdlib.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <string.h>
#include <errno.h>

static em ini(void);
static void fin(struct em*);
#ifndef PREF
#define PREF
#endif
#ifndef LANG
#define LANG
#endif
#ifndef SUFF
#define SUFF
#endif

static em from(bool, const char*, const char**);
static NoInline ob go(em v) {
  yo ip; ob *fp; ob xp, *sp, *hp;
  return Unpack(), ApY(ip, xp); }

int main(int argc, char **argv) {
  const char *boot = PREF "/lib/" LANG "/" LANG "." SUFF,
    *help =
      "usage: %s [options and scripts]\n"
      "with no arguments, start a repl\n"
      "options:\n"
      "  -h print this message\n"
      "  -i start repl unconditionally\n"
      "  -_ don't bootstrap\n"
      ;
  for (bool shell = argc == 1;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return EXIT_FAILURE;
      case 'h': fprintf(stdout, help, *argv); continue;
      case 'i': shell = true; continue;
      case '_': boot = NULL; continue;
      case -1:
        if (argc == optind && !shell) return EXIT_SUCCESS;
        em v = from(shell, boot, (const char **) argv + optind);
        return v && go(v) ? EXIT_SUCCESS : EXIT_FAILURE; } }

static ob seq(em v, ob a, ob b) {
  yo k; with(a, with(b, k = cells(v, 8)));
  return !k ? 0 : (ob)
    (k[0].ll = imm,  k[1].ll = (ll*) a,
     k[2].ll = call, k[3].ll = (ll*) N0,
     k[4].ll = jump, k[5].ll = (ll*) b,
     k[6].ll = NULL, k[7].ll = (ll*) k); }

static Vm(fin_ok) { return fin(v), nil; }
static Vm(repl) { for (Pack();;) {
  if ((xp = parse(v, stdin))) {
    if ((xp = eval(v, xp))) 
      emit(v, xp, stdout), fputc('\n', stdout); }
  else if (feof(stdin)) return fin(v), nil; } }

static ob scrp(em, const char*);
static yo comp(em v, bool shell, const char **paths) {
  const char *path = *paths;
  yo k; ob x, y; return
    !path ? !(k = cells(v, 3)) ? 0 :
               (k[0].ll = shell ? repl : fin_ok,
                k[1].ll = 0, k[2].ll = (ll*) k, k) :
    (y = (ob) comp(v, shell, paths+1)) &&
    (with(y, x = scrp(v, path)), !x) ? 0 :
    (yo) seq(v, x, y); }


static em from
  (bool shell, const char *boot, const char **paths) {
    em v = ini(); return 
      (v->ip = (yo) comp(v, shell, paths)) &&
      (!boot ||
       ((v->xp = scrp(v, boot)) &&
        (v->ip = (yo) seq(v, v->xp, (ob) v->ip)))) ? v : 0; }

// functions to compile scripts into a program
//
// scrpr : two em stream
static ob scrpr(em v, FILE *in) {
  ob y, x = parse(v, in);
  return !x ? feof(in) ? nil : 0 :
    !(x = pair(v, x, nil)) ||
    !(x = pair(v, v->glob[Quote], x)) ||
    !(x = pair(v, x, nil)) ||
    !(x = pair(v, v->glob[Eval], x)) ||
    !(with(x, y = scrpr(v, in)), y) ? 0 : pair(v, x, y); }

// scrp : yo em str
static Inline ob scrp(em v, const char *path) {
  ob x; FILE *in = fopen(path, "r");
  return !in ? err(v, "%s : %s", path, strerror(errno)) :
    !(x = scrpr(v, in), fclose(in), x) ||
    !(x = pair(v, v->glob[Seq], x)) ? 0 : (ob) ana(v, x); }

static void fin(em v) { if (v) free(v->pool), free(v); }
// initialization helpers
#define register_inst(a, b) ((b)?prim(v,b,a):inst(v, "i-"#a,a)) &&
#define def(s, x) (_=interns(v,s)) && tbl_set(v,v->glob[Topl],_,x)
#define bsym(i,s) (v->glob[i]=interns(v,s))
static NoInline bool inst(em v, const char *a, ll *b) {
  ob z; return !(z = interns(v, a)) ? 0 :
    !!tbl_set(v, v->glob[Topl], z, putnum((ob) b)); }

static NoInline bool prim(em v, const char *a, ll *i) {
  ob nom; yo k; return
    (nom = interns(v, a)) &&
    (nom = pair(v, nom, nil)) &&
    (with(nom, k = cells(v, 4)), k) ?
      !!tbl_set(v, v->glob[Topl], A(nom), (ob)
        (k[0].ll = i,    k[1].ll = (ll*) nom,
         k[2].ll = NULL, k[3].ll = (ll*) k)) : 0; }

static em ini(void) {
  ob _; em v = malloc(sizeof(struct em));
  return !v ? 0 :
    (v->t0 = clock(),
     v->rand = lcprng(v->t0),
     // initial memory state
     v->len = 1, v->pool = NULL, v->mm = NULL,
     v->fp = (void*) (v->hp = v->sp = (void*) sizeof(ob)),
     v->ip = (yo) (v->xp = v->syms = nil),
     setw(v->glob, nil, NGlobs),

     (v->glob[Topl] = table(v)) &&
     (v->glob[Macs] = table(v)) && insts(register_inst) // &&

     bsym(Eval, "ev") && bsym(Apply, "ap") &&
     bsym(Def, ":") && bsym(Cond, "?") &&
     bsym(Lamb, "\\") && bsym(Quote, "`") &&
     bsym(Seq, ",") && bsym(Splat, ".") &&

     def("_ns", v->glob[Topl]) &&
     def("_macros", v->glob[Macs]) ? v : (fin(v), NULL)); }
