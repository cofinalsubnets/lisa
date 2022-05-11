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
  yo ip; fr fp; ob xp, *sp, *hp;
  return Unpack(), ApY(ip, xp); }

int main(int argc, char **argv) {
  const char *boot = PREF "/lib/" LANG "/" LANG "." SUFF, *help =
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
  yo k; return
    with(a, with(b, k = cells(v, 8))), !k ? 0 : (ob)
      (k[0].ll = imm,  k[1].ll = (ll*) a,
       k[2].ll = call, k[3].ll = (ll*) N0,
       k[4].ll = jump, k[5].ll = (ll*) b,
       k[6].ll = NULL, k[7].ll = (ll*) k); }

static Ll(yield) { return Pack(), xp; }
static ob apply(em v, ob f, ob x) {
  yo k; return !Push(f, x) || !(k = cells(v, 5)) ? 0 :
    (k[0].ll = call,
     k[1].ll = (ll*) putnum(2),
     k[2].ll = yield,
     k[3].ll = NULL,
     k[4].ll = (ll*) k,
     x = tbl_get(v, v->glob[Topl], v->glob[Apply]),
     call(v, x, k, v->hp, v->sp, v->fp)); }

ob eval(em v, ob x) {
  ob args = pair(v, x, nil);
  return !args? 0 :
    apply(v, tbl_get(v, v->glob[Topl], v->glob[Eval]), args); }

Ll(ev_u) {
  Arity(1); mo y;
  return
    xp = tbl_get(v, v->glob[Topl], v->glob[Eval]),
    gethom(xp)->ll != ev_u ?
      ApY((yo) xp, nil) :
      !(Pack(), y = ana(v, *fp->argv)) ? 0 :
        (Unpack(), ApY(y, xp)); }

static Vm(fin_ok) { return fin(v), nil; }
static Vm(repl) { for (Pack();;) {
  if ((xp = parse(v, stdin))) {
    if ((xp = eval(v, xp))) 
      emit(v, xp, stdout), fputc('\n', stdout); }
  else if (feof(stdin)) return fin(v), nil; } }

static ob scrp(em, const char*);
static yo comp(em v, bool shell, const char **paths) {
  const char *path = *paths; yo k; ob x, y; return
    !path ? !(k = cells(v, 3)) ? 0 :
               (k[0].ll = shell ? repl : fin_ok,
                k[1].ll = 0, k[2].ll = (ll*) k, k) :
    (y = (ob) comp(v, shell, paths+1)) &&
    (with(y, x = scrp(v, path)), x) ? (yo) seq(v, x, y) : 0; }

static em from
  (bool shell, const char *boot, const char **paths) {
    em v = ini(); ob y; return 
      !(y = (ob) comp(v, shell, paths)) ||
      !(!boot ||
        ((with(y, (v->xp = scrp(v, boot))), v->xp) &&
         (y = seq(v, v->xp, (ob) y)))) ? 0 :
      (v->ip = (mo) y,
       v); }

// functions to compile scripts into a program
//
// scrpr : two em stream
static ob scrpr(em v, FILE *in) {
  ob y, x = parse(v, in);
  return !x ? feof(in) ? nil : 0 :
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->glob[Quote], x)) &&
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->glob[Eval], x)) &&
    (with(x, y = scrpr(v, in)), y) ? pair(v, x, y) : 0; }

// scrp : yo em str
static Inline ob scrp(em v, const char *path) {
  ob x; FILE *in = fopen(path, "r");
  return !in ? err(v, 0, "%s : %s", path, strerror(errno)) :
    (x = scrpr(v, in), fclose(in), x) &&
    (x = pair(v, v->glob[Seq], x)) ? (ob) ana(v, x) : 0; }

static void fin(em v) { if (v) free(v->pool), free(v); }
// initialization helpers
#define register_inst(a, b) ((b) ? prim(v,b,a) : inst(v, "i-"#a,a)) &&
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
  return v &&
    (v->t0 = clock(),
     v->rand = lcprng(v->t0),
     // initial memory state
     v->len = 1, v->pool = NULL, v->mm = NULL,
     v->fp = (fr) (v->hp = v->sp = (ob*) sizeof(ob)),
     v->ip = (yo) (v->xp = v->syms = nil),
     setw(v->glob, nil, NGlobs),

     (v->glob[Topl] = table(v)) && insts(register_inst) // &&

     (v->glob[Eval] = interns(v, "ev")) &&
     (v->glob[Apply] = interns(v, "ap")) &&
     (v->glob[Def] = interns(v, ":")) &&
     (v->glob[Cond] = interns(v, "?")) &&
     (v->glob[Lamb] = interns(v, "\\")) &&
     (v->glob[Quote] = interns(v, "`")) &&
     (v->glob[Seq] = interns(v, ",")) &&
     (v->glob[Splat] = interns(v, ".")) &&
     (_ = interns(v, "_ns")) &&
     tbl_set(v, v->glob[Topl], _, v->glob[Topl]) &&
     (_ = (ob) cells(v, 3 + Width(fr))))
    ? (gethom(_)[0].ll = yield,
       gethom(_)[1].ll = 0,
       gethom(_)[2].ll = (vm*) _,
       v->fp = (fr) v->sp - 1,
       v->sp = (ob*) v->fp,
       v->fp->clos = v->fp->subd = N0,
       v->fp->argc = putnum(-Width(fr)),
       v->fp->retp = _,
       v) : (fin(v), NULL); }
