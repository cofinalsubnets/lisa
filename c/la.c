#include "la.h"
#include <time.h>
#include <stdlib.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <string.h>
#include <errno.h>

static em ini(void);
static void fin(struct la*);
#ifndef PREF
#define PREF
#endif
#ifndef LANG
#define LANG
#endif
#ifndef SUFF
#define SUFF
#endif

static ob seq(em, ob, ob);
static mo comp(em, bool, const char**);
static ob eval(em, ob);

int main(int argc, char **argv) {

  const char
    *boot = PREF "/lib/" LANG "/" LANG "." SUFF,
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
        em v; ob r =

          (v = ini()) &&

          (v->ip =
           comp(v, shell, (const char **) argv + optind)) &&

          (!boot ||
           ((v->xp = scrp(v, boot)) &&
            (v->ip = (mo)
             seq(v, v->xp, (ob) v->ip)))) &&

          (v->xp = pair(v, (ob) v->ip, nil)) ?

          eval(v, v->xp) : 0;

        return fin(v), r ?
         EXIT_SUCCESS : EXIT_FAILURE; } }



// takes two threads, gives a thread that runs the first
// thread then the second thread
static ob seq(em v, ob a, ob b) {
  mo k; return
    with(a, with(b, k = cells(v, 8))), !k ? 0 : (ob)
      (k[0].ll = imm,  k[1].ll = (ll*) a,
       k[2].ll = call, k[3].ll = (ll*) N0,
       k[4].ll = jump, k[5].ll = (ll*) b,
       k[6].ll = NULL, k[7].ll = (ll*) k); }

Inline ob cwm(em v) {
  return A(v->glob[Topl]); }

ob lookup(em v, ob _) {
  ob x, mod = v->glob[Topl];
  for (; twop(mod); mod = B(mod))
    if ((x = tbl_get(v, A(mod), _)))
      return x;
  return 0; }

static ob eval(em, ob);

// bootstrap eval interpreter function
Ll(ev_u) {
  Arity(1); mo y;
  return
    // check to see if ev has been overridden in the
    // toplevel namespace and if so call that. this way
    // ev calls compiled pre-bootstrap will use the
    // bootstrapped compiler, which is what we want?
    // seems kind of strange to need this ...
    xp = lookup(v, v->glob[Eval]),
    xp && homp(xp) && gethom(xp)->ll != ev_u ?
      ApY((mo) xp, nil) :
      // otherwise use the bootstrap compiler.
      !(Pack(), y = ana(v, *fp->argv)) ? 0 :
        (Unpack(), ApY(y, xp)); }

// called after running all scripts successfully
static Ll(yield) { return Pack(), xp; }
static Vm(repl) { for (Pack();;) {
  if ((xp = parse(v, stdin))) {
    if ((xp = eval(v, xp))) 
      emit(v, xp, stdout), fputc('\n', stdout); }
  else if (feof(stdin)) return nil; } }

// takes scripts and if we want a repl, gives a thread
static mo comp(em v, bool shell, const char **paths) {
  const char *path = *paths; mo k; ob x, y; return
    !path ? !(k = cells(v, 3)) ? 0 :
               (k[0].ll = shell ? repl : yield,
                k[1].ll = 0, k[2].ll = (ll*) k, k) :
    (y = (ob) comp(v, shell, paths+1)) &&
    (with(y, x = scrp(v, path)), x) ? (mo) seq(v, x, y) : 0; }

static ob scrpr(em, FILE*);

// scrp : mo em str
// produce a hom from a file by interpreting the contents
// as a list action on the phase space.
ob scrp(em v, const char *path) {
  ob x; FILE *in = fopen(path, "r");
  return !in ? err(v, 0, "%s : %s", path, strerror(errno)) :
    (x = scrpr(v, in), fclose(in), x) &&
    (x = pair(v, v->glob[Seq], x)) ? (ob) ana(v, x) : 0; }

// scrpr : two em stream
// read all expressions from a stream and collect
// them into a list for sequencing. because right
// now we don't have good (any) shadowing rules,
// we quote  each expression and eval them in sequence.
static ob scrpr(em v, FILE *in) {
  ob y, x = parse(v, in);
  return !x ? feof(in) ? nil : 0 :
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->glob[Quote], x)) &&
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->glob[Eval], x)) &&
    (with(x, y = scrpr(v, in)), y) ? pair(v, x, y) : 0; }

static ob eval(em v, ob x) {
  mo k; return
    !(Push(x) && (k = cells(v, 6))) ? 0 :
    (k[0].ll = call,
     k[1].ll = (ll*) putnum(1),
     k[2].ll = yield,
     k[3].ll = ev_u,
     k[4].ll = NULL,
     k[5].ll = (ll*) k,
     call(v, (ob) (k + 3), k, v->hp, v->sp, v->fp)); }

// initialization helpers
static NoInline bool
  inst(em, const char *, ll *),
  prim(em, const char *, ll *);

// finalize a vm. 
static void fin(em v) {
  if (v) free(v->pool), free(v); }

static em ini(void) {
  ob _; em v = malloc(sizeof(struct la));
  return v && (
     // set time & random seed
     v->t0 = clock(),
     v->rand = lcprng(v->t0),

     // configure memory
     // how big a memory pool do we start with?
#define InitialPoolSize (1<<10)
     v->len = InitialPoolSize,
     // obviously there's no pool yet
     v->pool = NULL,
     // nor any protected values
     v->mm = NULL,
     // the data stack starts at the top of memory
     v->sp = v->pool + v->len,
     // the call stack lives on the data stack
     v->fp = (fr) v->sp,
     // the heap is all used up to start, so the first
     // allocation initializes the pool
     v->hp = v->sp,
     // ip, xp, symbols, and globals all start empty
     v->ip = (mo) nil,
     v->xp = nil,
     v->syms = nil,
     setw(v->glob, nil, NGlobs),

     // now we can start allocating.
     // global symbols // FIXME stop using these if possible
     (v->glob[Eval] = interns(v, "ev")) &&
     (v->glob[Apply] = interns(v, "ap")) &&
     (v->glob[Def] = interns(v, ":")) &&
     (v->glob[Cond] = interns(v, "?")) &&
     (v->glob[Lamb] = interns(v, "\\")) &&
     (v->glob[Quote] = interns(v, "`")) &&
     (v->glob[Seq] = interns(v, ",")) &&
     (v->glob[Splat] = interns(v, ".")) &&

     // make a hash for the toplevel namespace
     (_ = table(v)) &&
     (v->glob[Topl] = pair(v, _, nil)) // &&
     // register instruction addresses at toplevel so the
     // compiler can use them.
#define register_inst(a, b) && ((b) ? prim(v,b,a) : inst(v, "i-"#a,a))
     insts(register_inst))
    ? v : (fin(v), NULL); }

static NoInline bool inst(em v, const char *a, ll *b) {
  ob z; return !(z = interns(v, a)) ? 0 :
    !!tbl_set(v, cwm(v), z, putnum(b)); }

static NoInline bool prim(em v, const char *a, ll *i) {
  ob nom; mo k; return
    (nom = interns(v, a)) &&
    (nom = pair(v, nom, nil)) &&
    (with(nom, k = cells(v, 4)), k) ?
      !!tbl_set(v, cwm(v), A(nom), (ob)
        (k[0].ll = i,    k[1].ll = (ll*) nom,
         k[2].ll = NULL, k[3].ll = (ll*) k)) : 0; }
