#include "em.h"
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#ifndef PREFIX
#define PREFIX "/usr/local"
#endif
#define BOOT PREFIX "/lib/empath/prelude.em"
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

// scrp : yo em string
static ob scrp(em v, const char *path) {
  FILE *in = fopen(path, "r");
  if (!in) return err(v, "%s : %s", path, strerror(errno));
  ob x = scr_(v, in);
  fclose(in);
  bind(x, x);
  bind(x, pair(v, v->glob[Seq], x));
  return analyze(v, x); }

// scrr : yo em bool strings
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
