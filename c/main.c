#include "lips.h"
#include "vm.h"
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
#define BOOT PREFIX "/lib/lips/prelude.lips"
static const char *help =
  "usage: %s [options and scripts]\n"
  "with no arguments, start a repl\n"
  "options:\n"
  "  -h print this message\n"
  "  -i start repl unconditionally\n"
  "  -_ don't bootstrap\n";

static ob scrp(en, const char*), scrr(en, u1, const char**);
static en init(u1, const char*, char **);

// unpack state & jump into thread
static NoInline ob go(en v) {
  ob xp, ip, *sp, *hp, *fp;
  Unpack();
  Next(0); }

int main(int argc, char **argv) {
  for (u1 shell = argc == 1, boot = true;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return EXIT_FAILURE;
      case '_': boot = false; continue;
      case 'i': shell = true; continue;
      case 'h': fprintf(stdout, help, *argv); continue;
      case -1:
        if (argc == optind && !shell) return EXIT_SUCCESS;
        en v = init(shell, boot ? BOOT : NULL,  argv + optind);
        return v && go(v) ? EXIT_SUCCESS : EXIT_FAILURE; } }

// init : en u1 string? strings
static en init(u1 shell, const char *boot, char **paths) {
  en v;
  ob y, x;
  bind(v, li_ini());
  bind(x, scrr(v, shell, (const char **) paths));
  if (boot) {
    with(x, y = scrp(v, boot));
    bind(y, y);
    bind(x, sequence(v, y, x)); }
  v->ip = x;
  return v; }

// vm functions to yield from the main thread
//
// li_fin_ok : nil lips
static Vm(li_fin_ok) { return li_fin(v), nil; }
// li_repl : nil lips
static Vm(li_repl) {
  for (Pack();;) {
    if ((xp = parse(v, stdin))) {
      if ((xp = eval(v, xp))) emsep(v, xp, stdout, '\n'); }
    else if (feof(stdin)) return li_fin(v), nil; } }

// functions to compile scripts into a program
//
// scr_ : two en stream
static ob scr_(en v, FILE *in) {
  ob y, x = parse(v, in);
  if (!x) return feof(in) ? nil : 0;
  bind(x, pair(v, x, nil));
  bind(x, pair(v, Qt, x));
  bind(x, pair(v, x, nil));
  bind(x, pair(v, Eva, x));
  with(x, y = scr_(v, in));
  bind(y, y);
  return pair(v, x, y); }

// scrp : yo en string
static ob scrp(en v, const char *path) {
  FILE *in = fopen(path, "r");
  if (!in) return err(v, "%s : %s", path, strerror(errno));
  ob x = scr_(v, in);
  fclose(in);
  bind(x, x);
  bind(x, pair(v, Se, x));
  return analyze(v, x); }

// scrr : yo en bool strings
static ob scrr(en v, u1 shell, const char **paths) {
  yo h;
  ob x, y;
  const char *path = *paths;
  if (!path) {
    bind(h, cells(v, 3));
    h[0].ll = (vm*) (shell ? li_repl : li_fin_ok);
    h[1].ll = NULL;
    h[2].ll = (vm*) h;
    return (ob) h; }
  bind(y, scrr(v, shell, paths+1));
  with(y, x = scrp(v, path));
  bind(x, x);
  return sequence(v, x, y); }
