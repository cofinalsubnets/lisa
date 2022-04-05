#include "lips.h"
#include "terp.h"
#include "read.h"
#include "hom.h"
#include "mem.h"
#include "two.h"
#include "write.h"
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

static obj scrp(lips, const char*), scrr(lips, u1, const char**);
static lips init(u1, const char*, char **);

int main(int argc, char **argv) {
  for (u1 shell = argc == 1, boot = true;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return EXIT_FAILURE;
      case '_': boot = false; continue;
      case 'i': shell = true; continue;
      case 'h': fprintf(stdout, help, *argv); continue;
      case -1:
        if (argc == optind && !shell) return EXIT_SUCCESS;
        lips v = init(shell, boot ? BOOT : NULL,  argv + optind);
        return v && li_go(v) ? EXIT_SUCCESS : EXIT_FAILURE; } }

// unpack state & jump into thread
//
// go : obj lips
obj li_go(lips v) {
  obj xp, ip, *sp, *hp, *fp;
  Unpack();
  Next(0); }

// make a lips instance for these opts
//
// init : lips bool string strings
static lips init(u1 shell, const char *boot, char **paths) {
  lips v;
  obj y, x;
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
  for (Pack();;)
    if ((xp = parse(v, stdin))) {
      if ((xp = eval(v, xp))) emsep(v, xp, stdout, '\n'); }
    else if (feof(stdin)) return li_fin(v), nil; }

// functions to compile scripts into a program
//
// scr_ : two lips stream
static obj scr_(lips v, FILE *in) {
  obj y, x = parse(v, in);
  if (!x) return feof(in) ? nil : 0;
  bind(x, pair(v, x, nil));
  bind(x, pair(v, Qt, x));
  bind(x, pair(v, x, nil));
  bind(x, pair(v, Eva, x));
  with(x, y = scr_(v, in));
  bind(y, y);
  return pair(v, x, y); }

// scrp : hom lips string
static obj scrp(lips v, const char *path) {
  FILE *in = fopen(path, "r");
  if (!in) return err(v, "%s : %s", path, strerror(errno));
  obj x = scr_(v, in);
  fclose(in);
  bind(x, x);
  bind(x, pair(v, Se, x));
  return analyze(v, x); }

// scrr : hom lips bool strings
static obj scrr(lips v, u1 shell, const char **paths) {
  const char *path = *paths;
  if (!path) {
    hom h;
    bind(h, cells(v, 3));
    h[0] = shell ? li_repl : li_fin_ok;
    h[1] = NULL;
    h[2] = (terp*) h;
    return _H(h); }
  obj x, y;
  bind(y, scrr(v, shell, paths+1));
  with(y, x = scrp(v, path));
  bind(x, x);
  return sequence(v, x, y); }
