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

static obj go(int, char**),
           scri(lips, u1, const char*, const char **);

int main(int argc, char **argv) {
  return go(argc, argv) ? EXIT_SUCCESS : EXIT_FAILURE; }

#ifndef PREFIX
#define PREFIX "/usr/local"
#endif
#define BOOT PREFIX "/lib/lips/prelude.lips"

static obj go(int argc, char **argv) {
  for (u1 shell = argc == 1, boot = true;;)
    switch (getopt(argc, argv, "hi_")) {
      default: return 0;
      case '_': boot = false; break;
      case 'i': shell = true; break;
      case 'h':
        fprintf(stdout,
          "usage: %s [options and scripts]\n"
          "with no arguments, start a repl\n"
          "options:\n"
          "  -h print this message\n"
          "  -i start repl unconditionally\n"
          "  -_ don't bootstrap\n", *argv);
        break;
      case -1: {
        if (argc == optind && !shell) return nil;
        lips v = li_ini();
        if (!v || !(v->ip = scri(v, shell, boot ? BOOT : NULL, (const char**) argv + optind)))
          return li_fin(v), 0;
        obj xp, ip;
        mem sp, hp, fp;
        Unpack();
        Next(0); } } }

static obj scr_(lips v, FILE *in) {
  obj y, x = parse(v, in);
  if (!x) return feof(in) ? nil : 0;
  // lol :(
  bind(x, pair(v, x, nil));
  bind(x, pair(v, Qt, x));
  bind(x, pair(v, x, nil));
  bind(x, pair(v, Eva, x));
  with(x, y = scr_(v, in));
  bind(y, y);
  return pair(v, x, y); }

static obj scrp(lips v, const char *path) {
  FILE *in = fopen(path, "r");
  if (!in) return
    errp(v, "%s : %s", path, strerror(errno)),
    0;

  jmp_buf re;
  v->restart = &re;
  if (setjmp(re)) return v->restart = NULL, fclose(in), 0;
  obj x = scr_(v, in);
  v->restart = NULL, fclose(in);
  bind(x, x);
  bind(x, pair(v, Se, x));
  return analyze(v, x); }

static Vm(li_fin_ok) { return li_fin(v), nil; }
static Vm(li_repl) {
  obj x;
  jmp_buf re;
  for (v->restart = &re, setjmp(re);;)
    if ((x = parse(v, stdin)))
      emsep(v, eval(v, x), stdout, '\n');
    else if (feof(stdin)) return li_fin(v), nil; }

static obj scrr(lips v, u1 shell, const char **paths) {
  const char *path = *paths;
  if (!path) {
    hom h;
    bind(h, cells(v, 3));
    h[0] = shell ? li_repl : li_fin_ok;
    h[1] = NULL;
    h[2] = (terp*) h;
    return _H(h); }
  obj x, y = scrr(v, shell, paths+1);
  bind(y, y);
  with(y, x = scrp(v, path));
  bind(x, x);
  return sequence(v, x, y); }

static obj scri(lips v, u1 shell, const char *boot, const char **paths) {
  obj y, x;
  bind(x, scrr(v, shell, paths));
  if (boot) {
    with(x, y = scrp(v, boot));
    bind(y, y);
    x = sequence(v, y, x); }
  return x; }
