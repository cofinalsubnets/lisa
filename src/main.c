#include "i.h"
#include <getopt.h>
#include <unistd.h>

static FILE *boot_src(void);
static vm vm_repl;
static enum status enproc(li, char**, vm*),
                   enprocf(li, FILE*),
                   li_ev(li, ob);

static const char *help =
  "usage: %s [options] [scripts]\n"
  "with no arguments, interact\n"
  "options:\n"
  "  -h show this message\n"
  "  -i interact\n"
  "  -_ don't bootstrap\n";

int main(int ac, char **av) {
  bool boot = true,
       repl = ac == 1 && isatty(STDIN_FILENO);
  for (;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'i': repl = true; continue;
    case '_': boot = false; continue;
    case 'h': fprintf(stdout, help, *av); continue;
    case -1:
      if (ac == optind && !repl) return EXIT_SUCCESS;
      struct V v;
      av += optind;
      vm *j = repl ? vm_repl : xok;
      enum status s = li_ini(&v);
      if (s == Ok) s = enproc(&v, av, j);
      if (s == Ok && boot) s = enprocf(&v, boot_src());
      if (s == Ok) s = li_go(&v);
      return report(&v, s),
             li_fin(&v),
             s; } }

static enum status enprocf(li v, FILE *f) {
  if (!f) return SystemError;
  enum status r = receive(v, f);
  if (r != Ok) return fclose(f),
                      r == Eof ? Ok : r;
  ob x = v->xp;
  with(x, r = enprocf(v, f));
  if (r != Ok) return r;
  mo k = thd(v, immp, x,
                imm, ev_f, // assign target idx=3
                call, putnum(1),
                jump, v->ip, ev_f, // assign src idx=8
                End);
  if (!k) return OomError;
  return k[3].ap = (vm*) (k+8),
         v->ip = k,
         Ok; }

static enum status enproc(li v, char **av, vm *j) {
  const char *p = *av;
  if (!p) {
    mo k = thd(v, j, End);
    return !k ? OomError : (v->ip = k, Ok); }
  enum status r = enproc(v, av+1, j);
  return r == Ok ? enprocf(v, fopen(p, "r")) : r; }

static Vm(vm_repl) {
  Pack();
  for (enum status s; (s = receive(v, stdin)) != Eof;) {
    s = s == Ok ? li_ev(v, v->xp) : s;
    report(v, s), li_unwind(v);
    if (s == Ok) transmit(v, stdout, v->xp), putc('\n', stdout); }
  return Ok; }

static enum status li_ev(li v, ob x) {
  mo k = thd(v,
    immp, x,
    imm, nil, // assignment target idx=3
    call, putnum(1),
    xok, ev_f, // source idx=7
    End);
  if (!k) return OomError;
  return k[3].ap = (vm*) (k + 7),
         v->ip = k,
         li_go(v); }

#ifndef NOM
#define NOM "lisa"
#endif
#ifndef SUFF
#define SUFF "la"
#endif
static FILE *boot_src(void) {
  FILE *b; char *home, buf[256]; return
  (home = getenv("HOME")) &&
  snprintf(buf, sizeof(buf), "%s/.local/lib/" NOM "/boot." SUFF, home) < sizeof(buf) &&
  ((b = fopen(buf, "r")) ||
   (b = fopen("/usr/local/lib/" NOM "/boot." SUFF, "r")) ||
   (b = fopen("/usr/lib/" NOM "/boot." SUFF, "r"))) ? b :
   fopen("/lib/" NOM "/boot." SUFF, "r"); }
