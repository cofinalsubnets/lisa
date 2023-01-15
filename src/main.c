#include "i.h"
#include <getopt.h>
#include <unistd.h>
static vm vm_fin_exit_ok, vm_repl;
static enum status enproc(li, char**, vm*);
static enum status enproc2(li, FILE*);
static enum status vm_exit_error(li v, enum status r) {
  return report(v, r), li_fin(v), free(v), r; }

static NoInline enum status la_ev_x(la v, ob x) {
  mo k = thd(v,
    imm, x, push,
    imm, ev_f,
    rec, putnum(1),
    ev_f, NULL);
  if (!k) return OomError;
  return
    k[4].ap = (vm*) (k + 7),
    li_call(v, k, 0); }

static void interact(struct V*);
static FILE *boot_src(void);
static enum status source(struct V*, FILE*);

int main(int ac, char **av) {

  static const char *help =
    "usage: %s [options] [scripts]\n"
    "with no arguments, interact\n"
    "options:\n"
    "  -h show this message\n"
    "  -i interact\n"
    "  -_ don't bootstrap\n";

  bool boot = true,
       repl = ac == 1 && isatty(STDIN_FILENO);

  for (;;) switch (getopt(ac, av, "hi_")) {
    default: return EXIT_FAILURE;
    case 'i': repl = true; continue;
    case '_': boot = false; continue;
    case 'h': fprintf(stdout, help, *av); continue;
    case -1:
      if (ac == optind && !repl) return EXIT_SUCCESS;
      av += optind;
      goto out; } out:

  li v = malloc(sizeof(struct V));
  enum status s = v ? li_ini(v) : OomError;
  if (s == Ok) s = enproc(v, av, repl ? vm_repl : vm_fin_exit_ok);
  if (s == Ok && boot) s = enproc2(v, boot_src());
  if (s != Ok) return report(v, s), li_fin(v), free(v), s;
  return li_go(v); }

static enum status enproc2(li v, FILE *f) {
  if (!f) return SystemError;
  enum status r = receive(v, f);
  if (r == Eof) return fclose(f), Ok;
  ob x = v->xp;
  with(x, r = enproc2(v, f));
  if (r != Ok) return r;
  x = (ob) pair(v, x, nil);
  x = x ? (ob) pair(v, (ob) v->lex.quote, x) : x;
  x = x ? (ob) pair(v, x, nil) : x;
  x = x ? (ob) pair(v, (ob) v->lex.eval, x) : x;
  x = x ? (ob) ana(v, x) : x;
  x = x ? (ob) thd(v, imm, x, call, nil, jump, v->ip, NULL) : x;
  if (!x) return OomError;
  v->ip = (mo) x;
  return Ok; }

static enum status enproc1(li v, const char *p) {
  return enproc2(v, fopen(p, "r")); }

static enum status enproc(li v, char **av, vm *j) {
  const char *p = *av;
  if (!p) {
    mo k = thd(v, j, NULL);
    return k ? (v->ip = k, Ok) : OomError; }
  enum status r = enproc(v, av+1, j);
  return r == Ok ? enproc1(v, p) : r; }

static Vm(vm_fin_exit_ok) { return li_fin(v), free(v), Ok; }
static Vm(vm_repl) {
  Pack();
  enum status s;
  while ((s = receive(v, stdin)) == Ok &&
         (s = la_ev_x(v, v->xp)) != Eof)
    if (s == Ok) transmit(v, stdout, v->xp),
                 putc('\n', stdout);
    else report(v, s),
         li_unwind(v);
  return li_fin(v), free(v), Ok; }

#define NOM "li"
#define SUFF "la"
static FILE *boot_src(void) {
  FILE *b; char *home, buf[256]; return
  (home = getenv("HOME")) &&
  snprintf(buf, sizeof(buf), "%s/.local/lib/" NOM "/boot." SUFF, home) < sizeof(buf) &&
  ((b = fopen(buf, "r")) ||
   (b = fopen("/usr/local/lib/" NOM "/boot." SUFF, "r")) ||
   (b = fopen("/usr/lib/" NOM "/boot." SUFF, "r"))) ? b :
   fopen("/lib/" NOM "/boot." SUFF, "r"); }

void li_unwind(li v) {
  v->sp = (ob*) (v->fp = (frame) (v->pool + v->len)); }
