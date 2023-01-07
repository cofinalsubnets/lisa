#include "i.h"
#include "vm.h"
#include <getopt.h>
#include <unistd.h>

static NoInline enum status main_process(int, char**, bool, bool);

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
      return main_process(ac, av, boot, repl); } }

static enum status ana_arg(li v, char *path) {
    FILE *f = fopen(path, "r");
    if (!f) return SystemError;
    enum status s = load_file(v, f);
    if (s != Ok) return s;
    mo k = seq0(v, v->ip, (mo) v->xp);
    if (!k) return OomError;
    return v->ip = k, Ok; }

static enum status ana_args(li v, char **av, mo k) {
  char *path = *av;
  if (!path) return v->ip = k, Ok;
  enum status s = ana_args(v, ++av, k);
  return s != Ok ? s : ana_arg(v, path); }

static vm vm_fin_exit_ok, vm_repl;
static enum status main_process(int ac, char **av, bool boot, bool repl) {
  av += optind;
  struct V v;
  enum status s = li_ini(&v);
  if (s == Ok && boot)
    s = source(&v, boot_src());
  while (s == Ok && *av)
    s = source(&v, fopen(*av++, "r"));
  if (s == Ok && repl)
    interact(&v);
  return report(&v, s),
         li_fin(&v),
         s; }

static enum status source(struct V *v, FILE *in) {
  if (!in) return SystemError;
  for (enum status r;;)
    if ((r = receive(v, in)) != Ok ||
        (r = la_ev_x(v, v->xp)) != Ok)
      return fclose(in), r == Eof ? Ok : r; }

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
  while (v->fp != v->fp->subd) v->fp = v->fp->subd;
  v->sp = (ob*) v->fp; }


static Vm(vm_fin_exit_ok) {
  return li_fin(v), Ok; }

static Vm(vm_repl) {
  Pack();
  enum status s;
  while ((s = receive(v, stdin)) == Ok &&
         (s = la_ev_x(v, v->xp)) != Eof)
    if (s == Ok) transmit(v, stdout, v->xp),
                 putc('\n', stdout);
    else report(v, s),
         li_unwind(v);
  return li_fin(v), Ok; }

static void interact(struct V *v) {
  enum status s;
  while ((s = receive(v, stdin)) == Ok &&
         (s = la_ev_x(v, v->xp)) != Eof)
    if (s == Ok) transmit(v, stdout, v->xp),
                 putc('\n', stdout);
    else report(v, s),
         li_unwind(v); }
