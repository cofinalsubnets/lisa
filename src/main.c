#include "i.h"
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
      return main_process(ac, av + optind, boot, repl); } }

static enum status ana_arg(li v, char *path) {
    FILE *f = fopen(path, "r");
    if (!f) return SystemError;
    enum status s = load_file(v, f);
    if (s != Ok) return s;
    mo k = seq0(v, (mo) v->xp, v->ip);
    if (!k) return OomError;
    return v->ip = k, Ok; }

static vm vm_fin_exit_ok, vm_repl;
static enum status load_main_thread(li v, int ac, char **av, bool boot, bool repl) {
  mo k = thd(v, repl ? vm_repl : vm_fin_exit_ok, NULL);
  if (!k) return OomError;
  for (v->ip = k; ac--;) {
    enum status r = ana_arg(v, av[ac]);
    if (r != Ok) return r; }
  if (boot) {
    enum status r = load_file(v, boot_src());
    if (r != Ok) return r;
    k = seq0(v, (mo) v->xp, v->ip);
    if (!k) return OomError;
    v->ip = k; }
  return Ok; }

static enum status main_process(int ac, char **av, bool boot, bool repl) {
  li v = malloc(sizeof(struct V));
  enum status s = v ? li_ini(v) : OomError;
  if (s == Ok && boot) s = source(v, boot_src());
  while (s == Ok && *av) s = source(v, fopen(*av++, "r"));
  if (s == Ok && repl) interact(v);
  return report(v, s), li_fin(v), free(v), s; }

static enum status process_end(li v, enum status s) {
  return report(v, s), li_fin(v), s; }

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
