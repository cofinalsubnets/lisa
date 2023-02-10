#include "i.h"


static NoInline void errp(li, const char*, ...);

void report(li v, enum status s) {
  switch (s) {
    default: return;
    case DomainError: errp(v, "has no value"); return;
    case OomError: errp(v, "oom at %d words", v->len); return;
    case SyntaxError: errp(v, "syntax error"); return;
    case ArityError:
      errp(v, "wrong arity : %d of %d", v->fp->argc, getnum(v->xp));
      return;
    case SystemError:
      errp(v, "system error : %s", strerror(errno));
      return;
    case NameError: {
      const char *n = "#sym";
      size_t l = 4;
      str s = ((sym) v->xp)->nom;
      if (s) n = s->text, l = s->len;
      errp(v, "free variable : %.*s", l, n);
      return; } } }

static NoInline void show_call(li v, mo ip, frame fp) {
  putc('(', stderr);
  transmit(v, stderr, (ob) ip);
  for (size_t i = 0, argc = fp->argc; i < argc;
    putc(' ', stderr),
    transmit(v, stderr, fp->argv[i++]));
  putc(')', stderr); }

#define ErrPrefix ";;"
#define aubas (fp == (sf) (v->pool + v->len))
static void show_backtrace(li v, mo ip, frame fp) {
  while (!aubas) fputs(ErrPrefix " in ", stderr),
                 show_call(v, ip, fp),
                 putc('\n', stderr),
                 ip = (mo) fp->retp,
                 fp = fp->subd; }

// this prints a backtrace.
static NoInline void errp(li v, const char *msg, ...) {
  mo ip = v->ip; sf fp = v->fp;
  fputs(ErrPrefix " ", stderr);

  // show the function if there is one
  if (!aubas) show_call(v, ip, fp),
              putc(' ', stderr),
              ip = fp->retp,
              fp = fp->subd;
  // show message
  va_list xs;
  va_start(xs, msg),
  vfprintf(stderr, msg, xs),
  va_end(xs);

  putc('\n', stderr),
  show_backtrace(v, ip, fp); }
