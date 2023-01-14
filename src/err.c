#include "i.h"

static NoInline void errp(la, const char*, ...);

void report(la v, enum status s) {
  switch (s) {
    // not error codes, so print nothing.
    default: return;
    case DomainError: return errp(v, "has no value");
    case OomError: return errp(v, "oom at %d words", v->len);
    case SyntaxError: return errp(v, "syntax error");
    case ArityError: return
      errp(v, "wrong arity : %d of %d", v->fp->argc, getnum(v->xp));
    case SystemError:
      return errp(v, "system error : %s", strerror(errno));
    case NameError: {
      const char *n = "#sym";
      size_t l = 4;
      str s = ((sym) v->xp)->nom;
      if (s) n = s->text, l = s->len;
      return errp(v, "free variable : %.*s", l, n); } } }

static NoInline void show_call(la v, mo ip, frame fp) {
  putc('(', stderr);
  transmit(v, stderr, (ob) ip);
  for (size_t i = 0, argc = fp->argc; i < argc;
    putc(' ', stderr),
    transmit(v, stderr, fp->argv[i++]));
  putc(')', stderr); }

// this prints a backtrace.
// TODO maybe show it upside down like python?
#define aubas (fp == fp->subd)
static NoInline void errp(la v, const char *msg, ...) {
  mo ip = v->ip;
  sf fp = v->fp;

  // print error
  fputs(";; ", stderr);

  // show the function if there is one
  if (!aubas)
    show_call(v, ip, fp),
    putc(' ', stderr),
    ip = fp->retp,
    fp = fp->subd;

  // show message
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  putc('\n', stderr);

  // show backtrace
  while (!aubas)
    fputs(";; in ", stderr),
    show_call(v, ip, fp),
    putc('\n', stderr),
    ip = (mo) fp->retp,
    fp = fp->subd; }
