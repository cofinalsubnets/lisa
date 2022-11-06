#include "la.h"
#include <string.h>
#include <errno.h>
#include <stdarg.h>

static NoInline str str0catr(la v, size_t l, va_list xs) {
  char *cs = va_arg(xs, char*);
  if (!cs) {
    str s = cells(v, Width(str) + b2w(l+1));
    if (s) ini_str(s, l+1), s->text[l] = 0;
    return s ; }
  size_t i = strlen(cs);
  str s = str0catr(v, l+i, xs);
  if (s) memcpy(s->text + l, cs, i);
  return s; }

static str str0cat(la v, ...) {
  va_list xs;
  va_start(xs, v);
  str s = str0catr(v, 0, xs);
  va_end(xs);
  return s; }

#include <sys/stat.h>
// the str returned is null-terminated.
// FIXME distunguish OOM from file not found
static str seek_lib_path(la v, const char *nom) {
  str s;
  char *home = getenv("HOME");
  struct stat _;
  if (home) {
    s = str0cat(v, home, "/.local/lib/lisa/", nom, ".la", NULL);
    if (s && 0 == stat(s->text, &_)) return s; }
  s = str0cat(v, "/lib/lisa/", nom, ".la", NULL);
  if (s && 0 == stat(s->text, &_)) return s;
  s = str0cat(v, "/usr/lib/lisa/", nom, ".la", NULL);
  if (s && 0 == stat(s->text, &_)) return s;
  s = str0cat(v, "/usr/local/lib/lisa/", nom, ".la", NULL);
  if (s && 0 == stat(s->text, &_)) return s;
  return 0; }

bool la_lib(la v, const char *nom) {
  str path = seek_lib_path(v, nom);
  if (!path) return
    errp(v, "module not found : %s", nom),
    false;
  FILE *in = fopen(path->text, "r");
  if (!in) return
    errp(v, "%s : %s", path->text, strerror(errno)),
    false;
  bool ok = true;
  for (ob x; ok && !feof(in);
    x = la_rx(v, in),
    ok = x ? la_ev(v, x) : feof(in));
  if (!ok) errp(v, "%s : %s", "error loading module", nom);
  return fclose(in), ok; }
