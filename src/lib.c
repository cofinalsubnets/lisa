#include "la.h"
#include "alloc.h"
#include "str.h"
#include "tbl.h"
#include <string.h>
#include <errno.h>
#include <stdarg.h>

static NoInline str str0catr(la_carrier v, size_t l, va_list xs) {
  char *cs = va_arg(xs, char*);
  if (!cs) {
    str s = cells(v, wsizeof(struct str) + b2w(l+1));
    if (s) ini_str(s, l+1), s->text[l] = 0;
    return s ; }
  size_t i = strlen(cs);
  str s = str0catr(v, l+i, xs);
  if (s) memcpy(s->text + l, cs, i);
  return s; }

static str str0cat(la_carrier v, ...) {
  va_list xs;
  va_start(xs, v);
  str s = str0catr(v, 0, xs);
  va_end(xs);
  return s; }

#include <sys/stat.h>
// the str returned is null-terminated.
// FIXME distunguish OOM from file not found
static la_status seek_lib_path(la_carrier v, const char *nom) {
  str s;
  char *home = getenv("HOME");
  struct stat _;
  if (home) {
    s = str0cat(v, home, "/.local/lib/lisa/", nom, ".la", NULL);
    if (!s) return LA_XOOM;
    if (0 == stat(s->text, &_)) goto ok; }
  s = str0cat(v, "/lib/lisa/", nom, ".la", NULL);
  if (!s) return LA_XOOM;
  if (0 == stat(s->text, &_)) goto ok;
  s = str0cat(v, "/usr/lib/lisa/", nom, ".la", NULL);
  if (!s) return LA_XOOM;
  if (0 == stat(s->text, &_)) goto ok;
  s = str0cat(v, "/usr/local/lib/lisa/", nom, ".la", NULL);
  if (!s) return LA_XOOM;
  if (0 == stat(s->text, &_)) goto ok;
  return LA_XSYS;
ok:
  v->xp = (ob) s;
  return LA_OK; }

la_status la_lib(la_carrier v, const char *nom) {
  la_status s = seek_lib_path(v, nom);
  if (s != LA_OK) return s;
  str path = (str) v->xp;
//  if (tbl_get(v, v->topl, (ob) path, 0)) return LA_OK; // TODO
  FILE *in = fopen(path->text, "r");
  if (!in) return LA_XSYS;
  s = la_ev_stream(v, in);
  fclose(in);
  return s; }
