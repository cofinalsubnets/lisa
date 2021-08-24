#include "lips.h"
#include "terp.h"
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

#define NOM "lips"
#define USR_PATH ".local/lib/"NOM"/"
#define SYS_PATH "/usr/lib/"NOM"/"
static Inline int seekp(const char* p) {
 int b, c;
 b = openat(AT_FDCWD, getenv("HOME"), O_RDONLY);
 c = openat(b, USR_PATH, O_RDONLY), close(b);
 b = openat(c, p, O_RDONLY), close(c);
 if (b > -1) return b;
 b = openat(AT_FDCWD, SYS_PATH, O_RDONLY);
 c = openat(b, p, O_RDONLY), close(b);
 return c; }

int lips_boot(lips v) {
 const char * const path = "prelude.lips";
 int pre = seekp(path);
 if (pre == -1) return errp(v, "can't find %s", path), NO;
 FILE *f = fdopen(pre, "r");
 jmp_buf re;
 v->restart = &re;
 if (setjmp(re)) return
  errp(v, "error in %s", path),
  fclose(f), lips_fin(v), NO;
 return script(v, f); }

u0 lips_fin(lips v) { free(v->mem_pool); }

lips lips_open() {
 lips v = malloc(sizeof(struct lips));
 if (!v) return v;
 lips_init(v);
 if (lips_boot(v) != OK) {
  lips_close(v);
  return NULL; }
 return v; }

u0 lips_close(lips v) {
  lips_fin(v); free(v); }

static NoInline u0 rin(lips v, const char *a, terp *b) {
 obj z = interns(v, a);
 tblset(v, *Sp, z, Pn(b)); }

int lips_eval(lips v, char *expr) {
  FILE *f = fmemopen(expr, slen(expr), "r");
  if (!f) return NO;
  return script(v, f); }

u0 lips_init(lips v) {
 v->seed = v->t0 = clock(),
 v->ip = v->xp = v->syms = v->glob = nil,
 v->fp = v->hp = v->sp = (mem) W,
 v->count = 0, v->mem_len = 1, v->mem_pool = NULL,
 v->mem_root = NULL;
 vec t = cells(v, Size(tup) + NGlobs);
 set64(t->xs, nil, t->len = NGlobs);
 obj z, y = Glob = puttup(t);
 with(y,
  spush(v, table(v)),
#define repr(a,b)defprim(v,a,b)
#define rein(a)rin(v, "i-"#a,a)
  prims(repr), insts(rein),
  Top = spop(v),
  z = table(v), Mac = z,
#define bsym(i,s)(z=interns(v,s),AR(y)[i]=z)
  bsym(Eval, "ev"), bsym(Apply, "ap"),
  bsym(Def, ":"),   bsym(Cond, "?"), bsym(Lamb, "\\"),
  bsym(Quote, "`"), bsym(Seq, ","),  bsym(Splat, "."));
#define def(s, x) (y=interns(v,s),tblset(v,Top,y,x))
 def("ns", Top), def("macros", Mac); }
