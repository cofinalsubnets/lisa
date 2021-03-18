#include "lips.h"
////
/// lisp parser
//
// this should be portable to lisp as soon as
// the string processing primitives are good
// enough, at which point it can be called the
// bootstrap parser
#define err_eof "[parse] unexpected eof"
#define err_rpar "[parse] unmatched right delimiter"

NoInline const char *tnom(enum type t) { switch (t) {
  case Hom: return "hom";
  case Num: return "num";
  case Tbl: return "table";
  case Two: return "pair";
  case Tup: return "tuple";
  case Oct: return "string";
  case Sym: return "symbol";
  default:  return "nil"; } }

typedef obj P(vm, FILE*);
static P atom, reads, quote, str;

#define readx(v,m)(errp(v,m),0)

static int read0(FILE *i) {
  for (int c;;) switch ((c = getc(i))) {
    case '#':
    case ';': do c = getc(i); while (c != '\n' && c != EOF);
    case ' ': case '\t': case '\n': continue;
    default: return c; } }

obj parse(vm v, FILE *i) {
  for (int c;;) switch ((c = read0(i))) {
    case EOF:  return 0;
    case ')':  return readx(v, err_rpar);
    case '(':  return reads(v, i);
    case '"':  return str(v, i);
    case '\'': return quote(v, i);
    default:   return ungetc(c, i), atom(v, i); } }

static obj quote(vm v, FILE *i) {
  obj r = parse(v, i);
  return !r ? r :
   (r = pair(v, r, nil), pair(v, Qt, r)); }

static obj reads(vm v, FILE *i) {
  obj x, y, c;
  switch ((c = read0(i))) {
    case EOF: return readx(v, err_eof);
    case ')': return nil;
    default:  ungetc(c, i);
              if (!(x = parse(v, i))) return x;
              with(x, y = reads(v, i));
              return y ? pair(v, x, y) : y; } }

static obj rloop(vm v, FILE *i, oct o, num n, num lim,
  obj (*re)(vm, FILE*, oct, num, num)) {
  obj x;
  o->len = n, x = putoct(o);
  return o->text[n-1] == 0 ? x :
    (with(x, o = cells(v, 1 + b2w(2*n))),
     memcpy(o->text, getoct(x)->text, o->len = n),
     re(v, i, o, n, 2 * n)); }

static obj atom_(vm v, FILE *p, oct o, num n, num lim) {
  obj x;
  while (n < lim) switch (x = fgetc(p)) {
    case ' ': case '\n': case '\t': case ';': case '#':
    case '(': case ')': case '\'': case '"':
      ungetc(x, p); case EOF:
      o->text[n++] = 0;
      goto out;
    default: o->text[n++] = x; } out:
  return rloop(v, p, o, n, lim, atom_); }

static obj str_(vm v, FILE *p, oct o, num n, num lim) {
  obj x;
  while (n < lim) switch (x = fgetc(p)) {
    case '\\': if ((x = fgetc(p)) == EOF) {
    case EOF: case '"': o->text[n++] = 0; goto out; }
    default: o->text[n++] = x; } out:
  return rloop(v, p, o, n, lim, str_); }

static obj atom(vm v, FILE *i) {
  obj o = atom_(v, i, cells(v, 2), 0, 8);
  char *st = NULL;
  num j = strtol(chars(o), &st, 0);
  return !st || *st ? intern(v, o) : putnum(j); }

static obj str(vm v, FILE *i) {
  return str_(v, i, cells(v, 2), 0, 8); }

void emsep(vm v, obj x, FILE *o, char s) {
  emit(v, x, o), fputc(s, o); }

void phomn(vm v, obj x, FILE *o) {
  switch (kind(x)) {
    case Sym: emit(v, x, o); break;
    case Two:
      if (symp(X(x))) emit(v, X(x), o);
      if (twop(Y(x))) fputc('\\', o), phomn(v, Y(x), o); } }

static void emoct(vm v, oct s, FILE *o) {
  fputc('"', o);
  for (num i = 0, l = s->len - 1; i < l; i++)
    if (s->text[i] == '"') fputs("\\\"", o);
    else fputc(s->text[i], o);
  fputc('"', o); }
static void emtbl(vm v, tbl t, FILE *o) {
  fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }
static void emsym(vm v, sym y, FILE *o) {
  nilp(y->nom) ? fprintf(o, "#sym@%lx", (num) y) :
                 fputs(chars(y->nom), o); }
static void emtwo_(vm v, two w, FILE *o) {
  twop(w->y) ? (emsep(v, w->x, o, ' '), emtwo_(v, gettwo(w->y), o)) :
                emsep(v, w->x, o, ')'); }
static void emtwo(vm v, two w, FILE *o) {
  if (w->x == Qt && twop(w->y) && nilp(Y(w->y)))
    fputc('\'', o), emit(v, X(w->y), o);
  else fputc('(', o), emtwo_(v, w, o); }
static void emnum(vm v, num n, FILE *o) {
  fprintf(o, "%ld", n); }
static void emhom(vm v, hom h, FILE *o) {
  fputc('\\', o), phomn(v, homnom(v, puthom(h)), o); }
void emit(vm v, obj x, FILE *o) {
  switch (kind(x)) {
    case Hom: return emhom(v, gethom(x), o);
    case Num: return emnum(v, getnum(x), o);
    case Sym: return emsym(v, getsym(x), o);
    case Two: return emtwo(v, gettwo(x), o);
    case Oct: return emoct(v, getoct(x), o);
    case Tbl: return emtbl(v, gettbl(x), o);
    default:  return (void) fputs("()", o); } }

void vferrp(vm v, FILE *o, const char *msg, va_list xs) {
  vfprintf(o, msg, xs), fputc('\n', o); }

void errp(vm v, const char *msg, ...) {
  va_list xs;
  va_start(xs, msg), vferrp(v, stderr, msg, xs), va_end(xs); }
