#include "i.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <stdarg.h>
#define P1(n,i) { n, ((union cell[]){{i}})}
#define P2(n,i) { n, ((union cell[]){{curry}, {.x=putnum(2)},{i}})}
#define P3(n,i) { n, ((union cell[]){{curry}, {.x=putnum(3)},{i}})}

struct function_entry {
  const char *nom;
  union cell *val;
} ini_dict[] = {
  P2("+",  add), P2("-",  sub),
  P2("*",  mul), P2("/",  quot),
  P2("%",  rem), P2("=",  eq),
  P2("<",  lt), P2("<=",  le),
  P2(">=",  ge), P2(">",  gt),
  P1(".", print), P1("putc",  prc),
  P1("~",  not),
  P1("rand", rng),
  P2("X",  cons), P1("A",  car), P1("B",  cdr),
  P2("sget",  sget), P3("ssub",  ssub), P1("slen",  slen), P2("scat", scat),
  P1("s?",  Sp), P1("n?", Np), P1("X?",  Xp),
  P2("::", defmacro),
  P1("peek", peek),
  P2("poke", poke),
  P1("trim",  trim), P2("seek",  seek),
  P1("tnew", tnew), P1("tkeys", tkeys), P1("tlen", tlen),
  P3("tset", tset), P3("tget", tget), P3("tdel", tdel),
  P1("gensym", gensym), P1("ev", ev0),
  P1("thd", thda), };

static int stdin_getc(core f, input i) { return getc(stdin); }
static void stdin_ungetc(core f, input i, char c) { ungetc(c, stdin); }
static bool stdin_eof(core f, input i) { return feof(stdin); }
static void stdout_putc(core f, output o, char c) { putc(c, stdout); }
static void stderr_putc(core f, output o, char c) { putc(c, stderr); }
struct char_in std_input = { .getc = stdin_getc, .ungetc = stdin_ungetc, .eof = stdin_eof };
struct char_out std_output = { .putc = stdout_putc }, std_error = { .putc = stderr_putc };

static bool l_define(core f, const char *k, word v) {
  if (!pushs(f, 1, v)) return Oom;
  symbol y = literal_symbol(f, k);
  v = pop1(f);
  return y && table_set(f, f->dict, (word) y, v); }

static status l_ini(core f, bool (*please)(core, size_t), size_t len, word *pool) {
  word *loop = pool + len;
  memset(f, 0, sizeof(struct l_core));
  f->pool = pool, f->loop = loop;
  f->rand = f->t0 = clock();
  f->please = please;
  f->len = len, f->pool = pool, f->loop = loop;
  f->hp = pool, f->sp = pool + len;
  if (!(f->dict = new_table(f))) return Oom;
  if (!(f->macro = new_table(f))) return Oom;
  for (int i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++)
    if (!l_define(f, ini_dict[i].nom, (word) ini_dict[i].val))
      return Oom;
  return l_define(f, "global-namespace", (word) f->dict) ? Ok : Oom; }

void l_close(core f) {
  if (f) free(f->pool < f->loop ? f->pool : f->loop), free(f); }

l_core l_open(void) {
  core f = malloc(sizeof(struct l_core));
  if (!f) return NULL;
  const size_t len0 = 1;
  word *pool = malloc(2 * len0 * sizeof(word));
  if (!pool) return free(f), NULL;
  status s = l_ini(f, libc_please, len0, pool);
  return s == Ok ? f : (l_close(f), NULL); }


static NoInline word pushsr(core f, size_t m, size_t n, va_list xs) {
  if (!n) return f->please(f, m) ? m : n;
  word x = va_arg(xs, word), y;
  avec(f, x, y = pushsr(f, m, n - 1, xs));
  return y ? *--f->sp = x : y; }

word pushs(core f, size_t m, ...) {
  va_list xs; va_start(xs, m);
  word n, r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else for (n = 0, f->sp -= m; n < m; f->sp[n++] = r = va_arg(xs, word));
  va_end(xs);
  return r; }
  

static string new_buffer(core f) {
  string s = cells(f, Width(struct string) + 1);
  return s ? ini_str(s, sizeof(word)) : s; }

static NoInline string grow_buffer(core f, string s) {
  string t; size_t len = s->len;
  avec(f, s, t = cells(f, Width(struct string) + 2 * b2w(len)));
  if (t) memcpy(ini_str(t, 2 * len)->text, s->text, len);
  return t; }

#define Getc getc
#define Ungetc ungetc
#define Feof feof

// get the next significant character from the stream
static NoInline int read_char(core f, input i) {
  for (int c;;) loop: switch (c = i->getc(f, i)) {
    default: return c;
    case ' ': case '\t': case '\n': goto loop;
    case '#': case ';': for (;;) switch (i->getc(f, i)) {
      case '\n': case EOF: goto loop; } } }

static word read_str_lit(core, input),
            read_atom(core, input, int);

static status reads(core, input);
////
/// " the parser "
//
static status enquote(core f) {
  pair w = pairof(f, f->sp[0], nil);
  if (!w) return Oom;
  f->sp[0] = (word) w;
  symbol y = literal_symbol(f, "`");
  if (!y) return Oom;
  w = pairof(f, (word) y, f->sp[0]);
  if (!w) return Oom;
  f->sp[0] = (word) w;
  return Ok; }

static status read1c(core f, input i, int c) {
  word x; switch (c) {
    case EOF: return Eof;
    case '\'':
      c = read1i(f, i);
      return c == Ok ? enquote(f) : c;
    case '(': return reads(f, i);
    case ')': x = nil; break;
    case '"': x = read_str_lit(f, i); break;
    default: x = read_atom(f, i, c); }
  return x && pushs(f, 1, x) ? Ok : Oom; }

status read1i(core f, input i) {
  return read1c(f, i, read_char(f, i)); }


static status reads(core f, input i) {
  word c = read_char(f, i);
  switch (c) {
    case ')': case EOF: unnest:
      return pushs(f, 1, nil) ? Ok : Oom;
    default:
      c = read1c(f, i, c);
      if (c == Eof) goto unnest;
      if (c != Ok) return c;
      c = reads(f, i);
      if (c != Ok) return c;
      c = (word) pairof(f, f->sp[1], f->sp[0]);
      if (!c) return Oom;
      *++f->sp = (word) c;
      return Ok; } }

static NoInline word read_str_lit(core f, input i) {
  string o = new_buffer(f);
  for (size_t n = 0, lim = sizeof(word); o; o = grow_buffer(f, o), lim *= 2)
    for (int x; n < lim;) switch (x = i->getc(f, i)) {
      // backslash escapes next character
      case '\\': if ((x = i->getc(f, i)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, (word) o; }
  return 0; }

static NoInline word read_atom(core f, input i, int c) {
  string a = new_buffer(f);
  if (a) a->text[0] = c;
  for (size_t n = 1, lim = sizeof(word); a; a = grow_buffer(f, a), lim *= 2)
    while (n < lim) switch (c = i->getc(f, i)) {
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '"': case '\'': i->ungetc(f, i, c);
      case EOF: a->text[a->len = n] = 0; goto out;
      default: a->text[n++] = c; continue; } out:
  if (!a) return 0;
  char *e; long n = strtol(a->text, &e, 0);
  return *e == 0 ? putnum(n) : (word) intern(f, a); }


static const char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
#define DEFAULT_NUMBER_OUTPUT_BASE 10
static void print_num_r(core f, output o, intptr_t n, int base) {
  ldiv_t qr = ldiv(n, base);
  if (qr.quot) print_num_r(f, o, qr.quot, base);
  o->putc(f, o, digits[qr.rem]); }

void print_num(core v, output o, intptr_t n, int base) {
  if (!n) return o->putc(v, o, '0');
  if (n < 0) o->putc(v, o, '-'), n = -n;
  print_num_r(v, o, n, base); }

Vm(prc) {
  return Do(ip = (thread) sp[1],
            std_output.putc(f, &std_output, getnum(sp[1] = sp[0])),
            sp++); }

Vm(print) {
  transmit(f, &std_output, *sp);
  std_output.putc(f, &std_output, '\n');
  return op(1, *sp); }


void transmit(core f, output out, word x) {
  if (nump(x)) print_num(f, out, getnum(x), 10);
  else if (ptr(x)->ap == data) ptr(x)[1].typ->emit(f, out, x);
  else out->putc(f, out, '#'), print_num(f, out, x, 16); }

NoInline Vm(gc, size_t n) {
  return Pack(f), !f->please(f, n) ? Oom :
    f->ip->ap(f, f->ip, f->hp, f->sp); }
void *bump(core f, size_t n) {
  void *x = f->hp;
  f->hp += n;
  return x; }

void *cells(state f, size_t n) { return
  n <= avail(f) || f->please(f, n) ? bump(f, n) : 0; }

bool static_please(core f, size_t req) {
  word *p0 = f->pool, *p1 = f->loop;
  f->pool = p1, f->loop = p0;
  copy_from(f, p0, f->len);
  return avail(f) >= req; }

// garbage collector
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. governs the heap size
// as a side effect by trying to keep
//   v = (t2 - t0) / (t2 - t1)
// between
#define v_lo 8
// and
#define v_hi (v_lo << 6)
// where
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
bool libc_please(core f, size_t req) {
  word *b0p0 = f->pool, *b0p1 = f->loop;
  f->pool = b0p1, f->loop = b0p0;
  size_t t0 = f->t0, t1 = clock(),
         len0 = f->len;

  copy_from(f, b0p0, len0);

  req += len0 - avail(f);
  size_t t2 = f->t0 = clock(),
         v = t2 == t1 ? v_hi : (t2 - t0) / (t2 - t1),
         len1 = len0;

  // how to calculate whether v in bounds
#define too_little (len1 < req || v < v_lo)
#define too_big (len1 >> 1 > req && v > v_hi)

  // resize or no ?
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return true;

  // too big or too small so try and adjust
  word *b1p0 = malloc(len1 * 2 * sizeof(word));
  if (!b1p0) return req <= len0;

  f->len = len1;
  f->pool = b1p0;
  f->loop = b1p0 + len1;
  copy_from(f, b0p1, len0),

  free(b0p0 < b0p1 ? b0p0 : b0p1),
  f->t0 = clock();
  return true; }

NoInline void copy_from(core f, word *p0, size_t len0) {
  word len1 = f->len,
       *p1 = f->pool,
       *t0 = p0 + len0,
       *t1 = p1 + len1,
       *sp0 = f->sp,
       slen = t0 - sp0,
       *sp1 = t1 - slen;
  f->sp = sp1;
  f->hp = f->cp = p1;
  f->symbols = 0;
  f->ip = (thread) cp(f, (word) f->ip, p0, t0);
  f->dict = (table) cp(f, (word) f->dict, p0, t0);
  f->macro = (table) cp(f, (word) f->macro, p0, t0);
  // copy stack
  for (size_t i = 0; i < slen; i++) sp1[i] = cp(f, sp0[i], p0, t0);
  // copy managed values
  for (struct mm *r = f->safe; r; r = r->next) *r->addr = cp(f, *r->addr, p0, t0);
  // cheney's algorithm
  for (thread k; (k = (thread) f->cp) < (thread) f->hp;)
    if (datp(k)) k[1].typ->evac(f, (word) k, p0, t0); // is data
    else { for (; k->ap; k++) k->x = cp(f, k->x, p0, t0); // is thread
           f->cp = (word*) k + 2; } }

NoInline word cp(state v, word x, word *p0, word *t0) {
  // if it's a number or out of managed memory then return it
  if (!bounded(p0, x, t0)) return x;
  cell src = (cell) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (homp(x) && bounded(v->pool, x, v->pool + v->len)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return src[1].typ->copy(v, (word) src, p0, t0);
  // it's a thread, find the end
  struct tag *t = ttag(src);
  thread ini = t->head, d = bump(v, t->end - ini), dst = d;
  for (cell s = ini; (d->x = s->x); s++->x = (word) d++);
  d[1].ap = (vm*) dst;
  return (word) (src - ini + dst); }
