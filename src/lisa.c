#include "i.h"

void *cells(state f, size_t n) { return
  n <= avail(f) || please(f, n) ? bump(f, n) : 0; }

// garbage collector

typedef word gc_copy(state, word, word*, word*);
typedef void gc_evac(state, word, word*, word*);
static gc_copy cp_two, cp_str, cp;
static gc_evac wk_two, wk_str;
static gc_copy *gc_copy_s[] = { [Pair] = cp_two, [String] = cp_str, };
static gc_evac *gc_evac_s[] = { [Pair] = wk_two, [String] = wk_str, };

static word cp_str(state v, word x, word *p0, word *t0) {
  string src = (string) x;
  size_t len = sizeof(struct string) + src->len;
  string dst = bump(v, b2w(len));
  src->ap = memcpy(dst, src, len);
  return (word) dst; }

static word cp_two(state v, word x, word *p0, word *t0) {
  pair src = (pair) x,
       dst = bump(v, Width(struct pair));
  dst->ap = data, dst->typ = Pair,
  dst->a = src->a, dst->b = src->b;
  src->ap = (vm*) dst;
  return (word) dst; }

static void wk_str(state v, word x, word *p0, word *t0) {
  v->cp += b2w(sizeof(struct string) + ((string) x)->len); }

static void wk_two(state v, word x, word *p0, word *t0) {
  v->cp += Width(struct pair);
  A(x) = cp(v, A(x), p0, t0);
  B(x) = cp(v, B(x), p0, t0); }

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
static void copy_from(state, word*, size_t);
NoInline bool please(state f, size_t req) {
  word *b0p0 = f->pool, *b0p1 = f->loop;
  f->pool = b0p1, f->loop = b0p0;
#ifdef _l_mm_static
  return copy_from(f, b0p0, f->len), avail(f) >= req; }
#else
  size_t t0 = f->t0, t1 = clock(),
         len0 = f->len;

  copy_from(f, b0p0, len0);

  req += len0 - avail(f);
  size_t t2 = f->t0 = clock(),
         v = t2 == t1 ? v_hi : (t2 - t0) / (t2 - t1),
         len1 = len0;

  // resize or no ?
#define too_little (len1 < req || v < v_lo)
#define too_big (len1 >> 1 > req && v > v_hi)
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return true;

  // try to adjust
  word *b1p0 = malloc(len1 * 2 * sizeof(word));
  if (!b1p0) return req <= len0;

  f->len = len1;
  f->pool = b1p0;
  f->loop = b1p0 + len1;
  copy_from(f, b0p1, len0),

  free(b0p0 < b0p1 ? b0p0 : b0p1),
  f->t0 = clock();
  return true; }
#endif

static NoInline void copy_from(state f, word *p0, size_t len0) {
  word len1 = f->len,
       *p1 = f->pool,
       *t0 = p0 + len0,
       *t1 = p1 + len1,
       *sp0 = f->sp,
       slen = t0 - sp0,
       *sp1 = t1 - slen;
  f->sp = sp1;
  f->hp = f->cp = p1;
  f->ip = (verb) cp(f, (word) f->ip, p0, t0);
  f->dict = cp(f, f->dict, p0, t0);
  // copy stack
  for (size_t i = 0; i < slen; i++)
    sp1[i] = cp(f, sp0[i], p0, t0);
  // copy managed values
  for (struct mm *r = f->safe; r; r = r->next)
    *r->addr = cp(f, *r->addr, p0, t0);
  // cheney's algorithm
  for (verb k; (k = (verb) f->cp) < (verb) f->hp;)
    if (datp(k)) gc_evac_s[k[1].x](f, (word) k, p0, t0);
    else { for (; k->ap; k++) k->x = cp(f, k->x, p0, t0);
           f->cp = (word*) k + 2; }

  assert(f->hp <= f->sp);
  assert(f->sp <= f->pool + f->len); }

// this can give a false positive if x is a fixnum
#define livep(l,x) ((word*)x>=l->pool&&(word*)x<l->pool+l->len)
static NoInline word cp(state v, word x, word *p0, word *t0) {
  if (nump(x) || x < (word) p0 || x >= (word) t0) return x;
  cell src = (cell) x;
  if (homp(src->x) && livep(v, src->x)) return src->x;
  if (datp(src)) return gc_copy_s[src[1].x](v, (word) src, p0, t0);
  struct loop *t = mo_tag(src);
  thread ini = t->head, d = bump(v, t->end - ini), dst = d;
  for (verb s = ini; (d->x = s->x); s++->x = (word) d++);
  d[1].ap = (vm*) dst;
  return (word) (src - ini + dst); }

void l_fin(state f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

#define binop() {curry}, {.x = putnum(2)}
static union cell
  p_print[] = { {print} },
  p_eql[] = { binop(), {eq}, },
  p_not[] = { {not} },
  p_lt[] = { binop(), {lt}, },
  p_le[] = { binop(), {le} },
  p_gt[] = { binop(), {gt}, },
  p_ge[] = { binop(), {ge} },
  p_add[] = { binop(), {add}, };
static status l_ini_dict(state f) {
  static struct { const char *n; word x; } ini_dict[] = {
    { "+", (word) p_add },
    { "=", (word) p_eql },
    { "<", (word) p_lt },
    { "<=", (word) p_le },
    { ">", (word) p_gt },
    { ">=", (word) p_ge },
    { ".", (word) p_print },
  };
  for (int i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++) {
    string s = strof(f, ini_dict[i].n);
    pair w = s ? cons(f, (word) s, ini_dict[i].x) : 0,
         x = w ? cons(f, (word) w, f->dict) : 0;
    if (!x) return Oom;
    f->dict = (word) x; }
  return Ok; }

status l_ini(state f) {
  memset(f, 0, sizeof(struct lisa));
  const size_t len = 1;
  word *pool = malloc(2 * len * sizeof(word));
  if (!pool) return Oom;
  f->len = len;
  f->pool = f->hp = pool;
  f->loop = f->sp = pool + len;
  f->dict = nil;
  f->t0 = clock();
  status s = l_ini_dict(f);
  if (s != Ok) l_fin(f);
  return s; }

#include <stdarg.h>

// list length
size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

long lidx(state f, word l, word x) {
  for (long i = 0; twop(l); l = B(l), i++)
    if (eql(f, A(l), x)) return i;
  return -1; }

word assoc(state f, word l, word k) {
  for (; twop(l); l = B(l))
    if (eql(f, k, A(A(l)))) return A(l);
  return 0; }

word lookup(state f, word l, word k) {
  return (l = assoc(f, l, k)) ? B(l) : l; }

word dict_lookup(state f, word k) {
  return lookup(f, f->dict, k); }

static NoInline pair cons_(state f, word a, word b) {
  pair w = bump(f, Width(struct pair));
  w->ap = data;
  w->typ = Pair;
  w->a = a;
  w->b = b;
  return w; }

static NoInline pair cons_gc(state f, word a, word b) {
  bool _;
  avec(f, a, avec(f, b, _ = please(f, Width(struct pair))));
  return _ ? cons_(f, a, b) : 0; }

pair cons(state f, word a, word b) { return
  avail(f) >= Width(struct pair) ?
    cons_(f, a, b) :
    cons_gc(f, a, b); }

static NoInline word push1_gc(state l, word x) {
  bool ok; avec(l, x, ok = please(l, 1));
  return ok ? push1(l, x) : 0; }

word push1(state l, word x) { return
  avail(l) ? *--l->sp = x : push1_gc(l, x); }

static NoInline word push2_gc(state l, word x, word y) {
  bool ok; avec(l, x, avec(l, y, ok = please(l, 2)));
  return ok ? push2(l, x, y) : 0; }

word push2(state l, word x, word y) {
  if (avail(l) < 2) return push2_gc(l, x, y);
  word *sp = l->sp -= 2;
  return sp[1] = y, sp[0] = x; }

static NoInline word push3_gc(state f, word x, word y, word z) {
  bool ok;
  avec(f, x, avec(f, y, avec(f, z, ok = please(f, 3))));
  return ok ? push3(f, x, y, z) : 0; }

word push3(state f, word x, word y, word z) {
  if (avail(f) < 3) return push3_gc(f, x, y, z);
  word *sp = f->sp -= 3;
  return sp[2] = z, sp[1] = y, sp[0] = x; }

string strof(state f, const char *c) {
  size_t len = strlen(c);
  string o = cells(f, Width(struct string) + b2w(len));
  if (o) o->ap = data, o->typ = String, o->len = len,
         memcpy(o->text, c, len);
  return o; }

string buf_new(state f) {
  string s = cells(f, Width(struct string) + 1);
  if (s) s->ap = data, s->typ = String, s->len = sizeof(word);
  return s; }

NoInline string buf_grow(state f, string s) {
  string t; size_t len = s->len;
  avec(f, s, t = cells(f, Width(struct string) + 2 * b2w(len)));
  if (t) t->ap = data, t->typ = String, t->len = 2 * len,
         memcpy(t->text, s->text, len);
  return t; }

typedef status parser(state, source, word, status);
#define Parse(n) status n(state f, source i, word x, status s)
static Parse(rx_ret) { return
  s ? s : push1(f, x) ? Ok : Oom; }
static Inline Parse(pull) { return
  ((parser*) pop1(f))(f, i, x, s); }

#define Getc getc
#define Ungetc ungetc
#define Feof feof

static int read_char(source);
static parser pxs_cont, pxs_cons;
static status
  px(state, source);
static word
  pxs(state, source),
  read_str_lit(state, source),
  read_atom(state, source);

// push parser continuations
static Inline word ppk(state f, parser *k) {
  return push1(f, (word) k); }
static Inline word ppkx(state f, parser *k, word x) {
  return push2(f, (word) k, x); }

status read_source(state f, source i) {
  return ppk(f, rx_ret) ? px(f, i) : Oom; }

////
/// " the parser "
//
// simple except it uses the managed stack for recursion.

static NoInline status px(state f, source i) {
  word x, c = read_char(i); switch (c) {
    case ')': case EOF: return pull(f, i, 0, Eof);
    case '(': return pxs(f, i);
    case '"':
      x = read_str_lit(f, i);
      return pull(f, i, x, x ? Ok : Oom);
    default:
      Ungetc(c, i);
      x = read_atom(f, i);
      return pull(f, i, x, x ? Ok : Oom); } }

static word pxs(state f, source i) {
  int c = read_char(i); switch (c) {
    case ')': case EOF:
      return pull(f, i, nil, Ok);
    default:
      Ungetc(c, i);
      return ppk(f, pxs_cont) ?
        px(f, i) :
        pull(f, i, 0, Oom); } }

static Parse(pxs_cont) {
  return s ? pull(f, i, x, s) :
    ppkx(f, pxs_cons, x) ?
      pxs(f, i) :
      pull(f, i, 0, Oom); }

static Parse(pxs_cons) {
  word y = pop1(f);
  if (s) return pull(f, i, x, s);
  x = x ? (word) cons(f, y, x) : x;
  return pull(f, i, x, x ? Ok : Oom); }

static NoInline word read_str_lit(state f, source i) {
  string o = buf_new(f);
  for (size_t n = 0, lim = sizeof(word); o; o = buf_grow(f, o), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // backslash escapes next character
      case '\\': if ((x = Getc(i)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin:
        o->len = n;
        return (word) o; }
  return 0; }

static NoInline word read_atom(state f, source i) {
  string a = buf_new(f);
  for (size_t n = 0, lim = sizeof(word); a; a = buf_grow(f, a), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '"': Ungetc(x, i);
      case EOF: a->text[a->len = n] = 0; goto out;
      default: a->text[n++] = x; continue; } out:
  if (!a) return 0;
  char *e;
  long n = strtol(a->text, &e, 0);
  return *e == 0 ? putnum(n) : (word) a; }

// get the next token character from the stream
static NoInline int read_char(source i) {
  for (int c;;) switch (c = Getc(i)) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (Getc(i)) {
      case '\n': case EOF: return read_char(i); } } }

typedef void emitter(state, FILE*, word);
static emitter tx_two, tx_str;
static emitter *ems[] = { [Pair] = tx_two, [String] = tx_str, };

void transmit(state v, FILE* o, word x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (ptr(x)->ap == data) ems[ptr(x)[1].x](v, o, x);
  else fprintf(o, "#%lx", x); }

static void tx_str(state v, FILE *o, word _) {
  string s = (string) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static void tx_two(state v, FILE *o, word x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

typedef bool eqr(state, word, word);
static eqr eq_str, eq_two;
static eqr *eqs[] = { [Pair] = eq_two, [String] = eq_str, };

bool eql(state f, word a, word b) {
  if (a == b) return true;
  if (nump(a | b) || ptr(a)->ap != data) return false;
  return eqs[ptr(a)[1].x](f, a, b); }

// FIXME can overflow the stack
static bool eq_two(state f, word x, word y) {
  if (!htwop(ptr(y))) return false;
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static bool eq_str(state f, word x, word y) {
  if (!hstrp((thread) y)) return false;
  string a = (string) x, b = (string) y;
  if (a->len != b->len) return false;
  return 0 == strncmp(a->text, b->text, a->len); }
