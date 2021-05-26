#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <setjmp.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

// thanks !!
//
// first of all C makes you type waaaaaay too much
#define In inline __attribute__((always_inline))
#define Nin __attribute__((noinline))
#define Inline In
#define NoInline Nin
#define St static
#define Ty typedef
#define Sr struct
#define Un union
#define Ko const
#define En enum
#define Sz sizeof
#define R  return
#define El else
#define Sw switch
#define Bk break
#define Cu continue
#define Ks case
#define Df default
#define Wh while
#define Fo for

Ty intptr_t O, Z, *M;
Ty uintptr_t N;
Ty void _, Vd;
Ty char Ch;
Ty FILE *Io;
#define Ob (O)
#define non (Ob 0)
#define nil (~non)
#define W Sz(O) // pointer arithmetic unit
#define W2 (2*W)

// more fundamental data types
Ty Sr two { O x, y; } *Tw, *two; // pairs
Ty Sr tup { Z len; O xs[]; } *Ve, *tup; // vectors
Ty Sr oct { Z len; Ch text[]; } *By, *oct; // byte arrays
Ty Sr sym { O nom, code, l, r; } *Sy, *sym; // symbols
Ty Sr tble { O key, val; Sr tble *next; } *tble; // tables
Ty Sr tbl { Z len, cap; tble *tab; } *Ht, *tbl;

En T { // the 3 ls bits of each pointer are a type tag
 Hom = 0, Num = 1, Two = 2, Tup = 3,
 Oct = 4, Tbl = 5, Sym = 6, Nil = 7 };

En globl { // indices into a table of global constants
 Def, Cond, Lamb, Quote, Seq,
 Splat, Topl, Macs, Eval, Apply, NGlobs };

// a linked list of stack addresses containing live values
// that need to be preserved by garbage collection.
Ty Sr Mp { M one; Sr Mp *next; } *Mp;

// this structure is responsible for holding runtime state.
// most functions take a pointer to it as the first argument.
Ty Sr V {
 O ip, xp, *fp, *hp, *sp; // vm state variables
 O syms, glob; // symbols and globals
 Mp mem_root; // gc protection list
 Z t0, count, mem_len, *mem_pool; // memory data
 jmp_buf restart; // top level restart
} *V;

// this is the type of interpreter functions
Ty O terp(V, O, M, M, M, O);
Ty terp *T, **H; // code pointer ; the internal function type

V initialize(int, Ko Ch**),
  bootstrap(V),
  finalize(V);

_ emit(V, O, Io),
  errp(V, Ko Ch*, ...),
  emsep(V, O, Io, Ch),
  reqsp(V, Z);

O err(V, O, Ko Ch*, ...),
  restart(V),
  homnom(V, O),
  pair(V, O, O),
  parse(V, Io),
  intern(V, O),
  eval(V, O),
  table(V),
  tblset(V, O, O, O),
  tblget(V, O, O),
  tbldel(V, O, O),
  tblkeys(V, O),
  string(V, Ko Ch*);
int eql(O, O);

Ko Ch* tnom(En T);

#define kind(x) ((x)&7)
#define Gh(x) ((H)((x)))
#define Ph(x) (Ob(x))
#define Gn getnum
#define Pn putnum
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) (Ob(x)+Two)
#define getnum(n) ((Z)(n)>>3)
#define putnum(n) ((Ob(n)<<3)+Num)
#define getsym(x) ((sym)((O)(x)-Sym))
#define putsym(x) (Ob(x)+Sym)
#define gettup(x) ((tup)((x)-Tup))
#define puttup(x) (Ob(x)+Tup)
#define getoct(x) ((oct)((O)(x)-Oct))
#define putoct(x) (Ob(x)+Oct)
#define gettbl(x) ((tbl)((O)(x)-Tbl))
#define puttbl(x) (Ob(x)+Tbl)
#define homp(x) (kind(x)==Hom)
#define octp(x) (kind(x)==Oct)
#define nump(x) (kind(x)==Num)
#define twop(x) (kind(x)==Two)
#define symp(x) (kind(x)==Sym)
#define tupp(x) (kind(x)==Tup)
#define tblp(x) (kind(x)==Tbl)
#define nilp(x) ((x)==nil)
#define X(o) gettwo(o)->x
#define Y(o) gettwo(o)->y
#define XX(x) X(X(x))
#define XY(x) X(Y(x))
#define YX(x) Y(X(x))
#define YY(x) Y(Y(x))
#define F(x) ((H)(x)+1)
#define G(x) (*(H)(x))
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define chars(x) getoct(x)->text
#define symnom(y) chars(getsym(y)->nom)
#define mm(r) ((Safe=&((Sr Mp){(r),Safe})))
#define um (Safe=Safe->next)
#define LEN(x) (Sz((x))/Sz(*(x)))
#define AR(x) gettup(x)->xs
#define AL(x) gettup(x)->len
#define Mm(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define b2w(n)((n)/W+((n)%W&&1))
#define w2b(n) ((n)*W)
#define Szr(t) (Sz(Sr t)/W)
#define Size(t) Szr(t)
#define Ip v->ip
#define Fp v->fp
#define Hp v->hp
#define Sp v->sp
#define Safe v->mem_root
#define Xp v->xp
#define Pool v->mem_pool
#define Len v->mem_len
#define Dict Top
#define Syms (v->syms)
#define Glob v->glob
#define If AR(Glob)[Cond]
#define De AR(Glob)[Def]
#define La AR(Glob)[Lamb]
#define Qt AR(Glob)[Quote]
#define Se AR(Glob)[Seq]
#define Va AR(Glob)[Splat]
#define Top AR(Glob)[Topl]
#define Mac AR(Glob)[Macs]
#define Eva AR(Glob)[Eval]
#define App AR(Glob)[Apply]
#define Avail (Sp-Hp)

#define mix ((N)2708237354241864315)

St In H button(H h) {
 Wh (*h) h++;
 R h; }

St In _* bump(V v, Z n) { _* x;
 R x = v->hp, v->hp += n, x; }

St In _* cells(V v, Z n) {
 R Avail < n ? reqsp(v, n):0, bump(v, n); }

St In Z hbi(Z cap, N co) { R co % cap; }

St In tble hb(O t, N code) {
 R gettbl(t)->tab[hbi(gettbl(t)->cap, code)]; }

// subs for some libc functions: memset, memcpy, strlen
St _ fill(_*_d, O i, N l) { // fill with a word
 Fo (M d = _d; l--; *d++=i); }
St _ wcpy(_*_d, Ko _*_s, N l) { // copy words
 M d = _d; Ko O *s = _s;
 Wh (l--) *d++=*s++; }
St _ bcpy(_*_d, Ko _*_s, N l) { // copy bytes
 Ch *d = _d; Ko Ch *s = _s;
 Wh (l--) *d++=*s++; }
St N slen(Ko Ch*s) {
 Fo (N i=0;;s++,i++) if (!*s) R i; }

_Static_assert(
 Sz(O) >= 8,
 "pointers are smaller than 64 bits");

_Static_assert(
 -9 == (((-9)<<12)>>12),
 "opposite bit-shifts on a negative integer "
 "yield a nonidentical result");

////
/// lisp parser
//
// this should be portable to lisp as soon as
// the string processing primitives are good
// enough, at which point it can be called the
// bootstrap parser
#define err_eof "unexpected eof"
#define err_rpar "unmatched right delimiter"

Nin Ko Ch* tnom(En T t) { Sw (t) {
 Ks Hom: R "hom";
 Ks Num: R "num";
 Ks Tbl: R "tbl";
 Ks Two: R "two";
 Ks Tup: R "vec";
 Ks Oct: R "str";
 Ks Sym: R "sym";
 Df:     R "nil"; } }

Ty O P(V, Io);
St P atom, r1s, qt, str;

#define readx(v,m)(errp(v,m),0)

St int r0(Io i) { Fo (Z c;;) Sw ((c = getc(i))) {
 Ks '#': Ks ';': do c = getc(i); Wh (c != '\n' && c != EOF);
 Ks ' ': Ks '\t': Ks '\n': Cu;
 Df: R c; } }

O parse(V v, Io i) { Z c; Sw ((c = r0(i))) {
 Ks EOF:  R 0;
 Ks ')':  R readx(v, err_rpar);
 Ks '(':  R r1s(v, i);
 Ks '"':  R str(v, i);
 Ks '\'': R qt(v, i);
 Df:      R ungetc(c, i), atom(v, i); } }

St O qt(V v, Io i) { O r;
 R !(r = parse(v, i)) ? r :
  (r = pair(v, r, nil),
   pair(v, Qt, r)); }

St O r1s(V v, Io i) { O x, y, c;
 R (c = r0(i)) == EOF ? readx(v, err_eof) :
  c == ')' ? nil :
   (ungetc(c, i),
    !(x = parse(v, i)) ? x :
     (Mm(x, y = r1s(v, i)),
      y ? pair(v, x, y) : y)); }

St O rloop(V v, Io i, By o, Z n, Z lim,
           O (*re)(V, Io, By, Z, Z)) { O x;
 R o->len = n, x = putoct(o),
  o->text[n-1] == 0 ? x :
   (Mm(x, o = cells(v, 1 + b2w(2*n))),
    bcpy(o->text, getoct(x)->text, o->len = n),
    re(v, i, o, n, 2 * n)); }

St O atom_(V v, Io p, By o, Z n, Z lim) { O x;
 Wh (n < lim) Sw (x = fgetc(p)) {
  Ks ' ': Ks '\n': Ks '\t': Ks ';': Ks '#':
  Ks '(': Ks ')': Ks '\'': Ks '"':
   ungetc(x, p); Ks EOF:
   o->text[n++] = 0;
   goto out;
  Df: o->text[n++] = x; } out:
 R rloop(v, p, o, n, lim, atom_); }

St O str_(V v, Io p, By o, Z n, Z lim) { O x;
 Wh (n < lim) Sw (x = fgetc(p)) {
  Ks '\\': if ((x = fgetc(p)) == EOF) {
  Ks EOF: Ks '"': o->text[n++] = 0; goto out; }
  Df: o->text[n++] = x; } out:
 R rloop(v, p, o, n, lim, str_); }

St In Z chidx(Ch c, Ko Ch *s) {
  Fo (Z i = 0; *s; s++, i++) if (*s == c) R i;
  R -1; }

St Nin O readz_2(Ko Ch *s, Z rad) {
  St Ko Ch *dig = "0123456789abcdef";
  if (!*s) R nil;
  Z a = 0;
  int c;
  Fo (;;) {
    if (!(c = *s++)) Bk;
    a *= rad;
    int i = chidx(c, dig);
    if (i < 0 || i >= rad) R nil;
    a += i; }
  R Pn(a); }

St O readz(Ko Ch *s) {
  if (*s == '-') {
    O q = readz(s+1);
    R nump(q) ? Pn(-Gn(q)) : q; }
  if (*s == '0') switch (s[1]) {
    case 'b': R readz_2(s+2, 2);
    case 'o': R readz_2(s+2, 8);
    case 'd': R readz_2(s+2, 10);
    case 'z': R readz_2(s+2, 12);
    case 'x': R readz_2(s+2, 16); }
  R readz_2(s, 10); }

St O atom(V v, Io i) {
 O o = atom_(v, i, cells(v, 2), 0, 8), q = readz(chars(o));
 R nump(q) ? q : intern(v, o); }

St O str(V v, Io i) {
 R str_(v, i, cells(v, 2), 0, 8); }

_ emsep(V v, O x, Io o, Ch s) {
 emit(v, x, o), fputc(s, o); }

St _ emoct(V v, By s, Io o) {
 fputc('"', o);
 Fo (Z i = 0, l = s->len - 1; i < l; i++)
  if (s->text[i] == '"') fputs("\\\"", o);
  El fputc(s->text[i], o);
 fputc('"', o); }

St _ emtbl(V v, Ht t, Io o) {
 fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }

St _ emsym(V v, Sy y, Io o) {
 nilp(y->nom) ?
  fprintf(o, "#sym@%lx", (Z) y) :
  fputs(chars(y->nom), o); }

St _ emtwo_(V v, Tw w, Io o) {
 twop(w->y) ?
  (emsep(v, w->x, o, ' '), emtwo_(v, gettwo(w->y), o)) :
  emsep(v, w->x, o, ')'); }

St _ emtwo(V v, Tw w, Io o) {
 w->x == Qt && twop(w->y) && nilp(Y(w->y)) ?
  (fputc('\'', o), emit(v, X(w->y), o)) :
  (fputc('(', o), emtwo_(v, w, o)); }

St _ emnum(V v, Z n, Io o) { fprintf(o, "%ld", n); }

St _ phomn(V v, O x, Io o) {
 fputc('\\', o); 
 Sw (kind(x)) {
  Ks Sym: emit(v, x, o); Bk;
  Ks Two:
   if (symp(X(x))) emit(v, X(x), o);
   if (twop(Y(x))) phomn(v, Y(x), o); } }

St _ emhom(V v, H h, Io o) {
 phomn(v, homnom(v, Ph(h)), o); }

_ emit(V v, O x, Io o) { Sw (kind(x)) {
 Ks Hom: R emhom(v, Gh(x), o);
 Ks Num: R emnum(v, Gn(x), o);
 Ks Sym: R emsym(v, getsym(x), o);
 Ks Two: R emtwo(v, gettwo(x), o);
 Ks Oct: R emoct(v, getoct(x), o);
 Ks Tbl: R emtbl(v, gettbl(x), o);
 Df:     R (_) fputs("()", o); } }

_ errp(V v, Ko Ch *msg, ...) { va_list xs;
 fputs("# ", stderr);
 va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
 fputc('\n', stderr); }
#define OK EXIT_SUCCESS
#define NO EXIT_FAILURE

St int repl(V v, Io i, Io o) {
 O x;
 Fo (setjmp(v->restart);;)
  if ((x = parse(v, i))) emsep(v, eval(v, x), o, '\n');
  El if (feof(i)) Bk;
 R OK; }

St _ scr(V v, Io f) {
 Fo (O x; (x = parse(v, f)); eval(v, x)); }

St int scripts(V v, Ch** argv) {
 Fo (char *q; (q = *argv++);) {
  Io f = fopen(q, "r");
  if (!f) R errp(v, "%s : %s", q, strerror(errno)), NO;
  if (setjmp(v->restart)) R
   errp(v, "%s : fail", q),
   fclose(f),
   NO;
  scr(v, f);
  int ok = feof(f);
  fclose(f);
  if (!ok) R NO; }
 R OK; }

int main(int argc, Ch** argv) {
#define takka 1
#define nprel 2
 Ko Ch
  opts[] = "hi_",
  help[] =
   "usage: %s [options and scripts]\n"
   "options:\n"
   "  -_ don't bootstrap\n"
   "  -i interact unconditionally\n"
   "  -h print this message\n";

 int opt, args,
  F = argc == 1 ? takka : 0;

 Wh ((opt = getopt(argc, argv, opts)) != -1) Sw (opt) {
  Ks '?': R NO;
  Ks '_': F|=nprel; Bk;
  Ks 'i': F|=takka; Bk;
  Ks 'h': fprintf(stdout, help, argv[0]); Bk; }

 args = argc - optind;
 if (args == 0 && !(F&takka)) R OK;

 V v = initialize(argc, (Ko Ch**) argv);
 v = F&nprel ? v : bootstrap(v);
 if (!v) R NO;

 int r = OK;
 if (args) r = scripts(v, argv + optind);
 if (r == OK && F&takka) repl(v, stdin, stdout);
 R finalize(v), r; }

St int copy(V, Z);
St O cp(V, O, Z, M);
St In _ do_copy(V, Z, M, Z, M);
St O sskc(V, M, O);

// a simple copying garbage collector

// gc entry point reqsp : vm x num -> bool
//
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// manages the size of the memory pool. here is the procedure
// in a nutshell:
//
// - copy into a new pool of the same size. if this fails,
//   the request fails (oom).
// - if there's enough space and the garbage collector
//   is running fast enough, return success.
// - otherwise adjust the size and copy again. if this fails,
//   we can still succeed if the first copy left us with
//   enough free space (ie. we tried to grow for performance
//   reasons). but otherwise the request fails (oom).
//
// at a constant rate of allocation, doubling the size of the
// heap halves the amount of time spent in garbage collection.
// the memory manager uses this relation to automatically trade
// space for time to keep the time spent in garbage collection
// under a certain proportion of total running time: amortized
// time in garbage collection should e than about 6%, at the cost of
// less efficient memory use under pressure.
#define grow() (len*=2,vit*=2)
#define shrink() (len/=2,vit/=2)
#define growp (allocd > len || vit < 32) // lower bound
#define shrinkp (allocd < len/2 && vit >= 128) // upper bound
_ reqsp(V v, Z req) {
 Z len = v->mem_len, vit = copy(v, len);
 if (vit) { // copy succeeded
  Z allocd = len - (Avail - req);
     if (growp)   do grow();   Wh (growp);
  El if (shrinkp) do shrink(); Wh (shrinkp);
  El R; // no size change needed
  // otherwise grow or shrink
  if (copy(v, len)) R;
  // oh no that didn't work
  // maybe we can still return though
  if (allocd <= Len) R; } // aww darn
 errp(v, "oom"); // this is a bad error
 restart(v); }

// the first step in copying is to allocate
// a new pool of the given length, which must
// be at least enough to support the actual
// amount of reachable memory. if this fails
// then return 0. otherwise swap the pools,
// reset internal symbols, copy the stack,
// global variables, and any user saved
// locations, and free the old pool. then
// return u:
//
//     non-gc running time     t1    t2
// ,.........................,/      |
// -----------------------------------
// |                          `------'
// t0                  gc time (this cycle)
//
//       u = (t2 - t0) / (t2 - t1)
//
// t values come from clock(). if t0 < t1 < t2 then
// u will be >= 1. however, sometimes t1 == t2. in that case
// u = 1.
St int copy(V v, Z len) {
 clock_t t1 = clock(), t2, u;
 M b0 = v->mem_pool, b1 = malloc(w2b(len));
 R !b1 ? 0 :
  (do_copy(v, v->mem_len, b0, len, b1),
   free(b0),
   t2 = clock(),
   u = t1 == t2 ? 1 : (t2 - v->t0) / (t2 - t1),
   v->t0 = t2,
   u); }

St In _ do_copy(V v, Z l0, M b0, Z l1, M b1) {
 M s0 = Sp, t0 = b0 + l0, t1 = b1 + l1;
 Z ro = t1 - t0;
 v->mem_len = l1;
 v->mem_pool = Hp = b1;
 Sp += ro, Fp += ro;
 Syms = nil;
 Wh (t0-- > s0) Sp[t0 - s0] = cp(v, *t0, l0, b0);
#define CP(x) x=cp(v,x,l0,b0)
 CP(Ip), CP(Xp), CP(Glob);
 Fo (Mp r = Safe; r; r = r->next) CP(*(r->one)); }
#undef CP

// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
Ty O cp_(V, O, Z, M);
St cp_ cphom, cptup, cptwo, cpsym, cpoct, cptbl;
#define cpcc(n) St O n(V v, O x, Z ln, M lp)

cpcc(cp) {  Sw (kind(x)) {
 Ks Hom: R cphom(v, x, ln, lp);
 Ks Tup: R cptup(v, x, ln, lp);
 Ks Oct: R cpoct(v, x, ln, lp);
 Ks Two: R cptwo(v, x, ln, lp);
 Ks Sym: R cpsym(v, x, ln, lp);
 Ks Tbl: R cptbl(v, x, ln, lp);
 Df:  R x; } }

#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((M)(o),Pool,Pool+Len)
cpcc(cptwo) {
 Tw dst, src = gettwo(x);
 R fresh(src->x) ? src->x :
  (dst = bump(v, Size(two)),
   dst->x = src->x, src->x = (O) puttwo(dst),
   dst->y = cp(v, src->y, ln, lp),
   dst->x = cp(v, dst->x, ln, lp),
   puttwo(dst)); }

cpcc(cptup) {
 Ve dst, src = gettup(x);
 if (fresh(*src->xs)) R *src->xs;
 dst = bump(v, Size(tup) + src->len);
 Z l = dst->len = src->len;
 dst->xs[0] = src->xs[0];
 src->xs[0] = puttup(dst);
 dst->xs[0] = cp(v, dst->xs[0], ln, lp);
 Fo (Z i = 1; i < l; ++i)
   dst->xs[i] = cp(v, src->xs[i], ln, lp);
 R puttup(dst); }

cpcc(cpoct) {
 By dst, src = getoct(x);
 R src->len == 0 ? *(M)src->text :
  (dst = bump(v, Size(oct) + b2w(src->len)),
   wcpy(dst->text, src->text, b2w(src->len)),
   dst->len = src->len, src->len = 0,
   *(M)src->text = putoct(dst)); }

cpcc(cpsym) {
 Sy src = getsym(x), dst;
 if (fresh(src->nom)) R src->nom;
 if (nilp(src->nom)) // anonymous symbol
  dst = bump(v, Size(sym)),
  wcpy(dst, src, Size(sym));
 El dst = getsym(sskc(v, &Syms, cp(v, src->nom, ln, lp)));
 R src->nom = putsym(dst); }

#define stale(o) inb((M)(o),lp,lp+ln)
cpcc(cphom) {
 H dst, src = Gh(x), end = src, start;
 if (fresh(G(src))) R (O) G(src);
 Wh (*end) end++;
 start = (H) G(end+1);
 Z len = (end+2) - start;
 dst = bump(v, len);
 H j = dst;
 Fo (H k = start; k < end; j++, k++ )
  G(j) = G(k), G(k) = (T) Ph(j);
 G(j) = NULL;
 G(j+1) = (T) dst;
 Fo (O u; j-- > dst;)
  u = (O) G(j),
  G(j) = (T) (stale(u) ? cp(v, u, ln, lp) : u);
 R Ph(dst += src - start); }

St tble cptble(V v, tble src, Z ln, M lp) {
 if (!src) R NULL;
 tble dst = (tble) bump(v, 3);
 dst->next = cptble(v, src->next, ln, lp);
 dst->key = cp(v, src->key, ln, lp);
 dst->val = cp(v, src->val, ln, lp);
 R dst; }

cpcc(cptbl) {
 Ht src = gettbl(x);
 if (fresh(src->tab)) R (O) src->tab;
 Z src_cap = src->cap;
 Ht dst = bump(v, 3 + src_cap);
 dst->len = src->len;
 dst->cap = src_cap;
 dst->tab = (tble*) (dst + 1);
 tble *src_tab = src->tab;
 src->tab = (tble*) puttbl(dst);
 Wh (src_cap--)
  dst->tab[src_cap] = cptble(v, src_tab[src_cap], ln, lp);
 R puttbl(dst); }


// XXX data constructors

St N hc(V, O);

////
/// data constructors and utility functions
//

// pairs
O pair(V v, O a, O b) { Tw t;
 R Avail >= 2 ?
  (t = bump(v, 2),
   t->x = a, t->y = b,
   puttwo(t)) :
  (Mm(a, Mm(b, reqsp(v, 2))),
   pair(v, a, b)); }

// list functions
St Z idx(O l, O x) {
 Fo (Z i = 0; twop(l); l = Y(l), i++)
  if (x == X(l)) R i;
 R -1; }

St Z llen(O l) {
 Fo (Z i = 0;; l = Y(l), i++)
  if (!twop(l)) R i; }

St O snoc(V v, O l, O x) {
 if (!twop(l)) R pair(v, x, l);
 Mm(l, x = snoc(v, Y(l), x));
 R pair(v, X(l), x); }

St O linitp(V v, O x, M d) { O y;
 if (!twop(Y(x))) R *d = x, nil;
 Mm(x, y = linitp(v, Y(x), d));
 R pair(v, X(x), y); }


// strings
O string(V v, Ko Ch* c) {
 Z bs = 1 + slen(c);
 By o = cells(v, Size(oct) + b2w(bs));
 bcpy(o->text, c, o->len = bs);
 R putoct(o); }

//symbols

// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult. the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection. 
St O ssk(V v, O y, O x) {
 int i = strcmp(symnom(y), chars(x));
 R i == 0 ? y :
  sskc(v, i < 0 ? &(getsym(y)->r) : &(getsym(y)->l), x); }

St O sskc(V v, M y, O x) {
 if (!nilp(*y)) R ssk(v, *y, x);
 Sy u = bump(v, Size(sym));
 u->nom = x, u->code = hc(v, x);
 u->l = nil, u->r = nil;
 R *y = putsym(u); }

O intern(V v, O x) {
 if (Avail < Size(sym))
  Mm(x, reqsp(v, Size(sym)));
 R sskc(v, &Syms, x); }

St In N hash_bytes(Z len, char *us) { Z h = 1;
 Fo (; len--; h *= mix, h ^= *us++);
 R mix * h; }

St N hc(V v, O x) { N r;
 Sw (kind(x)) {
  Ks Sym: r = getsym(x)->code; Bk;
  Ks Oct: r = hash_bytes(getoct(x)->len, getoct(x)->text); Bk;
  Ks Two: r = hc(v, X(x)) ^ hc(v, Y(x)); Bk;
  Ks Hom: r = hc(v, homnom(v, x)) ^ (mix * (uintptr_t) G(x)); Bk;
  Ks Tbl: r = mix * mix; // umm this'll do for now ...
  Df:     r = mix * x; } 
 R (r<<48)|(r>>16); }

// tblrsz(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
St _ tblrsz(V v, O t, Z ns) {
 tble e, ch, *b, *d;
 Mm(t, fill((M) (b = cells(v, ns)), 0, ns));
 Ht o = gettbl(t);
 Z u, n = o->cap;
 d = o->tab; o->tab = b; o->cap = ns;
 Wh (n--) Fo (ch = d[n]; ch;
  e = ch,
  ch = ch->next,
  u = hbi(ns, hc(v, e->key)),
  e->next = b[u],
  b[u] = e); }

St _ tblade(V v, O t, O k, O x, Z b) { tble e; Ht y;
 Mm(t, Mm(k, Mm(x, e = cells(v, Size(tble)))));
 y = gettbl(t);
 e->key = k, e->val = x;
 e->next = y->tab[b], y->tab[b] = e;
 ++y->len; }

O tblset(V v, O t, O k, O val) {
 Z b = hbi(gettbl(t)->cap, hc(v, k));
 tble e = gettbl(t)->tab[b];
 Fo (;e; e = e->next)
  if (e->key == k) R e->val = val;
 R Mm(t, Mm(val,
    tblade(v,t,k,val,b),
    gettbl(t)->len / gettbl(t)->cap > 2 ?
     tblrsz(v, t, gettbl(t)->cap*2) : 0)),
   val; }

St O tblkeys_j(V v, tble e, O l) { O x;
 if (!e) R l;
 x = e->key;
 Mm(x, l = tblkeys_j(v, e->next, l));
 R pair(v, x, l); }

St O tblkeys_i(V v, O t, Z i) { O k;
  if (i == gettbl(t)->cap) R nil;
  Mm(t, k = tblkeys_i(v, t, i+1));
  R tblkeys_j(v, gettbl(t)->tab[i], k); }

O tblkeys(V v, O t) {
  R tblkeys_i(v, t, 0); }

O tbldel(V v, O t, O k) {
  Ht y = gettbl(t);
  O r = nil;
  Z b = hbi(y->cap, hc(v, k));
  tble e = y->tab[b];
  Sr tble _v = {0,0,e};
  Fo (tble l = &_v; l && l->next; l = l->next)
    if (l->next->key == k) {
      r = l->next->val;
      l->next = l->next->next;
      y->len--;
      Bk; }
  y->tab[b] = _v.next;
  if (y->len && y->cap / y->len > 2)
   Mm(r, Mm(t, tblrsz(v, t, y->cap / 2)));
  R r; }

O tblget(V v, O t, O k) {
  tble e = hb(t, hc(v, k));
  Fo (;e; e = e->next) if (eql(e->key, k)) R e->val;
  R 0; }

O table(V v) {
  tbl t = cells(v, Sz(Sr tbl)/w2b(1) + 1);
  tble *b = (tble*)(t+1);
  *b = NULL;
  t->tab = b;
  t->len = 0;
  t->cap = 1;
  R puttbl(t); }

// this is a cool way to do "static data", i got it from luajit :)
#define insts(_)\
  _(tget),   _(tset),   _(thas),   _(tlen),   _(gsym_u),\
  _(arity),  _(idZ),    _(idH),    _(id2),    _(idT),\
  _(imm),    _(arg),    _(clo),    _(loc),    _(take),\
  _(locals), _(loc_),   _(pc0),    _(pc1),    _(clos),\
  _(encll),  _(encln),  _(yield),  _(ret),    _(jump),\
  _(branch), _(barnch), _(call),   _(rec),    _(lbind),\
  _(tbind),  _(push),   _(add),    _(sub),    _(mul),\
  _(dqv),    _(mod),    _(neg),    _(lt),     _(lteq),\
  _(eq),     _(gteq),   _(gt),     _(twopp),  _(numpp),\
  _(nilpp),  _(strpp),  _(tblpp),  _(sympp),  _(hompp),\
  _(car),    _(cdr),    _(cons),   _(vecpp),  _(hom_u),\
  _(add_u),  _(sub_u),  _(mul_u),  _(div_u),  _(mod_u),\
  _(lt_u),   _(lteq_u), _(eq_u),   _(gteq_u), _(gt_u),\
  _(twop_u), _(nump_u), _(homp_u), _(tblp_u), _(strp_u),\
  _(nilp_u), _(car_u),  _(cdr_u),  _(cons_u), _(vecp_u),\
  _(strmk),  _(strg),   _(strl),   _(strs),   _(strc),\
  _(symp_u), _(unit),   _(one),    _(zero),   _(hfin_u),\
  _(arg0),   _(arg1),   _(loc0),   _(loc1),   _(clo0),\
  _(clo1),   _(brlt),   _(brlteq), _(breq),   _(brgteq),\
  _(brgt),   _(brne),   _(zzz),    _(tbll),   _(tblmk),\
  _(tblg),   _(tblc),   _(tbls),   _(tbld),   _(tblks),\
  _(hom_seek_u), _(hom_geti_u), _(hom_getx_u),\
  _(fail),   _(ccc_u),  _(cont),   _(vararg), _(tuck),\
  _(dupl),   _(emi),    _(drop),   _(emx_u),  _(emi_u),\
  _(emx),    _(em_u),   _(ev_u),   _(ap_u), _(rnd_u)
#define prims(_)\
  _("A", car_u),     _("B", cdr_u),          _("X", cons_u),    _("=", eq_u),\
  _("<", lt_u),      _("<=", lteq_u),        _(">", gt_u),      _(">=", gteq_u),\
  _("+", add_u),     _("-", sub_u),          _("*", mul_u),     _("/", div_u),\
  _("%", mod_u),     _("ap", ap_u),          _("ccc", ccc_u),   _("ev", ev_u),\
  _("fail", fail),   _("tbl", tblmk),        _("tget", tblg),   _("tset", tbls),\
  _("thas", tblc),   _("tdel", tbld),        _("tkeys", tblks), _("tlen", tbll),\
  _("slen", strl),   _("sget", strg),        _("scat", strc),   _("ssub", strs),\
  _("str", strmk),   _("gensym", gsym_u),    _("zzz", zzz),     _(".", em_u),\
  _("vecp", vecp_u), _("nump", nump_u),      _("symp", symp_u), _("twop", twop_u),\
  _("tblp", tblp_u), _("strp", strp_u),      _("nilp", nilp_u), _("homp", homp_u),\
  _("hom", hom_u),   _("hseek", hom_seek_u), _("emx", emx_u),   _("hgetx", hom_getx_u),\
  _("emi", emi_u),   _("hgeti", hom_geti_u), _("hfin", hfin_u), _("rand", rnd_u)

#define ninl(x) x NoInline
terp insts(ninl);

////
/// bootstrap thread compiler
//
// functions construct their continuations by pushing function
// pointers onto the main stack with Pu()
#define Pu(...) pushs(v,__VA_ARGS__,non)
#define Push(...) Pu(__VA_ARGS__)
// and then calling them with ccc
#define Cc ((c1*)Gn(*Sp++))
#define ccc Cc
// there's a natural correspondence between the Pu/Cc pattern
// in this file and normal continuation passing style in lisp
// (cf. the stage 2 compiler).

// in addition to the main stack, the compiler uses Xp and Ip
// as stacks for storing code entry points when generating
// conditionals, which is admittedly kind of sus.
//
// this compiler emits runtime type checks for safety but does
// (almost) no optimizations or static typing since all it has
// to do is bootstrap the main compiler.

// " compilation environments "
// the current lexical environment is passed to compiler
// functions as a pointer to an object, either a tuple with a
// structure specified below, or nil for toplevel. it's a
// pointer to an object, instead of just an object, so it can
// be gc-protected once instead of separately by every function.
// in the other compiler it's just a regular object.
#define toplp(x) !e
#define arg(x)  AR(x)[0] // argument variables : a list
#define loc(x)  AR(x)[1] // local variables : a list
#define clo(x)  AR(x)[2] // closure variables : a list
#define par(x)  AR(x)[3] // surrounding scope : tuple or nil
#define name(x) AR(x)[4] // function name : a symbol or nil
#define asig(x) AR(x)[5] // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.
//

Ty O c1(V,M,Z), c2(V,M,Z,O), c3(V,M,Z,O,O);
St Vd c_de_r(V, M, O),
      scan(V, M, O),
      pushs(V, ...);
St c1 c_ev, c_d_bind, inst, insx, c_ini;
St c2 c_eval, c_sy, c_2, c_imm, ltu, c_ap, c_la_clo;
St c3 late;
St O snoc(V,O,O),
     look(V,O,O),
     linitp(V,O,M),
     hfin(V,O),
     hini(V,Z);
St Z idx(O, O);
#define interns(v,c) intern(v,string(v,c))

En { Here, Loc, Arg, Clo, Wait };
#define c1(nom,...) St O nom(V v,M e,Z m,##__VA_ARGS__)
#define c2(nom,...) St O nom(V v,M e,Z m,O x,##__VA_ARGS__)

// emit code backwards like cons
St In O em1(T i, O k) {
  R k -= W, G(k) = i, k; }
St In O em2(T i, O j, O k) {
  R em1(i, em1((T)j, k)); }

St O imx(V v, M e, Z m, T i, O x) {
 R Pu(Pn(i), x), insx(v, e, m); }

St O apply(V v, O f, O x) {
  Pu(f, x);
  H h = cells(v, 5);
  h[0] = call;
  h[1] = (T) Pn(2);
  h[2] = yield;
  h[3] = NULL;
  h[4] = (T) h;
  R call(v, Ph(h), Fp, Sp, Hp, tblget(v, Dict, App)); }

St O compile(V v, O x) {
 Pu(Pn(c_ev), x, Pn(inst), Pn(yield), Pn(c_ini));
 R ccc(v, NULL, 0); }

/// evaluate an expression
O eval(V v, O x) { R
 x = pair(v, x, nil),
 apply(v, tblget(v, Dict, Eva), x); }

St O rwlade(V v, O x) { O y; R
 Mm(x, y = snoc(v, YX(x), XY(x)),
       y = pair(v, La, y),
       y = pair(v, y, YY(x))),
 pair(v, XX(x), y); }

St int scan_def(V v, M e, O x) {
 if (!twop(x)) R 1; // this is an even case so export all the definitions to the local scope
 if (!twop(Y(x))) R 0; // this is an odd case so ignore these, they'll be imported after the rewrite
 mm(&x);
 int r = scan_def(v, e, YY(x));
 if (r) {
  while (twop(X(x))) x = rwlade(v, x);
  O y = pair(v, X(x), loc(*e));
  loc(*e) = y;
  scan(v, e, XY(x)); }
 R um, r; }

St _ scan(V v, M e, O x) {
 if (!twop(x) || X(x) == La || X(x) == Qt) R;
 if (X(x) == De) R (void) scan_def(v, e, Y(x));
 Fo (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
 um; }

St O asign(V v, O a, Z i, M m) { O x;
 if (!twop(a)) R *m = i, a;
 if (twop(Y(a)) && XY(a) == Va)
  R *m = -(i+1), pair(v, X(a), nil);
 Mm(a, x = asign(v, Y(a), i+1, m));
 R pair(v, X(a), x); }

St Ve tuplr(V v, Z i, va_list xs) { Ve t; O x;
 R (x = va_arg(xs, O)) ?
  (Mm(x, t = tuplr(v, i+1, xs)), t->xs[i] = x, t) :
  ((t = cells(v, Size(tup) + i))->len = i, t); }

St O tupl(V v, ...) { Ve t; va_list xs;
 R va_start(xs, v),
  t = tuplr(v, 0, xs),
  va_end(xs),
  puttup(t); }

St O scope(V v, M e, O a, O n) { Z s = 0;
  R Mm(n, a = asign(v, a, 0, &s)),
    tupl(v, a, nil, nil, e ? *e : nil, n, Pn(s), non); }

St O compose(V v, M e, O x) {
  Pu(Pn(c_ev), x, Pn(inst), Pn(ret), Pn(c_ini));
  scan(v, e, Sp[1]);
  x = ccc(v, e, 4); // 4 = 2 + 2
  O i = llen(loc(*e));
  if (i) x = em2(locals,  Pn(i), x);
  i = Gn(asig(*e));
     if (i > 0) x = em2(arity, Pn(i), x);
  El if (i < 0) x = em2(vararg, Pn(-i-1), x);
  x = hfin(v, x);
  R twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expression, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
St O ltu(V v, M e, O n, O l) { O y;
  l = Y(l);
  Mm(n,
    l = twop(l) ? l : pair(v, l, nil),
    Mm(y, l = linitp(v, l, &y),
          Mm(l, n = pair(v, n, toplp(e) ? nil : e ? name(*e):nil)),
          n = scope(v, e, l, n)),
    l = compose(v, &n, X(y)));
  R l; }


c2(c_la) {
  T j = imm;
  O k, nom = *Sp == Pn(c_d_bind) ? Sp[1] : nil;
  Mm(nom, Mm(x, k = ccc(v, e, m+2)));
  Mm(k,
    x = homp(x = ltu(v, e, nom, x)) ? x :
    (j = toplp(e) || !twop(loc(*e)) ? encln : encll,
     c_la_clo(v, e, X(x), Y(x))));
  R em2(j, x, k); }

c2(c_imm) { R Pu(Pn(imm), x), insx(v, e, m); }

St O c_la_clo(V v, M e, O arg, O seq) {
  Z i = llen(arg);
  mm(&arg), mm(&seq);
  Fo (Pu(Pn(insx), Pn(take), Pn(i), Pn(c_ini));
      twop(arg);
      Pu(Pn(c_ev), X(arg), Pn(inst), Pn(push)), arg = Y(arg));
  R arg = ccc(v, e, 0), um, um,
    pair(v, seq, arg); }

c1(c_d_bind) {
 O y = *Sp++;
 R toplp(e) ? imx(v, e, m, tbind, y) :
              imx(v, e, m, loc_, Pn(idx(loc(*e), y))); }

St _ c_de_r(V v, M e, O x) {
 if (!twop(x)) R;
 if (twop(X(x))) R c_de_r(v, e, rwlade(v, x));
 Mm(x, c_de_r(v, e, YY(x))),
 Pu(Pn(c_ev), XY(x), Pn(c_d_bind), X(x)); }

// syntactic sugar for define
St O def_sug(V v, O x) { O y = nil; R
 Mm(y, x = linitp(v, x, &y)),
 x = pair(v, x, y),   x = pair(v, Se, x),
 x = pair(v, x, nil), x = pair(v, La, x),
 pair(v, x, nil); }

c2(c_de) {
 R !twop(Y(x)) ? c_imm(v, e, m, nil) :
  llen(Y(x)) % 2 ? c_eval(v, e, m, def_sug(v, x)) :
  (c_de_r(v, e, Y(x)), ccc(v, e, m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 Xp
#define S2 Ip

// before generating anything, store the
// exit address in stack 2
c1(c_co_pre) {
  O x = ccc(v, e, m);
  R x = pair(v, x, S2), X(S2 = x); }

// before generating a branch emit a jump to
// the top of stack 2
c1(c_co_pre_con) {
  O x = ccc(v, e, m+2), k = X(S2);
  R G(k) == ret ? em1(ret, x) : em2(jump, k, x); }

// after generating a branch store its address
// in stack 1
c1(c_co_post_con) {
  O x = ccc(v, e, m);
  R x = pair(v, x, S1), X(S1 = x); }

// before generating an antecedent emit a branch to
// the top of stack 1
c1(c_co_pre_ant) {
  O x = ccc(v, e, m+2);
  R x = em2(branch, X(S1), x), S1 = Y(S1), x; }

St _ c_co_r(V v, M e, O x) {
  if (!twop(x)) x = pair(v, nil, nil);
  if (!twop(Y(x)))
    Pu(Pn(c_ev), X(x), Pn(c_co_pre_con));
  El Mm(x, Pu(Pn(c_co_post_con), Pn(c_ev), XY(x),
              Pn(c_co_pre_con)),
           c_co_r(v, e, YY(x))),
     Pu(Pn(c_ev), X(x), Pn(c_co_pre_ant)); }

c2(c_co) { R
 Mm(x, Pu(Pn(c_co_pre))),
 c_co_r(v, e, Y(x)),
 x = ccc(v, e, m),
 S2 = Y(S2),
 x; }

St _ c_se_r(V v, M e, O x) {
 if (twop(x)) Mm(x, c_se_r(v, e, Y(x))),
              Pu(Pn(c_ev), X(x)); }
c2(c_se) {
 if (!twop(x = Y(x))) x = pair(v, nil, nil);
 R c_se_r(v, e, x), ccc(v, e, m); }

c1(c_call) {
 O a = *Sp++, k = ccc(v, e, m + 2);
 R em2(G(k) == ret ? rec : call, a, k); }

#define L(n,x) pair(v, Pn(n), x)
St O look(V v, O e, O y) { O q; R
 nilp(e) ?
  ((q = tblget(v, Dict, y)) ?  L(Here, q) : L(Wait, Dict)) :
 ((q = idx(loc(e), y)) != -1) ?
  L(Loc, e) :
 ((q = idx(arg(e), y)) != -1) ?
  L(Arg, e) :
 ((q = idx(clo(e), y)) != -1) ?
  L(Clo, e) :
 look(v, par(e), y); }
#undef L

c2(late, O d) { O k; R
 x = pair(v, d, x),
 Mm(x, k = ccc(v, e, m+2)),
 Mm(k, x = pair(v, Pn(8), x)),
 em2(lbind, x, k); }

c2(c_sy) { O y, q;
 Mm(x, y = X(q = look(v, e ? *e:nil, x)));
 Sw (Gn(y)) {
  Ks Here: R c_imm(v, e, m, Y(q));
  Ks Wait: R late(v, e, m, x, Y(q));
  Df: if (Y(q) == *e) Sw (Gn(y)) {
       Ks Loc: R imx(v, e, m, loc, Pn(idx(loc(*e), x)));
       Ks Arg: R imx(v, e, m, arg, Pn(idx(arg(*e), x)));
       Ks Clo: R imx(v, e, m, clo, Pn(idx(clo(*e), x))); }
      y = llen(clo(*e));
      Mm(x, q = snoc(v, clo(*e), x)), clo(*e) = q;
      R imx(v, e, m, clo, Pn(y)); } }

c1(c_ev) { R c_eval(v, e, m, *Sp++); }
c2(c_eval) { R (symp(x)?c_sy:twop(x)?c_2:c_imm)(v,e,m,x); }

c2(c_qt) { R c_imm(v, e, m, twop(x = Y(x)) ? X(x) : x); }
c2(c_2) { O z = X(x); R
 (z==Qt?c_qt:
  z==If?c_co:
  z==De?c_de:
  z==La?c_la:
  z==Se?c_se:c_ap)(v,e,m,x); }

#define Rec(...) {\
  O _s1 = S1, _s2 = S2;\
  Mm(_s1, Mm(_s2,__VA_ARGS__));\
  S1 = _s1, S2 = _s2; }

c2(c_ap) {
 O y = tblget(v, Mac, X(x));
 if (y) {
  Rec(x = apply(v, y, Y(x)));
  R c_eval(v, e, m, x); }
 Fo (mm(&x),
     Pu(Pn(c_ev), X(x), Pn(inst), Pn(idH),
        Pn(c_call), Pn(llen(Y(x))));
     twop(x = Y(x));
     Pu(Pn(c_ev), X(x), Pn(inst), Pn(push)));
 R um, ccc(v, e, m); }

c1(inst) {
 T i = (T) Gn(*Sp++);
 R em1(i, ccc(v, e, m+1)); }

c1(insx) {
 T i = (T) Gn(*Sp++);
 O x = *Sp++, k;
 Mm(x, k = ccc(v, e, m+2));
 R em2(i, x, k); }

c1(c_ini) {
 O k = hini(v, m+1);
 R em1((T)(e ? name(*e):Eva), k); }

St Vd pushss(V v, Z i, va_list xs) {
 O x; (x = va_arg(xs, O)) ?
   (Mm(x, pushss(v, i, xs)), *--Sp = x) :
   reqsp(v, i); }

St Vd pushs(V v, ...) {
 Z i = 0;
 va_list xs; va_start(xs, v);
 Wh (va_arg(xs, O)) i++;
 va_end(xs), va_start(xs, v);
 if (Avail < i) pushss(v, i, xs);
 El Fo (M sp = Sp -= i; i--; *sp++ = va_arg(xs, O));
 va_end(xs); }

O hini(V v, Z n) {
 H a = cells(v, n + 2);
 R G(a+n) = NULL,
   GF(a+n) = (T) a,
   fill((M) a, nil, n),
   Ph(a+n); }

St O hfin(V v, O a) {
 R Ob (GF(button(Gh(a))) = (T) a); }

O homnom(V v, O x) {
 T k = G(x);
 if (k == clos || k == pc0 || k == pc1)
  R homnom(v, (O) G(FF(x)));
 M h = (M) Gh(x);
 Wh (*h) h++;
 x = h[-1];
 R (M) x >= Pool && (M) x < Pool+Len ? x :
   x == Ob yield ? Eva :
   nil; }

St Vd rpr(V v, M d, Ko Ch *n, T u) {
 O x, y = pair(v, interns(v, n), nil);
 Mm(y, x = hini(v, 2));
 x = em2(u, y, x);
 tblset(v, *d, X(y), x); }

St Vd rin(V v, M d, Ko Ch *n, T u) {
 O y = interns(v, n);
 tblset(v, *d, y, Pn(u)); }

#define RPR(a,b) rpr(v,&d,a,b)
#define RIN(x) rin(v,&d,"i-"#x,x)
St In O code_dictionary(V v) {
 O d; R d = table(v), Mm(d, prims(RPR), insts(RIN)), d; }
#undef RPR
#undef RIN

St In Vd init_globals_array(V v) {
 Ve t = cells(v, Size(tup) + NGlobs);
 fill(t->xs, nil, t->len = NGlobs);
 O z, y = Glob = puttup(t);
 Mm(y,
  z = code_dictionary(v), Top = z,
  z = table(v), Mac = z,
#define bsym(i,s)(z=interns(v,s),AR(y)[i]=z)
  bsym(Eval, "ev"), bsym(Apply, "ap"),
  bsym(Def, ":"),   bsym(Cond, "?"), bsym(Lamb, "\\"),
  bsym(Quote, "`"), bsym(Seq, ","),  bsym(Splat, ".")); }
#undef bsym


#define NOM "lips"
#define USR_PATH ".local/lib/"NOM"/"
#define SYS_PATH "/usr/lib/"NOM"/"
St int seekp(Ko Ch* p) {
 int b, c;
 b = open(getenv("HOME"), O_RDONLY);
 c = openat(b, USR_PATH, O_RDONLY), close(b);
 b = openat(c, p, O_RDONLY), close(c);
 if (-1 < b) R b;
 b = open(SYS_PATH, O_RDONLY);
 c = openat(b, p, O_RDONLY), close(b);
 R c; }


V bootstrap(V v) {
 if (v == NULL) R v;
 Ko Ch *path = "prelude.lips";
 int pre = seekp(path);
 if (pre == -1) errp(v, "can't find %s", path);
 El {
  Io f = fdopen(pre, "r");
  if (setjmp(v->restart)) R
   errp(v, "error in %s", path),
   fclose(f), finalize(v);
  scr(v, f), fclose(f); }
 R v; }

V initialize(int argc, Ko Ch **argv) {
 V v = malloc(Sz(Sr V));
 if (!v || setjmp(v->restart))
  R errp(v, "oom"), finalize(v);
 v->t0 = clock(),
 v->ip = v->xp = v->syms = v->glob = nil,
 v->fp = v->hp = v->sp = (M) w2b(1),
 v->count = 0, v->mem_len = 1, v->mem_pool = NULL,
 v->mem_root = NULL;
 init_globals_array(v);
 O y = interns(v, "ns");
 tblset(v, Top, y, Top);
 y = interns(v, "macros");
 tblset(v, Top, y, Mac);
 y = interns(v, "argv");
 O a = nil;
 mm(&y); mm(&a);
 Fo (O z; argc--;)
  z = string(v, argv[argc]),
  a = pair(v, z, a);
 um, um, tblset(v, Top, y, a);
 srand(clock());
 R v; }

V finalize(V v) {
 if (v) free(v->mem_pool), free(v);
 R NULL; }

#undef arg
#undef loc
#undef clo

// " the virtual machine "
// it's a stack machine with one free register that's
// implemented on top of the C compiler's calling convention.
// this allows us to keep the most important state variables
// in CPU registers at all times while the interpreter is
// running, without any platform-specific code.

// " the interpreter "
// is all the functions of type terp:
#define Vm(n,...) Nin O \
  n(V v, O ip, M fp, M sp, M hp, O xp, ##__VA_ARGS__)
// the arguments to a terp function collectively represent the
// runtime state, and the  return value is the result of the
// program. there are six arguments because that's the number
// that the prevalent unix calling convention on AMD64 (System
// V ABI) says should be passed in registers; that's the only
// reason why there aren't more. but it's not too bad; the six
// arguments are:
// - v  : vm instance pointer ; most lips functions take this as the first argument
// - ip : instruction pointer ; the current vm instruction ; function pointer pointer
// - fp : frame pointer       ; current function context
// - sp : stack pointer       ; data/call stack
// - hp : heap pointer        ; the next free heap location
// - xp : return value        ; general-purpose register

// when the interpreter isn't running, the state variables that
// would normally be in registers are stored in slots on the
// vm structure. however while the interpreter is running it
// uses these struct slots to pass and return extra values
// without using the stack. so the interpreter has to restore
// the current values in the vm struct before it makes any
// "external" function calls.
#define Pack() (Ip=ip,Sp=sp,Hp=hp,Fp=fp,Xp=xp)
#define Unpack() (fp=Fp,hp=Hp,sp=Sp,ip=Ip,xp=Xp)
#define CallC(...)(Pack(),(__VA_ARGS__),Unpack())

// the frame structure holds the current function context.
Ty Sr fr { O clos, retp, subd, argc, argv[]; } *fr;
#define ff(x)((fr)(x))
#define Clos ff(fp)->clos
#define Retp ff(fp)->retp
#define Subd ff(fp)->subd
#define Argc ff(fp)->argc
#define Argv ff(fp)->argv
// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer.
#define Locs fp[-1]
// if a function has locals, this will have been initialized by the
// by the time they are referred to. the wrinkle in the representation
// gives a small but significant benefit to general function call
// performance and should be extended to the closure pointer, which is often
// nil.

// the return value of a terp function is usually a call
// to another terp function.
#define Jump(f,...) R (f)(v,ip,fp,sp,hp,xp,##__VA_ARGS__)
#define Cont(n, x) R ip+=w2b(n), xp=x, G(ip)(v,ip,fp,sp,hp,xp)
#define Ap(f,x) R ip=f,G(ip)(v,ip,fp,sp,hp,x)
#define Go(f,x) R f(v,ip,fp,sp,hp,x)
#define N(n) ip+=w2b(n);Ap(ip, xp)
// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function
St Vm(gc) { Z n = Xp; CallC(reqsp(v, n)); N(0); }
// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.
#define avail (sp-hp)
#define Have(n) if (avail < n) Jump((Xp=n,gc))
#define Have1() if (hp == sp) Jump((Xp=1,gc)) // common case, faster comparison

// the interpreter takes a very basic approach to error
// handling: something is wrong? jump to nope().
St Vm(nope);
#define TyCh(x,t) if(kind((x)-(t)))Jump(nope) // type check
#define ArCh(n) if(Pn(n)>Argc)Jump(nope)

// " virtual machine instructions "
//
// load instructions
Vm(imm) { xp = Ob GF(ip); N(2); }
// common constants
Vm(unit) { xp = nil;   N(1); }
Vm(one)  { xp = Pn(1); N(1); }
Vm(zero) { xp = Pn(0); N(1); }

// indexed load instructions
// this pointer arithmetic works because fixnums are
// premultiplied by W
#define fast_idx(b) (*(Z*)((Z)(b)+(Z)GF(ip)-Num))
Vm(arg)  { xp = fast_idx(Argv);     N(2); }
Vm(arg0) { xp = Argv[0];            N(1); }
Vm(arg1) { xp = Argv[1];            N(1); }
Vm(loc)  { xp = fast_idx(AR(Locs)); N(2); }
Vm(loc0) { xp = AR(Locs)[0];        N(1); }
Vm(loc1) { xp = AR(Locs)[1];        N(1); }
Vm(clo)  { xp = fast_idx(AR(Clos)); N(2); }
Vm(clo0) { xp = AR(Clos)[0];        N(1); }
Vm(clo1) { xp = AR(Clos)[1];        N(1); }

// store instructions
Vm(push) { Have1(); *--sp = xp; N(1); } // stack push
Vm(loc_) { fast_idx(AR(Locs)) = xp; N(2); } // set a local variable
Vm(tbind) { CallC(tblset(v, Dict, Ob GF(ip), xp)); N(2); }

// initialize local variable slots
Vm(locals) {
  Z n = Gn(GF(ip));
  Have(n + 2);
  Ve t = (Ve) hp;
  fill(t->xs, nil, t->len = n);
  hp += n + 1;
  *--sp = puttup(t);
  N(2); }

// late bind
// this function is a lil complicated, because it incorporates
// the "static" type and arity checking that would have been
// done by the compiler if the function had been bound early.
Vm(lbind) {
 O w = Ob GF(ip),
   d = XY(w), y = X(w);
 if (!(w = tblget(v, d, xp = YY(w)))) Jump(nope); // free variable
 xp = w;
 if (Gn(y) != 8) TyCh(xp, Gn(y)); // do the type check
 T q = G(FF(ip)); // omit the arity check if possible
 if (q == call || q == rec) {
  O aa = Ob GF(FF(ip));
  if (G(xp) == arity && aa >= Ob GF(xp))
   xp += W2; }
 G(ip) = imm;
 GF(ip) = (T) xp;
 N(2); }

// control flow instructions
// return to C
Vm(yield) { R Pack(), xp; }

// conditional jumps
Vm(branch) { Ap(xp == nil ? Ob FF(ip) : Ob GF(ip), xp); }
Vm(barnch) { Ap(xp == nil ? Ob GF(ip) : Ob FF(ip), xp); }
// relational jumps
Vm(brlt)   { Ap(*sp++ <  xp    ? Ob GF(ip) : Ob FF(ip), xp); }
Vm(brgteq) { Ap(*sp++ <  xp    ? Ob FF(ip) : Ob GF(ip), xp); }
Vm(brlteq) { Ap(*sp++ <= xp    ? Ob GF(ip) : Ob FF(ip), xp); }
Vm(brgt)   { Ap(*sp++ <= xp    ? Ob FF(ip) : Ob GF(ip), xp); }
Vm(breq)   { Ap(eql(*sp++, xp) ? Ob GF(ip) : Ob FF(ip), xp); }
Vm(brne)   { Ap(eql(*sp++, xp) ? Ob FF(ip) : Ob GF(ip), xp); }

// unconditional jumps
Vm(jump) { Ap(Ob GF(ip), xp); }
Vm(clos) { Clos = Ob GF(ip); Ap(Ob G(FF(ip)), xp); }

// return from a function
Vm(ret) {
 ip = Retp;
 sp = (M) ((Z) Argv + Argc - Num);
 fp = (M) ((Z)   sp + Subd - Num);
 N(0); }

// regular function call
Vm(call) {
 Have(Size(fr));
 O adic = Ob GF(ip);
 Z off = fp - (M) ((Z) sp + adic - Num);
 fp = sp -= Size(fr);
 Retp = Ph(ip+W2);
 Subd = Pn(off);
 Clos = nil;
 Argc = adic;
 Ap(xp, nil); }

// general tail call
Vm(rec) {
 Z adic = Gn(GF(ip));
 if (Argc == Ob GF(ip)) {
  Fo (M p = Argv; adic--; *p++ = *sp++);
  sp = fp;
  Ap(xp, nil); }

 O off = Subd, rp = Retp; // save return info
 M src = sp + adic;
 // overwrite current frame with new frame
 sp = Argv + Gn(Argc);
 // important to copy in reverse order since they
 // may overlap
 Fo (Z i = adic; i--; *--sp = *--src);
 fp = sp -= Size(fr);
 Retp = rp;
 Argc = Pn(adic);
 Subd = off;
 Clos = nil;
 Ap(xp, nil); }

// type/arity checking
#define arn(n) if(n>Argc)Jump(nope)
#define tcn(k) {if(kind(xp-k))Jump(nope);}
Vm(idZ) { tcn(Num); N(1); }
Vm(id2) { tcn(Two); N(1); }
Vm(idH) { tcn(Hom); N(1); }
Vm(idT) { tcn(Tbl); N(1); }
Vm(arity) { arn(Ob GF(ip)); N(2); }

// continuations
//
// this is a simple but expensive way of doing continuations.
// it would be more memory efficient to do a copy-on-write
// kind of thing where the stack is only copied if the function
// returns. a spaghetti stack would be another option but it
// would be slower. faster continuations at the cost of slower
// function calls seems like a bad deal given the relative
// frequency of the two.
Vm(ccc_u) {
 O x;
 ArCh(1);
 TyCh(x = Argv[0], Hom);
 // we need space for:
 // the entire stack
 // the frame offset
 // the length (to put it all in a tuple)
 // the continuation thread (4 words)
 Z ht = Pool + Len - sp;
 Have(ht + 6);
 Ve t = (Ve) hp;
 hp += ht + 2;
 t->len = ht + 1;
 t->xs[0] = Pn(fp - sp);
 wcpy(t->xs+1, sp, ht);
 H c = (H) hp;
 hp += 4;
 c[0] = cont;
 c[1] = (T) puttup(t);
 c[2] = NULL;
 c[3] = (T) c;
 Argv[0] = Ph(c);
 Ap(x, nil); }

// call a continuation
Vm(cont) {
 Ve t = gettup(GF(ip));
 Have(t->len - 1);
 xp = Gn(Argc) == 0 ? nil : *Argv;
 Z off = Gn(t->xs[0]);
 sp = Pool + Len - (t->len - 1);
 fp = sp + off;
 wcpy(sp, t->xs+1, t->len-1);
 Jump(ret); }

Vm(ap_u) {
 ArCh(2);
 O x = Argv[0], y = Argv[1];
 TyCh(x, Hom);
 Z adic = llen(y);
 Have(adic);
 O off = Subd, rp = Retp;
 sp = Argv + Gn(Argc) - adic;
 Fo (Z j = 0; j < adic; y = Y(y))
  sp[j++] = X(y);
 fp = sp -= Size(fr);
 Retp = rp;
 Argc = Pn(adic);
 Subd = off;
 Clos = nil;
 Ap(x, nil); }

// instructions used by the compiler
Vm(hom_u) {
 O x;
 ArCh(1);
 TyCh(x = *Argv, Num);
 Z len = Gn(x) + 2;
 Have(len);
 H h = (H) hp;
 hp += len;
 fill((M) h, nil, len);
 h[len-1] = (T) h;
 h[len-2] = NULL;
 Go(ret, Ph(h+len-2)); }

Vm(tset) {
 O x = *sp++, y = *sp++;
 CallC(x = tblset(v, xp, x, y));
 Ap(ip+W, x); }

Vm(emx) {
 O h = *sp++ - W;
 G(h) = (T) xp;
 Ap(ip+W, h); }

Vm(emi) {
 O h = *sp++ - W;
 G(h) = (T) Gn(xp);
 Ap(ip+W, h); }
Vm(emx_u) {
 ArCh(2);
 TyCh(Argv[1], Hom);
 O h = Argv[1] - W;
 G(h) = (T) Argv[0];
 Go(ret, h); }
Vm(emi_u) {
 ArCh(2);
 TyCh(Argv[0], Num);
 TyCh(Argv[1], Hom);
 O h = Argv[1] - W;
 G(h) = (T) Gn(Argv[0]);
 Go(ret, h); }
Vm(hom_geti_u) {
 ArCh(1);
 TyCh(Argv[0], Hom);
 Go(ret, Pn(G(Argv[0]))); }
Vm(hom_getx_u) {
 ArCh(1);
 TyCh(Argv[0], Hom);
 Go(ret, Ob G(Argv[0])); }
Vm(hom_seek_u) {
 ArCh(2);
 TyCh(Argv[0], Hom);
 TyCh(Argv[1], Num);
 Go(ret, Ph(Gh(Argv[0])+Gn(Argv[1]))); }

// hash tables
Vm(tblg) {
 ArCh(2);
 TyCh(Argv[0], Tbl);
 xp = tblget(v, Argv[0], Argv[1]);
 Go(ret, xp ? xp : nil); }
Vm(tget) {
 xp = tblget(v, xp, *sp++);
 Ap(ip+W, xp ? xp : nil); }
Vm(thas) {
#define ok Pn(1)
 xp = tblget(v, xp, *sp++) ? ok : nil;
 N(1); }
Vm(tlen) {
 xp = putnum(gettbl(xp)->len);
 N(1); }
Vm(tkeys) { O x;
 CallC(x = tblkeys(v, xp));
 xp = x;
 N(1); }

Vm(tblc) {
 ArCh(2);
 TyCh(Argv[0], Tbl);
 xp = tblget(v, Argv[0], Argv[1]);
 Go(ret, xp ? Pn(0) : nil); }

St O tblss(V v, Z i, Z l) {
 M fp = Fp;
 R i > l-2 ? Argv[i-1] :
  (tblset(v, Xp, Argv[i], Argv[i+1]),
   tblss(v, i+2, l)); }

Vm(tbls) {
 O x = nil;
 ArCh(1);
 TyCh(xp = *Argv, Tbl);
 CallC(x = tblss(v, 1, Gn(Argc)));
 Go(ret, x); }

Vm(tblmk) {
 CallC(Xp = table(v), tblss(v, 0, Gn(Argc)));
 Go(ret, Xp); }

Vm(tbld) {
 O x = nil;
 ArCh(2);
 TyCh(Argv[0], Tbl);
 CallC(x = tbldel(v, Argv[0], Argv[1]));
 Go(ret, x); }
Vm(tblks) {
 ArCh(1);
 TyCh(Argv[0], Tbl);
 O x;
 CallC(x = tblkeys(v, Argv[0]));
 Go(ret, x); }
Vm(tbll) {
 ArCh(1);
 TyCh(Argv[0], Tbl);
 Go(ret, Pn(gettbl(*Argv)->len)); }

// string instructions
Vm(strl) {
 ArCh(1);
 TyCh(*Argv, Oct);
 Go(ret, Pn(getoct(*Argv)->len-1)); }
Vm(strg) {
 ArCh(2);
 TyCh(Argv[0], Oct);
 TyCh(Argv[1], Num);
 Go(ret, Gn(Argv[1]) < getoct(Argv[0])->len-1 ?
  Pn(getoct(Argv[0])->text[Gn(Argv[1])]) :
  nil); }

Vm(strc) {
 Z l = Gn(Argc), sum = 0, i = 0;
 Wh (i < l) {
  O x = Argv[i++];
  TyCh(x, Oct);
  sum += getoct(x)->len - 1; }
 Z words = b2w(sum+1) + 1;
 Have(words);
 By d = (oct) hp;
 hp += words;
 d->len = sum + 1;
 d->text[sum] = 0;
 Wh (i) {
  By x = getoct(Argv[--i]);
  sum -= x->len - 1;
  bcpy(d->text+sum, x->text, x->len - 1); }
 Go(ret, putoct(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(strs) {
 ArCh(3);
 TyCh(Argv[0], Oct);
 TyCh(Argv[1], Num);
 TyCh(Argv[2], Num);

 By src = getoct(Argv[0]);
 Z lb = Gn(Argv[1]), ub = Gn(Argv[2]);
 lb = max(lb, 0), ub = max(min(ub, src->len-1), lb);
 Z words = 1 + b2w(ub - lb + 1);
 Have(words);

 By dst = (By) hp; hp += words;
 dst->len = ub - lb + 1;
 dst->text[ub - lb] = 0;
 bcpy(dst->text, src->text + lb, ub - lb);
 Go(ret, putoct(dst)); }

Vm(strmk) {
 Z i, l = Gn(Argc)+1, size = 1 + b2w(l);
 Have(size);
 By s = (oct) hp;
 hp += size;
 Fo (i = 0; i < l-1; i++) {
  O x = Argv[i];
  TyCh(x, Num);
  if (x == Pn(0)) Bk;
  s->text[i] = Gn(x); }
 s->text[i] = 0;
 s->len = i+1;
 Go(ret, putoct(s)); }

Vm(vararg) {
 Z reqd = Gn(GF(ip)),
   vdic = Gn(Argc) - reqd;
 ArCh(reqd);
 // in this case we need to add another argument
 // slot to hold the nil.
 if (!vdic) {
  Have1();
  sp = --fp;
  Fo (Z i = 0; i < Size(fr) + reqd; i++)
    fp[i] = fp[i+1];
  Argc += W;
  Argv[reqd] = nil; }
 // in this case we just keep the existing slots.
 // the path is knowable at compile time in many cases
 // so maybe vararg should be two or more different
 // functions.
 El {
  Have(2 * vdic);
  Tw t = (two) hp;
  hp += 2 * vdic;
  Fo (Z i = vdic; i--;)
   t[i].x = Argv[reqd + i],
   t[i].y = puttwo(t+i+1);
  t[vdic-1].y = nil;
  Argv[reqd] = puttwo(t); }
 N(2); }

// the next few functions create and store
// lexical environments.
St Vm(encl) {
 Z n = Xp;
 O x = Ob GF(ip);
 M block = hp;
 hp += n;
 O arg = nil; // optional argument array
 if (n > 11) {
  n -= 12;
  Ve t = (Ve) block;
  block += 1 + n;
  t->len = n;
  Wh (n--) t->xs[n] = Argv[n];
  arg = puttup(t); }

 Ve t = (Ve) block; // compiler thread closure array (1 length 5 elements)
 H at = (H) (block+6); // compiler thread (1 instruction 2 data 2 tag)

 t->len = 5; // initialize alpha closure
 t->xs[0] = arg;
 t->xs[1] = xp; // Locs or nil
 t->xs[2] = Clos;
 t->xs[3] = Y(x);
 t->xs[4] = Ph(at);

 at[0] = pc0;
 at[1] = (T) puttup(t);
 at[2] = (T) X(x);
 at[3] = 0;
 at[4] = (T) at;

 Ap(ip+W2, Ph(at)); }

Vm(prencl) {
 Z n = Gn(Argc);
 n += n ? 12 : 11;
 Have(n);
 Xp = n;
 Jump(encl); }

Vm(encll) { Go(prencl, Locs); }
Vm(encln) { Go(prencl, nil); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
Vm(pc0) {
 O ec = Ob GF(ip),
   arg = AR(ec)[0],
   loc = AR(ec)[1];
 Z adic = nilp(arg) ? 0 : AL(arg);
 Have(Size(fr) + adic + 1);
 Z off = (M) fp - sp;
 G(ip) = pc1;
 sp -= adic;
 Fo (Z z = adic; z--; sp[z] = AR(arg)[z]);
 ec = Ob GF(ip);
 fp = sp -= Size(fr);
 Retp = ip;
 Subd = Pn(off);
 Argc = Pn(adic);
 Clos = AR(ec)[2];
 if (!nilp(loc)) *--sp = loc;
 ip = AR(ec)[3];
 N(0); }

// finalize function instance closure
Vm(pc1) {
 G(ip) = clos;
 GF(ip) = (T) xp;
 N(0); }

// this is used to create closures.
Vm(take) {
 Z n = Gn(Ob GF(ip));
 Have(n + 1);
 Ve t = (Ve) hp;
 hp += n + 1;
 t->len = n;
 wcpy(t->xs, sp, n);
 sp += n;
 Go(ret, puttup(t)); }

// print to console
Vm(em_u) {
 Z l = Gn(Argc), i;
 if (l) {
  Fo (i = 0; i < l - 1; i++)
   emsep(v, Argv[i], stdout, ' ');
  emit(v, xp = Argv[i], stdout); }
 fputc('\n', stdout);
 Jump(ret); }

// pairs
Vm(cons) {
 Have1(); hp[0] = xp, hp[1] = *sp++;
 xp = puttwo(hp); hp += 2; N(1); }
Vm(car) { Ap(ip+W, X(xp)); }
Vm(cdr) { Ap(ip+W, Y(xp)); }

Vm(cons_u) {
 Z aa = Gn(Argc);
 if (!aa) Jump(nope);
 Have(2); hp[0] = Argv[0], hp[1] = aa == 1 ? nil : Argv[1];
 xp = puttwo(hp), hp += 2; Jump(ret); }
Vm(car_u) { ArCh(1); TyCh(*Argv, Two); Go(ret, X(*Argv)); }
Vm(cdr_u) { ArCh(1); TyCh(*Argv, Two); Go(ret, Y(*Argv)); }

// arithmetic
Vm(neg) { Ap(ip+W, Pn(-Gn(xp))); }
Vm(add) { xp = xp + *sp++ - Num; N(1); }
Vm(sub) { xp = *sp++ - xp + Num; N(1); }
Vm(mul) { xp = Pn(Gn(xp) * Gn(*sp++)); N(1); }
Vm(dqv) {
 if (xp == Pn(0)) Jump(nope);
 xp = Pn(Gn(*sp++) / Gn(xp));
 N(1); }
Vm(mod) {
 if (xp == Pn(0)) Jump(nope);
 xp = Pn(Gn(*sp++) % Gn(xp));
 N(1); }

#define mm_u(_c,_v,_z,op){\
 O x,m=_z,*xs=_v,*l=xs+_c;\
 if (_c) Fo(;xs<l;m=m op Gn(x)){\
  x = *xs++; TyCh(x, Num);}\
 Go(ret, Pn(m));}
#define mm_u0(_c,_v,_z,op){\
 O x,m=_z,*xs=_v,*l=xs+_c;\
 if (_c) Fo(;xs<l;m=m op Gn(x)){\
  x = *xs++; TyCh(x, Num);\
  if (x == Pn(0)) Jump(nope);}\
 Go(ret, Pn(m));}

Vm(add_u) {
 mm_u(Gn(Argc), Argv, 0, +); }
Vm(mul_u) {
 mm_u(Gn(Argc), Argv, 1, *); }
Vm(sub_u) {
 Z i = Gn(Argc);
 if (i == 0) Go(ret, Pn(0));
 TyCh(*Argv, Num);
 if (i == 1) Go(ret, Pn(-Gn(*Argv)));
 mm_u(i-1,Argv+1,Gn(Argv[0]),-); }

Vm(div_u) {
 Z i = Gn(Argc);
 if (i == 0) Go(ret, Pn(1));
 TyCh(*Argv, Num);
 mm_u0(i-1,Argv+1,Gn(*Argv),/); }
Vm(mod_u) {
 Z i = Gn(Argc);
 if (i == 0) Go(ret, Pn(1));
 TyCh(*Argv, Num);
 mm_u0(i-1,Argv+1,Gn(*Argv),%); }

#define Tf(x) ((x)?ok:nil)
// type predicates
Vm(numpp) { Ap(ip+W, Tf(nump(xp))); }
Vm(hompp) { Ap(ip+W, Tf(homp(xp))); }
Vm(twopp) { Ap(ip+W, Tf(twop(xp))); }
Vm(sympp) { Ap(ip+W, Tf(symp(xp))); }
Vm(strpp) { Ap(ip+W, Tf(octp(xp))); }
Vm(tblpp) { Ap(ip+W, Tf(tblp(xp))); }
Vm(nilpp) { Ap(ip+W, Tf(nilp(xp))); }
Vm(vecpp) { Ap(ip+W, Tf(tupp(xp))); }

// comparison
int eql(O, O);

// pairs are immutable, so we can take this opportunity to
// deduplicate them.
St int twoeq(O a, O b) {
 if (!eql(X(a), X(b))) R 0; El X(a) = X(b);
 if (!eql(Y(a), Y(b))) R 0; El Y(a) = Y(b);
 R 1; }

St int streq(O a, O b) {
 By o = getoct(a), m = getoct(b);
 if (o->len != m->len) R 0;
 Fo (Z i = 0; i < o->len; i++)
  if (o->text[i] != m->text[i]) R 0;
 R 1; }

int eql(O a, O b) {
 if (a == b) R 1;
 if (kind(a) != kind(b)) R 0;
 Sw (kind(a)) {
  Ks Two: R twoeq(a, b);
  Ks Oct: R streq(a, b);
  Df: R 0; } }

Vm(lt)    { xp = *sp++ <  xp ? xp : nil; N(1); }
Vm(lteq)  { xp = *sp++ <= xp ? xp : nil; N(1); }
Vm(gteq)  { xp = *sp++ >= xp ? xp : nil; N(1); }
Vm(gt)    { xp = *sp++ >  xp ? xp : nil; N(1); }
// there should be a separate instruction for simple equality.
Vm(eq)    { xp = eql(xp, *sp++) ? ok : nil; N(1); }

#define ord_w(r){\
 O n=Gn(Argc),*xs=Argv,m,*l;\
 Sw(n){\
  Ks 0: no: Go(ret, nil);\
  Ks 1: Bk;\
  Df: Fo (l=xs+n-1,m=*xs;xs<l;m=*++xs)\
       if(!(m r xs[1])) goto no;}\
 Go(ret, ok);}

#define ord_wv(r){\
 O n=Gn(Argc),*xs=Argv,m,*l;\
 Sw(n){\
  Ks 0: Go(ret, nil);\
  Ks 1: Bk;\
  Df: Fo(l=xs+n-1,m=*xs;xs<l;m=*++xs)\
       if(!(r(m,xs[1]))) Go(ret, nil);}\
 Go(ret, ok);}

#define ord_v(r) Go(ret, ord_u(Gn(Argc), Argv, r))

Vm(lt_u)   { ord_w(<); }
Vm(lteq_u) { ord_w(<=); }
Vm(eq_u)   { ord_wv(eql); }
Vm(gteq_u) { ord_w(>=); }
Vm(gt_u)   { ord_w(>); }

#define typpp(t) {\
 Fo (O *xs = Argv, *l=xs+Gn(Argc);xs<l;)\
  if (kind(*xs++)!=t) Go(ret, nil);\
 Go(ret, ok); }
Vm(nump_u) { typpp(Num); }
Vm(homp_u) { typpp(Hom); }
Vm(strp_u) { typpp(Oct); }
Vm(tblp_u) { typpp(Tbl); }
Vm(twop_u) { typpp(Two); }
Vm(symp_u) { typpp(Sym); }
Vm(nilp_u) { typpp(Nil); }
Vm(vecp_u) { typpp(Tup); }

// stack manipulation
Vm(tuck) { Have1(); sp--, sp[0] = sp[1], sp[1] = xp; N(1); }
Vm(drop) { sp++; N(1); }
Vm(dupl) { Have1(); --sp; sp[0] = sp[1]; N(1); }

// errors
Vm(fail) { Jump(nope); }
Vm(zzz) { exit(EXIT_SUCCESS); }
Vm(gsym_u) {
 Have(Size(sym));
 Sy y = (Sy) hp;
 hp += Size(sym);
 y->nom = y->l = y->r = nil;
 y->code = v->count++ * mix;
 Go(ret, putsym(y)); }

Vm(hfin_u) {
 ArCh(1);
 TyCh(*Argv, Hom);
 O a = *Argv;
 GF(button(Gh(a))) = (T) a;
 Go(ret, a); }

Vm(ev_u) {
 ArCh(1);
 O x;
 CallC(x = compile(v, *Argv),
       x = G(x)(v, x, Fp, Sp, Hp, nil));
 Go(ret, x); }

Vm(rnd_u) { Go(ret, Pn(rand())); }


// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
St In _ perrarg(V v, M fp) {
 Z argc = fp == Pool + Len ? 0 : Gn(Argc), i = 0;
 if (argc) Fo (fputc(' ', stderr);;fputc(' ', stderr)) {
  O x = Argv[i++];
  emit(v, x, stderr);
  if (i == argc) Bk; }
 fputc(')', stderr); }

St Vm(nope) {
 fputs("# (", stderr), emit(v, Ph(ip), stderr),
 perrarg(v, fp);
 fputs(" does not exist\n", stderr);
 Fo (;;) {
  ip = Retp, fp += Size(fr) + Gn(Argc) + Gn(Subd);
  if (button(Gh(ip))[-1] == yield) Bk;
  fputs("#  in ", stderr), emsep(v, Ph(ip), stderr, '\n'); }
 R Hp = hp, restart(v); }

O restart(V v) {
 Fp = Sp = Pool + Len;
 Xp = Ip = nil;
 v->mem_root = NULL;
 longjmp(v->restart, 1); }
