#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

// thanks !!

typedef uintptr_t N;
typedef intptr_t ob;

typedef struct mo *mo; // procedure type
typedef struct fr *fr; // frame pointer
typedef struct pt *la, *pt; // runtime point
#define Vm(n, ...)\
  ob n(pt v, ob xp, mo ip, ob *hp, ob *sp, fr fp)
#define Ll Vm
#define Dt Ll
typedef Vm(vm);
typedef vm host, ll;

// FIXME 2bit
#define TagBits 3
#define TagMask ((1<<TagBits)-1)
enum class { Hom, Num, Two, Str, Sym, Tbl, };
#define NomHom "hom"
#define NomNum "num"
#define NomTwo "two"
// FIXME ext
#define NomTbl "tbl"
// FIXME principled reason to separate sym & str?
#define NomStr "str"
#define NomSym "sym"

typedef struct str { ob ext; intptr_t len; char text[]; } *str;
typedef struct sym { ob nom, code, l, r; } *sym;
typedef struct two { ob a, b; } *two;
typedef struct tbl { ob *tab, len, cap; } *tbl;

struct fr { ob clos, retp, subd, argc, argv[]; };
struct mo { vm *ll; };

// language symbols
enum lex {
  Def, Cond, Lamb, Quote, Seq, Splat,
  Eval, Apply, LexN };

// linked list for gc protection
typedef struct keep { ob *it; struct keep *et; } *keep;

struct pt {
  // vm state -- kept in CPU registers most of the time
  mo ip; // current thread
  fr fp; // top of control stack
  ob xp, // free register
     *hp, // top of heap
     *sp; // top of data stack
          // sp - hp = free memory

  // memory state
  keep keep; // list of C stack addresses to copy on gc
  intptr_t t0, // gc timestamp, governs len
           len, // memory pool size
           *pool; // memory pool

  // other runtime state
  ob wns, // working namespace -- a stack of dicts
     sns, // system namespace -- a dict of dicts
     syms, // internal symbols
     rand, // random seed
     lex[LexN]; }; // grammar symbols

// runtime constructor/destructor
pt la_ini(void);
void la_fin(pt);

// pairs
ob pair(pt, ob, ob);
size_t llen(ob);
intptr_t lidx(ob, ob);

// hash tables
size_t hash(pt, ob);
ob table(pt),
   tbl_set(pt, ob, ob, ob),
   tbl_get(pt, ob, ob);

// strings & symbols
ob string(pt, const char*),
   intern(pt, ob);

// functions
ob hnom(pt, ob); // FIXME try to get function name
mo ana(pt, ob, ob), // compiler interface
   button(mo); // get tag at end
               //
#define Push(...) pushs(v, __VA_ARGS__, (ob) 0)
bool
  pushs(pt, ...),
  please(pt, size_t), // gc interface
  eql(ob, ob); // logical equality

ob refer(pt, ob), // FIXME these should be private
   sskc(pt, ob*, ob); // FIXME

#define N0 putnum(0)
#define nil N0
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define A(o) get2(o)->a
#define B(o) get2(o)->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define Avail (v->sp-v->hp)
#define mm(r) ((v->keep=&((struct keep){(r),v->keep})))
#define um (v->keep=v->keep->et)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Width(t) b2w(sizeof(struct t))

#define nilp(_) ((_)==nil)

#define F(_) ((mo)(_)+1)
#define G(_) (((mo)(_))->ll)

#define putstr(_) ((ob)(_)+Str)
#define getnum getZ
#define putnum putZ
#define getZ(_) ((ob)(_)>>TagBits)
#define putZ(_) (((ob)(_)<<TagBits)+Num)
#define getstr(_) ((str)((_)-Str))
#define puthom(_) ((ob)(_))
#define gethom(_) ((mo)(_))
#define getsym getY
#define putsym putY
#define getY(_) ((sym)((_)-Sym))
#define putY(_) ((ob)(_)+Sym)
#define gettbl(_) ((tbl)((_)-Tbl))
#define puttbl(_) ((ob)(_)+Tbl)
#define get2(_) ((two)((_)-Two))
#define put2(_) ((ob)(_)+Two)

#define ptr(x) ((ob*)(x))
#define R ptr
#define T putZ(-1)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

#define Q TypeOf
#define TypeOf(_) (((ob)(_))&TagMask)
#define nump(_) (TypeOf(_)==Num)
#define strp(_) (TypeOf(_)==Str)
#define twop(_) (TypeOf(_)==Two)
#define tblp(_) (TypeOf(_)==Tbl)
#define homp(_) (TypeOf(_)==Hom)
#define symp(_) (TypeOf(_)==Sym)

static Inline size_t b2w(size_t b) {
  size_t quot = b / sizeof(ob),
         rem = b % sizeof(ob);
  return rem ? quot + 1 : quot; }

static Inline void setw(void *x, intptr_t i, uintptr_t l) {
  for (intptr_t *d = x; l--; *d++ = i); }

static Inline void cpyw(void *x, const void *y, uintptr_t l) {
  intptr_t *d = x;
  const intptr_t *s = y;
  while (l--) *d++ = *s++; }

static Inline void rcpyw(void *x, const void *y, uintptr_t l) {
  intptr_t *d = (ob*) x + (l - 1);
  const intptr_t *s = (const intptr_t*) y + (l - 1);
  while (l--) *d-- = *s--; }

// unchecked allocator -- make sure there's enough memory!
static Inline void *bump(pt v, intptr_t n) {
  void *x = v->hp;
  v->hp += n;
  return x; }

static Inline void *cells(pt v, uintptr_t n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

static Inline intptr_t lcprng(intptr_t s) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }

// XXX FIXME XXX
_Static_assert(sizeof(intptr_t) == 8, "64bit");
_Static_assert(-1 == -1 >> 1, "signed >>");
