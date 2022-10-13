#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

// XXX FIXME XXX
_Static_assert(sizeof(intptr_t) == 8, "64bit");
_Static_assert(-1 == -1 >> 1, "signed >>");

// thanks !!

typedef intptr_t ob;

typedef struct mo *mo; // procedure type
typedef struct fr *fr; // frame pointer
typedef struct la *la; // what lists act on
#define Vm(n, ...)\
  ob n(la v, ob xp, mo ip, ob *hp, ob *sp, fr fp)
typedef Vm(vm);

// FIXME 2bit
#define TagBits 3
#define TagMask ((1<<TagBits)-1)
enum class { Hom, Num, Two, Str, Sym, Tbl, };
#define NomHom "hom"
#define NomNum "num"
#define NomTwo "two"
#define NomTbl "tbl"
// FIXME principled reason to separate sym & str?
#define NomStr "str"
#define NomSym "sym"

typedef struct str { ob ext; uintptr_t len; char text[]; } *str;
typedef struct sym { ob nom, code, l, r; } *sym;
typedef struct two { ob a, b; } *two;
typedef struct tbl { ob *tab; uintptr_t len, cap; } *tbl;

typedef struct vtbl {
  vm *ap;
  ob (*gc)(la, ob, size_t, ob*),
     dtbl; } *vtbl;
typedef struct dyn { vm *go; vtbl vt; ob dat[]; } *dyn;

struct fr { ob clos, retp, subd, argc, argv[]; };
struct mo { vm *ll; };

// language symbols
enum lex {
  Def, Cond, Lamb, Quote, Seq, Splat, Eval, LexN };

// linked list for gc protection
typedef struct keep { ob *it; struct keep *et; } *keep;

struct la {
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
     syms, // internal symbols
     rand, // random seed
     lex[LexN]; }; // grammar symbols

// runtime constructor/destructor
la la_ini(void);
void la_fin(la);

// pairs
ob pair(la, ob, ob);
size_t llen(ob);

// hash tables
size_t hash(la, ob);
ob table(la),
   tbl_set(la, ob, ob, ob),
   tbl_get(la, ob, ob);

// strings & symbols
ob string(la, const char*),
   intern(la, ob);

// functions
ob hnom(la, ob); // FIXME try to get function name
mo ana(la, ob, ob), // compiler interface
   button(mo); // get tag at end
#define Push(...) pushs(v, __VA_ARGS__, (ob) 0)
bool
  pushs(la, ...),
  please(la, size_t), // gc interface
  eql(ob, ob); // logical equality

ob sskc(la, ob*, ob); // FIXME ugly

#define N0 putnum(0)
#define nil N0
#define FF(x) F(F(x))
#define GF(x) G(F(x))
#define A(o) gettwo(o)->a
#define B(o) gettwo(o)->b
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
#define getZ getnum
#define putZ putnum
#define getnum(_) ((ob)(_)>>TagBits)
#define putnum(_) (((ob)(_)<<TagBits)+Num)
#define getstr(_) ((str)((_)-Str))
#define puthom(_) ((ob)(_))
#define gethom(_) ((mo)(_))
#define getsym(_) ((sym)((_)-Sym))
#define putsym(_) ((ob)(_)+Sym)
#define gettbl(_) ((tbl)((_)-Tbl))
#define puttbl(_) ((ob)(_)+Tbl)
#define gettwo(_) ((two)((_)-Two))
#define puttwo(_) ((ob)(_)+Two)

#define ptr(x) ((ob*)(x))
#define R ptr
#define T putnum(-1)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

#define TypeOf(_) (((ob)(_))&TagMask)
#define nump(_) (TypeOf(_)==Num)
#define strp(_) (TypeOf(_)==Str)
#define twop(_) (TypeOf(_)==Two)
#define tblp(_) (TypeOf(_)==Tbl)
#define homp(_) (TypeOf(_)==Hom)
#define symp(_) (TypeOf(_)==Sym)

ob err(la, const char*, ...) NoInline;

static Inline size_t b2w(size_t b) {
  size_t quot = b / sizeof(ob),
         rem = b % sizeof(ob);
  return rem ? quot + 1 : quot; }

// unchecked allocator -- make sure there's enough memory!
static Inline void *bump(la v, intptr_t n) {
  void *x = v->hp;
  v->hp += n;
  return x; }

static Inline void *cells(la v, size_t n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

#define mkthd mkmo
mo mkmo(la, size_t);

// lib
intptr_t lcprng(intptr_t);
void setw(void*, intptr_t, size_t),
     cpyw(void*, const void*, size_t),
     rcpyw(void*, const void*, size_t);
char cmin(char);
size_t slen(const char*);
int scmp(const char*, const char*);
