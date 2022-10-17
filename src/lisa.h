#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

// thanks !!

typedef intptr_t ob;

typedef struct la *la; // what lists act on
// frame pointer
typedef struct fr { ob clos, retp, subd, argc, argv[]; } *fr;
typedef struct mo *mo; // procedures
#define Vm(n, ...) ob n(la v, ob xp, mo ip, ob *hp, ob *sp, fr fp)
typedef Vm(vm);
struct mo { vm *ll; };

// FIXME stop using tagged pointers!
// - it assumes pointer alignment that limits the platforms we can run on
// - it stops us from using cheney's algorithm to gc in constant stack
// instead just use the least significant bit to distinguish immediate values
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

// pairs
typedef struct two { ob a, b; } *two;

// strings
typedef struct str { vm *disp; size_t len; char text[]; } *str;
// TODO pre-hash strings for faster lookup & comparison

// symbols
typedef struct sym { ob nom, code, l, r; } *sym;
// FIXME this is a silly way to do internal symbols
// - it's slower than a hash table
// - anonymous symbols waste 2 words

// hash tables
typedef struct tbl { ob *tab; uintptr_t len, cap; } *tbl;

// TODO include type data
typedef struct dtbl {
  vm *ap;
  void (*show)(la, ob, FILE*);
  ob (*gc)(la, ob, size_t, ob*);
  // ob dyn; // TODO do we want this?
} *dtbl;
typedef struct ext { vm *disp; dtbl dtbl; ob data[]; } *ext;


// grammar symbols
enum lex { Def, Cond, Lamb, Quote, Seq, Splat, Eval, LexN };

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

// pairs
ob pair(la, ob, ob);
size_t llen(ob);

// hash tables
size_t hash(la, ob);
ob table(la),
   tbl_set(la, ob, ob, ob),
   tbl_get(la, ob, ob);

// strings & symbols
ob string(la, const char*), intern(la, ob), interns(la, const char*),
  sskc(la, ob*, ob); // FIXME a symbol-interning function that should be private


// functions
mo mkmo(la, size_t), // allocator
   ana(la, ob, ob), // compiler interface
   button(mo); // get tag at end
               //
#define Push(...) pushs(v, __VA_ARGS__, (ob) 0)
bool
  define_primitives(la),
  primp(ob),
  pushs(la, ...),
  please(la, size_t), // gc interface
  eql(ob, ob); // logical equality

ob hnom(la, ob); // FIXME try to get function name
ob rx(la, FILE*); // read sexp
void tx(la, FILE*, ob), // write sexp
     emhom(la, FILE*, ob);
void *cells(la, size_t); // allocate memory

// internal libc substitutes
intptr_t lcprng(intptr_t);
void setw(void*, uintptr_t, size_t),
     cpyw(void*, const void*, size_t),
     rcpyw(void*, const void*, size_t),
     cpy8(void*, const void*, size_t);
char cmin(char);
size_t slen(const char*);
int scmp(const char*, const char*);

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
ob nope(la, const char*, ...) NoInline; // runtime error

#define TypeOf(_) (((ob)(_))&TagMask)
#define nump(_) (TypeOf(_)==Num)
#define strp(_) (TypeOf(_)==Str)
#define twop(_) (TypeOf(_)==Two)
#define tblp(_) (TypeOf(_)==Tbl)
#define homp(_) (TypeOf(_)==Hom)
#define symp(_) (TypeOf(_)==Sym)

#define err nope
#define LEN(ary) (sizeof(ary)/sizeof(*ary))

static Inline size_t b2w(size_t b) {
  size_t quot = b / sizeof(ob),
         rem = b % sizeof(ob);
  return rem ? quot + 1 : quot; }

struct prim { vm *go; const char *nom; };
extern struct prim primitives[];

// XXX FIXME XXX
_Static_assert(sizeof(ob) == 8, "64bit");
_Static_assert(-1 == -1 >> 1, "signed >>");
_Static_assert(sizeof(ob) == sizeof(size_t), "size_t matches address space");
