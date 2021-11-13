#include "lips.h"
#include "sym.h"
#include "tbl.h"
#include "str.h"
#include "mem.h"
#include "terp.h"

//symbols

// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult. the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection.
static Inline obj ssk(lips v, obj y, obj x) {
  sym z = getsym(y);
  int i = scmp(chars(z->nom), chars(x));
  return i == 0 ? y : sskc(v, i < 0 ? &z->r : &z->l, x); }

obj sskc(lips v, mem y, obj x) {
  if (!nilp(*y)) return ssk(v, *y, x);
  sym z = bump(v, Width(sym));
  z->code = hash(v, z->nom = x) ^ mix;
  z->l = z->r = nil;
  return *y = putsym(z); }

obj intern(lips v, obj x) {
  if (Avail < Size(sym)) with(x, reqsp(v, Size(sym)));
  return sskc(v, &v->syms, x); }

obj interns(lips v, const char *s) {
  return intern(v, string(v, s)); }

VM(gsym_u) {
  if (ARGC > Pn(0) && strp(*ARGV))
    RETC(v->xp = intern(v, *ARGV));
  Have(Width(sym));
  sym y = (sym) hp;
  hp += Size(sym);
  y->nom = y->l = y->r = nil;
  y->code = v->count++ * mix;
  GO(ret, putsym(y)); }
