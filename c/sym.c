#include "lips.h"
#include "terp.h"
#include <string.h>

//symbols

// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult. the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection.
ob interns(en v, const char *s) {
  ob _;
  bind(_, string(v, s));
  return intern(v, _); }


// FIXME this is too bad
ob sskc(en v, ob*y, ob x) {
  sym z;
  if (!nilp(*y)) {
    z = getsym(*y);
    int i = strcmp(S(z->nom)->text, S(x)->text);
    return i == 0 ? *y : sskc(v, i < 0 ? &z->r : &z->l, x); }
  // the caller must ensure Avail >= Width(sym) because to GC
  // here would cause the tree to be rebuilt
  z = cells(v, Width(sym));
  z->code = hash(v, z->nom = x) ^ mix;
  z->l = z->r = nil;
  return *y = putsym(z); }

ob intern(en v, ob x) {
  if (Avail < Width(sym)) {
    u1 o;
    with(x, o = please(v, Width(sym)));
    bind(o, o); }
  return sskc(v, &v->syms, x); }

#include "terp.h"
Vm(gsym_u) {
  if (Argc > _N(0) && strp(*Argv)) {
    CallC(v->xp = intern(v, *Argv));
    Jump(ret); }
  Have(Width(sym));
  sym y = (sym) hp;
  hp += Width(sym);
  y->nom = y->l = y->r = nil;
  y->code = v->rand = lcprng(v->rand);
  Go(ret, putsym(y)); }

Vm(ystr_u) {
  Arity(1);
  xp = *Argv;
  CheckType(xp, Sym);
  Go(ret, getsym(xp)->nom); }
