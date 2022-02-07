#include "lips.h"
#include "sym.h"
#include "str.h"

//symbols

// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult. the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection.
obj interns(lips v, const char *s) {
  return intern(v, string(v, s)); }

#include "mem.h"
#include "tbl.h"
obj sskc(lips v, mem y, obj x) {
  if (!nilp(*y)) {
    sym z = getsym(*y);
    int i = scmp(S(z->nom)->text, S(x)->text);
    return i == 0 ? *y : sskc(v, i < 0 ? &z->r : &z->l, x); }
  sym z = bump(v, Width(sym));
  z->code = hash(v, z->nom = x) ^ mix;
  z->l = z->r = nil;
  return *y = putsym(z); }

obj intern(lips v, obj x) {
  if (Avail < Width(sym)) {
    u1 ok;
    with(x, ok = cycle(v, Width(sym)));
    if (!ok) return 0; }
  return sskc(v, &v->syms, x); }

#include "terp.h"
Vm(gsym_u) {
  if (Argc > _N(0) && strp(*Argv))
    RetC(v->xp = intern(v, *Argv));
  Have(Width(sym));
  sym y = (sym) hp;
  hp += Width(sym);
  y->nom = y->l = y->r = nil;
  y->code = v->count++ * mix;
  Go(ret, putsym(y)); }

Vm(ystr_u) {
 Ary(1);
 xp = *Argv;
 Tc(xp, Sym);
 Go(ret, getsym(xp)->nom); }
