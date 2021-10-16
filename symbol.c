#include "lips.h"
#include "symbol.h"
#include "table.h"

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
 sym z = bump(v, Size(sym));
 z->code = hc(v, z->nom = x), z->l = z->r = nil;
 return *y = putsym(z); }

obj intern(lips v, obj x) {
 if (Avail < Size(sym)) with(x, reqsp(v, Size(sym)));
 return sskc(v, &v->syms, x); }
