#include "lisa.h"
#include "vm.h"

#define prim_ent(go, nom) { go, nom },
struct prim primitives[] = { i_primitives(prim_ent) };

bool define_primitives(la v) {
  struct prim *p = primitives,
              *lim = p + LEN(primitives);
  for (;p < lim; p++) {
    ob z = interns(v, p->nom);
    if (!z || !tbl_set(v, v->wns, z, (ob) p)) return false; }
  return true; }

// function functions
// allocator
mo mkmo(la v, size_t n) {
  mo k = cells(v, n+2);
  if (k) k[n].ll = 0, k[n+1].ll = (vm*) k;
  return k; }

// get the tag at the end of a function
mo button(mo k) { return G(k) ? button(F(k)) : k; }

// try to get the name of a function
ob hnom(la v, ob x) {
  vm *k = gethom(x)->ll;
  if (k == clos || k == clos0 || k == clos1)
    return hnom(v, (ob) gethom(x)[2].ll);
  ob* h = (ob*) gethom(x);
  while (*h) h++;
  x = h[-1];
  int inb = (ob*) x >= v->pool && (ob*) x < v->pool+v->len;
  return inb ? x : nil; }

static void emhomn(la v, FILE *o, ob x) {
  if (symp(x)) fputc('\\', o), tx(v, o, x);
  else if (!twop(x)) fputc('\\', o);
  else { // FIXME this is weird
    if (symp(A(x)) || twop(A(x))) emhomn(v, o, A(x));
    if (symp(B(x)) || twop(B(x))) emhomn(v, o, B(x)); } }

bool primp(ob x) {
  struct prim *_ = (struct prim*) x;
  return _ >= primitives && _ < primitives + LEN(primitives); }

void emhom(la v, FILE *o, ob x) {
  if (primp(x)) fprintf(o, "\\%s", ((struct prim*)x)->nom);
  else emhomn(v, o, hnom(v, x)); }

// instructions for the internal compiler
Vm(hom_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Num);
  size_t len = getnum(xp) + 2;
  Have(len);
  xp = (ob) hp;
  hp += len;
  setw((ob*) xp, nil, len);
  ptr(xp)[len-2] = 0;
  ptr(xp)[len-1] = xp;
  return ApC(ret, (ob) (ptr(xp) + len - 2)); }

Vm(hfin_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Hom);
  button((mo)xp)[1].ll = (vm*) xp;
  return ApC(ret, xp); }

Vm(emx) {
  mo k = (mo) *sp++ - 1;
  k->ll = (vm*) xp;
  return ApN(1, (ob) k); }

Vm(emi) {
  mo k = (mo) *sp++ - 1;
  k->ll = (vm*) getnum(xp);
  return ApN(1, (ob) k); }

Vm(emx_u) {
  ArityCheck(2);
  TypeCheck(xp = Argv[1], Hom);
  xp = (ob) (ptr(xp) - 1);
  ptr(xp)[0] = Argv[0];
  return ApC(ret, xp); }

Vm(emi_u) {
  ob n;
  ArityCheck(2);
  TypeCheck(n = Argv[0], Num);
  TypeCheck(xp = Argv[1], Hom);
  xp = (ob) (ptr(xp) - 1);
  ptr(xp)[0] = getnum(n);
  return ApC(ret, xp); }

Vm(peeki_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Hom);
  return ApC(ret, putnum(gethom(xp)->ll)); }

Vm(peekx_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Hom);
  return ApC(ret, (ob) gethom(xp)->ll); }

Vm(seek_u) {
  ArityCheck(2);
  TypeCheck(xp = Argv[0], Hom);
  TypeCheck(Argv[1], Num);
  return ApC(ret, xp + Argv[1] - Num); }
