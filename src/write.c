#include "lips.h"
#include "write.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include "str.h"
#include "terp.h"

u0 ems(lips v, FILE *o, obj x, char s) {
 emit(v, o, x), fputc(s, o); }

u0 emsep(lips v, obj x, FILE *o, char s) {
 emit(v, o, x), fputc(s, o); }

#include "tbl.h"
#include "hom.h"
#include "vec.h"
static u0 emnil(lips v, FILE *o, obj x) { fputs("()", o); }

u0 emit(lips v, FILE *o, obj x) {
  static emitter *emitters[] = {
    [Hom] = emhom,
    [Tbl] = emtbl,
    [Num] = emnum,
    [Str] = emstr,
    [Sym] = emsym,
    [Nil] = emnil,
    [Vec] = emvec,
    [Two] = emtwo, };
  emitters[kind(x)](v, o, x); }

// print to console
Vm(em_u) {
  u64 l = N(Argc), i;
  if (l) {
    for (i = 0; i < l - 1; i++)
      emsep(v, Argv[i], stdout, ' ');
    emit(v, stdout, xp = Argv[i]); }
  fputc('\n', stdout);
  Jump(ret); }

Vm(putc_u) {
  Ary(1);
  fputc(N(*Argv), stdout);
  Jump(ret); }

Vm(dump) {
  Ary(2);
  Tc(Argv[0], Str);
  Tc(Argv[1], Str);
  char *p = S(Argv[0])->text,
       *d = S(Argv[1])->text;
  write_file(v, p, d);
  Jump(ret); }
