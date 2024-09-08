#include "i.h"

static Inline symbol ini_sym(void *_, string nom, uintptr_t code) {
  symbol y = _; return
    y->ap = data, y->typ = &symbol_type,
    y->nom = nom, y->code = code,
    y->l = y->r = 0, y; }

static Inline symbol ini_anon(void *_, word code) {
  symbol y = _;
  y->ap = data, y->typ = &symbol_type;
  y->nom = 0, y->code = code;
  return y;  }

static word hash_symbol(core v, word _) { return ((symbol) _)->code; }
static word copy_symbol(core f, word x, word *p0, word *t0) {
  symbol src = (symbol) x,
         dst = src->nom ?
           intern(f, (string) cp(f, (word) src->nom, p0, t0)) :
           ini_anon(bump(f, Width(struct symbol) - 2), src->code);
  return (word) (src->ap = (vm*) dst); }
static void walk_symbol(core f, word x, word *p0, word *t0) {
  f->cp += Width(struct symbol) - (((symbol)x)->nom ? 0 : 2); }

  /*
static bool atomp(string s) {
  const char cc[] = " \n\t;#()\"'";
  for (size_t i = 0; i < s->len; i++)
    for (const char *c = cc; *c; c++)
      if (s->text[i] == *c) return false;
  return true; }
  */

static void tx_symbol(core f, FILE *o, word _) {
  symbol s = (symbol) _;
  fputs(s->nom ? s->nom->text : "#anon", o); }

bool literal_equal(core f, word a, word b) {
  return a == b;
}

struct typ symbol_type = {
  .hash = hash_symbol,
  .copy = copy_symbol,
  .evac = walk_symbol,
  .equal = literal_equal,
  .emit = tx_symbol,
};


static symbol intern_r(core v, string b, symbol *y) {
  if (*y) {
    symbol z = *y;
    string a = z->nom;
    int i = strncmp(a->text, b->text,
      a->len < b->len ? a->len : b->len);
    if (i == 0) {
      if (a->len == b->len) return z;
      i = a->len < b->len ? -1 : 1; }
    return intern_r(v, b, i < 0 ? &z->l : &z->r); }
  return *y = ini_sym(bump(v, Width(struct symbol)), b,
    hash(v, putnum(hash(v, (word) b)))); }

symbol intern(core f, string b) {
  // make sure there's enough memory now because we need stable
  // pointers if and when we allocate the new symbol
  return avail(f) >= Width(struct symbol) || f->please(f, Width(struct symbol)) ?
    intern_r(f, b, &f->symbols): 0; }

Vm(gensym) {
  const int req = Width(struct symbol) - 2;
  Have(req);
  symbol y = (symbol) hp;
  hp += req;
  return op(1, (word) ini_anon(y, f->rand = liprng(f))); }

Vm(string_of_symbol) {
  word x = sp[0];
  if (!symp(x)) return op(1, nil);
  string y = ((symbol) x)->nom;
  return op(1, y ? (word) y : nil); }

Vm(symbol_of_string) {
  word x = sp[0];
  if (!strp(x)) return op(1, nil);
  Pack(f);
  symbol y = intern(f, (string) x);
  Unpack(f);
  return !y ? Oom : op(1, (word) y); }
