#ifndef _sym_h
#define _sym_h
typedef struct sym { obj nom, code, l, r; } *sym;
obj
 intern(lips, obj),
 interns(lips, const char*),
 sskc(lips, mem, obj);
#define Y(x) getsym(x)
#define _Y(x) putsym(x)
static Inline sym getsym(obj x) { return (sym) (x - Sym); }
static Inline obj putsym(u0 *y) { return (obj) y + Sym; }
static Inline u1 symp(obj x) { return kind(x) == Sym; }
#endif
