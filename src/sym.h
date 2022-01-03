typedef struct sym { obj nom, code, l, r; } *sym;
obj
 intern(lips, obj),
 interns(lips, const char*),
 sskc(lips, mem, obj);
#define symnom(y) chars(getsym(y)->nom)
#define Y(x) getsym(x)
#define _Y(x) putsym(x)
static Inline sym getsym(obj x) { return (sym) (x - Sym); }
static Inline obj putsym(void *y) { return (obj) y + Sym; }
static Inline bool symp(obj x) { return kind(x) == Sym; }
