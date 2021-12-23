typedef struct sym { obj nom, code, l, r; } *sym;
obj
 intern(lips, obj),
 interns(lips, const char*),
 sskc(lips, mem, obj);
#define symnom(y) chars(getsym(y)->nom)
#define Y(x) getsym(x)
#define _Y(x) putsym(x)
#define getsym(x) ((sym)((obj)(x)-Sym))
#define putsym(x) ((obj)(x)+Sym)
#define symp(x) (kind(x)==Sym)
