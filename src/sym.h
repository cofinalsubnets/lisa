struct sym { obj nom, code, l, r; };
obj
 intern(lips, obj),
 interns(lips, const char*),
 sskc(lips, mem, obj);
#define symnom(y) chars(getsym(y)->nom)
