static Inline hom F(hom h) { return h + 1; }
static Inline terp *G(hom h) { return *h; }
static Inline hom gethom(obj x) { return (hom) (x - Hom); }
static Inline obj puthom(hom h) { return (obj) h + Hom; }
static Inline hom button(hom h) { while (*h) h++; return h; }
static Inline bool homp(obj x) { return kind(x) == Hom; }
#define H(x)  gethom(x)
#define _H(x) puthom(x)
obj eval(lips, obj), homnom(lips, obj);
