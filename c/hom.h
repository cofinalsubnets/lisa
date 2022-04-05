#ifndef _hom_h
#define _hom_h
static Inline hom F(hom h) { return h + 1; }
static Inline terp *G(hom h) { return *h; }
static Inline hom gethom(obj x) { return (hom) (x - Hom); }
static Inline obj puthom(hom h) { return (obj) h + Hom; }
static Inline hom button(hom h) { while (*h) h++; return h; }
static Inline u1 homp(obj x) { return kind(x) == Hom; }
#define H(x)  gethom(x)
#define _H(x) puthom(x)
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
obj eval(lips, obj), homnom(lips, obj), analyze(lips, obj), sequence(lips, obj, obj);
#endif
