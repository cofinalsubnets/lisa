#define F(x) (H(x)+1)
#define G(x) (*H(x))
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define H(x)  gethom(x)
#define _H(x) puthom(x)
#define gethom(x) ((hom)((x)-Hom))
#define puthom(x) ((obj)((x)+Hom))
#define homp(x) (kind(x)==Hom)
obj eval(lips, obj),
    homnom(lips, obj);
static Inline hom button(hom h) { while (*h) h++; return h; }
