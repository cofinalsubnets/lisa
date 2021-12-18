obj eval(lips, obj), homnom(lips, obj);
#define F(x) ((hom)(x)+1)
#define G(x) (*(hom)(x))
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
static Inline hom button(hom h) { while (*h) h++; return h; }
