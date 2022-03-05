#ifndef _two_h
#define _two_h
typedef struct two { obj a, b; } *two;
obj pair(lips, obj, obj);
u64 llen(obj);
#define A(o) gettwo(o)->a
#define B(o) gettwo(o)->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
//#define W(x) gettwo(x)
//#define _W(w) puttwo(w)

static Inline two gettwo(obj x) { return (two) (x - Two); }
static Inline obj puttwo(u0 *x) { return (obj) x + Two; }
static Inline u1 twop(obj x) { return kind(x) == Two; }
#endif
