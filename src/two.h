typedef struct two { obj a, b; } *two;
obj pair(lips, obj, obj);
u64 llen(obj);
#define A(o) gettwo(o)->a
#define B(o) gettwo(o)->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) ((obj)(x)+Two)
#define twop(x) (kind(x)==Two)
