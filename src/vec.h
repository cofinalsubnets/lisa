typedef struct vec { u64 len; obj xs[]; } *vec;
#define V(x) getvec(x)
#define _V(x) putvec(x)
static Inline vec getvec(obj x) { return (vec) (x - Vec); }
static Inline obj putvec(vec v) { return (obj) v + Vec; }
static Inline u1 vecp(obj x) { return kind(x) == Vec; }
