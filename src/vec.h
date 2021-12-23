typedef struct vec { u64 len; obj xs[]; } *vec;
#define V(x) getvec(x)
#define _V(x) putvec(x)
#define getvec(x) ((vec)((x)-Vec))
#define putvec(x) ((obj)(x)+Vec)
#define vecp(x) (kind(x)==Vec)
