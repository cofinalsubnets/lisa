#define Avail (v->sp-v->hp)
#define mm(r) ((v->root=&((struct root){(r),v->root})))
#define um (v->root=v->root->next)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Width(t) b2w(sizeof(struct t))

u0 *cells(lips, u64);
u1 cycle(lips, u64);
