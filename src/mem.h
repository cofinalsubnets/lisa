#define Avail (v->sp-v->hp)
#define GC(n) obj n(lips v, obj x, u64 len0, mem base0)
#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((mem)(o),v->pool,v->pool+v->len)
#define stale(o) inb((mem)(o),base0,base0+len0)
#define COPY(dst,src) (dst=cp(v,src,len0,base0))
#define CP(x) COPY(x,x)
#define mm(r) ((v->root=&((struct root){(r),v->root})))
#define um (v->root=v->root->next)
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define Width(t) b2w(sizeof(struct t))

struct root { mem one; struct root *next; };
u0 *bump(lips, u64), *cells(lips, u64);
bool cycle(lips, u64);
typedef obj copier(lips, obj, u64, mem);
copier cphom, cpvec, cptwo, cpsym, cpstr, cptbl, cp;
