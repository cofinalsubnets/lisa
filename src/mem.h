#define Avail (v->sp-v->hp)
#define balloc(t) bump(v, Width(t))
#define GC(n) obj n(lips v, obj x, u64 len0, mem base0)
#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((mem)(o),v->pool,v->pool+v->len)
#define stale(o) inb((mem)(o),base0,base0+len0)
#define COPY(dst,src) (dst=cp(v,src,len0,base0))
#define CP(x) COPY(x,x)

u0 reqsp(lips, u64), *bump(lips, u64), *cells(lips, u64);
typedef obj copier(lips, obj, u64, mem);
copier cphom, cptup, cptwo, cpsym, cpstr, cptbl, cp;
