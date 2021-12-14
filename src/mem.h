#define Avail (Sp-Hp)
#define balloc(t) bump(v, Width(t))
#define GC(n) obj n(lips v, obj x, u64 len0, mem base0)
#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((mem)(o),v->pool,v->pool+v->len)
#define stale(o) inb((mem)(o),base0,base0+len0)
#define COPY(dst,src) (dst=cp(v,src,len0,base0))
#define CP(x) COPY(x,x)

u0 reqsp(lips, u64);

typedef obj copier(lips, obj, u64, mem);
copier cphom, cptup, cptwo, cpsym, cpstr, cptbl, cp;
 
static Inline u0* bump(lips v, u64 n) {
  u0* x = v->hp;
  return v->hp += n, x; }
   
static Inline u0* cells(lips v, u64 n) {
  if (Avail < n) reqsp(v, n);
  return bump(v, n); }
