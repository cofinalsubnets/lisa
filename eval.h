typedef obj c1(vm, mem, num),
            c2(vm, mem, num, obj),
            c3(vm, mem, num, obj, obj);
static Inline obj em1(terp *i, obj k) {
  return k -= Word, G(k) = i, k; }
static Inline obj em2(terp *i, obj j, obj k) {
  return em1(i, em1((terp*)j, k)); }
obj hom_ini(vm, num);
#define N(x) putnum(x)
#define Gn(x) getnum(x)
