#define Avail (Sp-Hp)
#define balloc(t) bump(v, Width(t))
u0 reqsp(lips, u64);
static Inline u0* bump(lips v, u64 n) {
 u0* x = v->hp;
 return v->hp += n, x; }
static Inline u0* cells(lips v, u64 n) {
 if (Avail < n) reqsp(v, n);
 return bump(v, n); }
