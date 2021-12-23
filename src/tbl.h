typedef struct ent { obj key, val; struct ent *next; } *ent; // tables
typedef struct tbl { u64 len, cap; ent *tab; } *tbl;
u64 hash(lips, obj);
obj
  tblkeys(lips, obj),
  table(lips),
  tbl_set(lips, obj, obj, obj),
  tbl_set_s(lips, obj, obj, obj),
  tbl_get(lips, obj, obj);
#define mix ((u64)2708237354241864315)
#define T(x) gettbl(x)
#define _T(x) puttbl(x)
#define gettbl(x) ((tbl)((obj)(x)-Tbl))
#define puttbl(x) ((obj)(x)+Tbl)
#define tblp(x) (kind(x)==Tbl)
