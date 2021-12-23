typedef struct ent { obj key, val; struct ent *next; } *ent; // tables
struct tbl { u64 len, cap; ent *tab; };
#define mix ((u64)2708237354241864315)
u64 hash(lips, obj);
obj
  tblkeys(lips, obj),
  table(lips),
  tbl_set(lips, obj, obj, obj),
  tbl_set_s(lips, obj, obj, obj),
  tbl_get(lips, obj, obj);
