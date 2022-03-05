#ifndef _tbl_h
#define _tbl_h
typedef struct ent { obj key, val; struct ent *next; } *ent; // tables
typedef struct tbl { u64 len, cap; ent *tab; } *tbl;
u64 hash(lips, obj);
obj tblkeys(lips, obj),
    table(lips),
    tbl_set(lips, obj, obj, obj),
    tbl_set_s(lips, obj, obj, obj),
    tbl_get(lips, obj, obj);
static Inline tbl gettbl(obj x) { return (tbl) (x - Tbl); }
static Inline obj puttbl(tbl t) { return (obj) t + Tbl; }
static Inline u1 tblp(obj x) { return kind(x) == Tbl; }
#define mix ((u64)2708237354241864315)
#define T(x) gettbl(x)
#define _T(x) puttbl(x)
#endif
