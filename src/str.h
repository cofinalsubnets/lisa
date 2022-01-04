typedef struct str { u64 len; char text[]; } *str;
obj string(lips, const char*);
static Inline str getstr(obj x) { return (str) (x - Str); }
static Inline obj putstr(str s) { return (obj) s + Str; }
static Inline bool strp(obj x) { return kind(x) == Str; }
#define S(x) getstr(x)
#define _S(x) putstr(x)
