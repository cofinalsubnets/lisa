typedef struct str { u64 len; char text[]; } *str;
obj string(lips, const char*);
#define S(x) getstr(x)
#define _S(x) putstr(x)
#define getstr(x) ((str)((obj)(x)-Str))
#define putstr(x) ((obj)(x)+Str)
#define strp(x) (kind(x)==Str)
#define chars(x) S(x)->text
