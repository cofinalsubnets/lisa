struct str { u64 len; char text[]; };
#define chars(x) getstr(x)->text
obj string(lips, const char*);
