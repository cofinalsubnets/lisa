#ifndef _num_h
#define _num_h
static Inline u1 nump(obj x) { return kind(x) == Num; }
static Inline i64 getnum(obj x) { return x >> 3; }
static Inline obj putnum(i64 n) { return (n << 3) + Num; }
#define N(x) getnum(x)
#define _N(x) putnum(x)
#endif
