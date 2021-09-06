#include "pre.h"
// freestanding substitutes for some libc functionality.
//
// mem{set,cpy} analogs
#define memn(n)\
 u0 set##n(u0*_d,u##n i,u64 l) {\
  for(u##n*d=_d;l--;*d++=i); }\
 u0 cpy##n(u0*_d,const u0*_s, u64 l) {\
  u##n*d=_d; const u##n*s=_s;\
  while (l--) *d++=*s++; }\
 u0 cpy##n##r(u0*_d,const u0*_s, u64 l) {\
  u##n*d=_d; const u##n*s=_s;\
  while (l--) d[l]=s[l]; }\
 u0 mov##n(u0*_d,const u0*_s, u64 l) {\
  if (_d<_s) cpy##n(_d, _s, l);\
  else if (_d>_s) cpy##n##r(_d, _s, l); }
BWDQ(memn)
