#include "lips.h"
#include "terp.h"
#include "hom.h"
#include "err.h"

// this is used to create closures.
VM(take) {
 u64 n = Gn((obj) GF(ip));
 Have(n + 1);
 vec t = (vec) hp;
 hp += n + 1;
 t->len = n;
 cpy64(t->xs, sp, n);
 sp += n;
 GO(ret, putvec(t)); }

VM(vset_u) {
 ARY(3);
 TC(ARGV[0], Vec);
 TC(ARGV[1], Num);
 num idx = getnum(ARGV[1]);
 vec ary = getvec(ARGV[0]);
 if (idx < 0 || idx >= ary->len) Jump(nope, oob_err_msg, idx, ary->len);
 GO(ret, ary->xs[idx] = ARGV[2]); }

VM(vget_u) {
 ARY(2);
 TC(ARGV[0], Vec);
 TC(ARGV[1], Num);
 num idx = getnum(ARGV[1]);
 vec ary = getvec(ARGV[0]);
 if (idx < 0 || idx >= ary->len) Jump(nope, oob_err_msg, idx, ary->len);
 GO(ret, ary->xs[idx]); }

VM(vec_u) {
 obj n = N(ARGC);
 Have(n + 1);
 vec t = (vec) hp;
 hp += 1 + n;
 cpy64(t->xs, ARGV, t->len = n);
 GO(ret, putvec(t)); }
