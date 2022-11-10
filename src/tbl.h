#ifndef _la_tbl_h
#define _la_tbl_h

// hash tables
struct tbl {
  struct header head;
  size_t len, cap;
  ob *tab; };

// hash tables
tbl mktbl(la),
    tbl_set(la, tbl, ob, ob);
ob tbl_get(la, tbl, ob, ob);

#endif
