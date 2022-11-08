#ifndef _la_tbl_h
#define _la_tbl_h

// hash tables
struct tbl {
  struct header head;
  size_t len, cap;
  ob *tab;
};

// hash tables
tbl mktbl(la),
    tblset(la, tbl, ob, ob);
ob tblget(la, tbl, ob);

#endif
