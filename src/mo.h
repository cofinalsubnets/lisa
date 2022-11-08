#ifndef _la_mo_h
#define _la_mo_h

// every dynamically allocated thread ends
// with a footer holding a pointer to its head
typedef struct tag {
  void *null; // always null
  struct mo
    *head, // pointer to head of thread
    end[]; // first address after thread
} *tag;

mo mkmo(la, size_t); // allocate a thread
tag motag(mo); // get tag at end
ob hnom(la, mo);

bool primp(mo); // is it a primitive function?
static Inline mo ini_mo(void *_, size_t len) {
  mo k = _;
  tag t = (tag) (k + len);
  t->null = NULL, t->head = k;
  return k; }

#endif
