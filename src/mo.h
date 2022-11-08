#ifndef _la_mo_h
#define _la_mo_h

// every dynamically allocated thread ends
// with a footer holding a pointer to its head
typedef struct tag {
  void *null; // always null
  struct la_fn
    *head, // pointer to head of thread
    end[]; // first address after thread
} *tag, *la_fn_tag;

la_fn mkmo(la_carrier, size_t); // allocate a thread
la_fn_tag motag(la_fn); // get tag at end
la_ob hnom(la_carrier, la_fn);

bool primp(la_fn); // is it a primitive function?
static Inline la_fn ini_mo(void *_, size_t len) {
  la_fn k = _;
  la_fn_tag t = (la_fn_tag) (k + len);
  t->null = NULL, t->head = k;
  return k; }

#endif
