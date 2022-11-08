#ifndef _la_gc_h
#define _la_gc_h

// linked list for gc protection
struct keep {
  void **addr;
  struct keep *next; };

#define Gc(n) ob n(la v, ob x, ob *pool0, ob *top0)

#define mm(r) \
  ((v->safe = &((struct keep){(void**)(r), v->safe})))
#define um (v->safe = v->safe->next)
#define with(y,...) (mm(&(y)), (__VA_ARGS__), um)

bool please(la, size_t); // ask GC for available memory
ob cp(la, ob, ob*, ob*); // copy something; used by type-specific copying functions

#endif
