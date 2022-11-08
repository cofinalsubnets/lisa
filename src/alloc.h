#ifndef _la_alloc_h
#define _la_alloc_h

#define Avail (v->sp - v->hp)

void *bump(la_carrier, size_t), // allocate memory unchecked
     *cells(la_carrier, size_t); // allocate memory checked

bool pushs(la, ...); // push args onto stack
ob tupl(la, ...); // collect args into tuple (data thread)

#endif
