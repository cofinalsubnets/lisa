#ifndef _la_sym_h
#define _la_sym_h

// FIXME this is a silly way to store internal symbols
// - it's slower than a hash table
// - anonymous symbols waste 2 words
struct sym {
  struct header head;
  str nom;
  intptr_t code;
  struct sym *l, *r;
};
sym symof(la, str);

#endif
