#ifndef _empath_h
#define _empath_h
#include <stdint.h>
#include <stdbool.h>

struct em;
struct em *ini(void);
void fin(struct em*);

typedef intptr_t ob;

#endif
