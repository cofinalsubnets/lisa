#ifndef _empath_h
#define _empath_h
#include <stdint.h>

typedef intptr_t ob;
struct em;

struct em *ini(void);
void fin(struct em*);
#endif
