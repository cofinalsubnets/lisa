#ifndef _empathy_h
#define _empathy_h
#include <stdint.h>
typedef intptr_t ob;
struct em;
struct em *ini(void);
void fin(struct em*);
#endif
