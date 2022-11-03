// liblisa definitions
#ifndef _lisa_h
#define _lisa_h
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct la *la;
typedef intptr_t la_ob;

// initialize / deinitialize a runtime instance
bool la_open(la);
void la_close(la);
// as above but manage the instance with malloc / free
la la_ini(void);
void la_fin(la);

bool la_script(la, const char*);

la_ob
  la_ev(la, la_ob), // eval a value
  la_rx(la, FILE*); // read a value

long la_tx(la, FILE*, la_ob); // write a value

#endif
