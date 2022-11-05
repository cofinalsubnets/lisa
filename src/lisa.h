// liblisa definitions
#ifndef _lisa_h
#define _lisa_h
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

enum la_status {
  LA_OK = 0,
  LA_DOMAIN_ERROR,
  LA_ARITY_ERROR,
  LA_PARSE_ERROR,
  LA_SYSTEM_ERROR,
  LA_OUT_OF_MEMORY,
  LA_OUT_OF_BOUNDS, };

typedef struct la_carrier *la_carrier;
typedef intptr_t la_point;

bool la_open(la_carrier);
void la_close(la_carrier);

bool
  la_lib(la_carrier, const char*),
  la_script(la_carrier, const char*);

la_point
  la_ev(la_carrier, la_point), // eval a value
  la_rx(la_carrier, FILE*); // read a value

long la_tx(la_carrier, FILE*, la_point); // write a value

#endif
