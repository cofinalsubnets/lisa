// liblisa definitions
#ifndef _lisa_h
#define _lisa_h
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

typedef enum {
  LA_EOF = -1,
  LA_OK,
  LA_XDOM,
  LA_XARY,
  LA_XSYN,
  LA_XSYS,
  LA_XOOM,
} la_status;

typedef struct la_carrier *la_carrier;
typedef intptr_t la_point;

la_status la_open(la_carrier);
void la_close(la_carrier);

#endif
