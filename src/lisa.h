#ifndef _lisa_h
#define _lisa_h
#include <stdint.h>

typedef intptr_t la_ob;
typedef struct la_carrier *la_carrier;
typedef struct la_fn *la_fn;

typedef enum la_status {
  LA_EOF = -1,
  LA_OK,
  LA_XDOM,
  LA_XARY,
  LA_XNOM,
  LA_XSYN,
  LA_XSYS,
  LA_XOOM,
} la_status;

la_status la_open(la_carrier);
void la_close(la_carrier);

#endif
