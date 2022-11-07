// liblisa definitions
#ifndef _lisa_h
#define _lisa_h

typedef enum la_status {
  LA_EOF = -1,
  LA_OK,
  LA_XDOM,
  LA_XARY,
  LA_XSYN,
  LA_XSYS,
  LA_XOOM,
} la_status;

typedef struct la_carrier *la_carrier;

enum la_status la_open(la_carrier);
void la_close(la_carrier);

#endif
