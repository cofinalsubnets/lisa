#ifndef _la_status_h
#define _la_status_h

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

#endif
