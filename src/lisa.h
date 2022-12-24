#ifndef _lisa_h
#define _lisa_h

typedef struct la_carrier *la_carrier;
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

enum la_status la_ini(struct la_carrier *);
void la_fin(struct la_carrier *);

#endif
