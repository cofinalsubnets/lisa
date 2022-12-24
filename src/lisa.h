#ifndef _lisa_h
#define _lisa_h

struct carrier;
enum status {
  LA_EOF = -1,
  LA_OK,
  LA_XDOM,
  LA_XARY,
  LA_XNOM,
  LA_XSYN,
  LA_XSYS,
  LA_XOOM };

enum status la_ini(struct carrier*);
void la_fin(struct carrier*);

#endif
