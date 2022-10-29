#ifndef _lisa_h
#define _lisa_h
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct la *la;
typedef intptr_t la_ob;

la la_ini(void);
void la_fin(la);

la_ob
  la_ev_x(la, la_ob),
  la_ev_f(la, FILE*),
  la_ev_s(la, const char **),
  la_rx_f(la, FILE*),
  la_rx_s(la, const char **);

int la_tx_f(la, FILE*, la_ob);
char *la_tx_s(la, la_ob);

bool
  la_nilp(la_ob),
  la_nump(la_ob),
  la_strp(la_ob),
  la_twop(la_ob),
  la_tblp(la_ob),
  la_homp(la_ob);

intptr_t la_getnum(la_ob);
char *la_getstr(la_ob);
la_ob la_get0(la_ob), la_get1(la_ob);
#endif
