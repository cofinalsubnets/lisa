// liblisa definitions
#ifndef _lisa_h
#define _lisa_h
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct la *la;
typedef intptr_t la_ob;

la la_ini(void);

void
  la_fin(la),
  la_atpanic(la, la_ob (*)(la));

la_ob
  la_ev_x(la, la_ob),
  la_ev_f(la, FILE*),
  la_rx_f(la, FILE*);

int la_tx_f(la, FILE*, la_ob);
#endif
