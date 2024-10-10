#ifndef _l_h
#define _l_h
#include <stdint.h>
#include <stdbool.h>
typedef intptr_t l_word;
typedef struct l_core *l_core;
typedef enum l_status {
  l_status_eof = -1,
  l_status_ok = 0,
  l_status_oom = 1,
} l_status;
l_core l_open(void);
void l_close(l_core);
#endif
