#ifndef _gwen_h
#define _gwen_h
#include <stdint.h>
#include <stdio.h>
// thanks !!
typedef intptr_t gwen_word;
typedef struct gwen_core *gwen_core;
typedef enum gwen_status {
  GwenStatusEof = -1,
  GwenStatusOk = 0,
  GwenStatusOom = 1,
} gwen_status;
gwen_core
  gwen_close(gwen_core),
  gwen_open(void);
void
  gwen_write1f(gwen_core, FILE*);
gwen_status
  gwen_read1f(gwen_core, FILE*),
  gwen_eval(gwen_core);
gwen_word
  pop1(gwen_core);
#endif
