#ifndef _gwen_h
#define _gwen_h
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

// thanks !!
typedef intptr_t gwen_word;
typedef struct gwen_core *gwen_core;
typedef enum gwen_status {
  GwenStatusEof = -1,
  GwenStatusOk = 0,
  GwenStatusOom = 1,
} gwen_status;

// character i/o interfaces interface
typedef struct gwen_input *gwen_input;
typedef struct gwen_output  *gwen_output;
extern struct gwen_input std_input;
extern struct gwen_output std_output, std_error;

gwen_core
  gwen_open(void);
void
  gwen_close(gwen_core),
  transmit(gwen_core, gwen_output, gwen_word),
  report(gwen_core, gwen_output, gwen_status),
  reset_stack(gwen_core);
gwen_status
  read1f(gwen_core, FILE*),
  read1i(gwen_core, gwen_input),
  eval(gwen_core);
gwen_word
  pop1(gwen_core);
#endif
