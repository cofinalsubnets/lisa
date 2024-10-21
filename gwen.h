#ifndef _gwen_h
#define _gwen_h
#include <stdint.h>

#ifdef __STDC_HOSTED__
#include <stdio.h>
typedef FILE *gwen_file;
#else
typedef struct gwen_file *gwen_file;
#endif

// thanks !!
typedef intptr_t gwen_word;
typedef uintptr_t gwen_size;
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
  gwen_write1f(gwen_core, gwen_file);
gwen_status
  gwen_read1f(gwen_core, gwen_file),
  gwen_eval(gwen_core);
gwen_word
  pop1(gwen_core);
#endif
