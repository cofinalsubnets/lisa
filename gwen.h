#ifndef _gwen_h
#define _gwen_h
#include <stdint.h>
#include <stdbool.h>

// thanks !!
// basic data type
typedef intptr_t gwen_word;
typedef struct gwen_core *gwen_core;
typedef enum gwen_status {
  Eof = -1,
  Ok = 0,
  Oom = 1,
} gwen_status;


// character input interface
typedef struct gwen_char_in {
  int (*getc)(gwen_core, struct gwen_char_in*);
  void (*ungetc)(gwen_core, struct gwen_char_in*, char);
  bool (*eof)(gwen_core, struct gwen_char_in*);
  intptr_t data[];
} *gwen_input, *input;

// character output interface
typedef struct gwen_char_out {
  void (*putc)(gwen_core, struct gwen_char_out*, char);
  intptr_t data[];
} *gwen_output, *output;

extern struct gwen_char_in std_input;
extern struct gwen_char_out std_output, std_error;

gwen_core
  gwen_open(void);
void
  gwen_close(gwen_core),
  transmit(gwen_core, gwen_output, gwen_word),
  report(gwen_core, gwen_output, gwen_status),
  reset_stack(gwen_core);
gwen_status
  eval(gwen_core),
  read1i(gwen_core, gwen_input);
gwen_word
  pop1(gwen_core);
#endif
