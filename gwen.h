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
  int (*getc)(struct gwen_core*, struct gwen_char_in*);
  void (*ungetc)(struct gwen_core*, struct gwen_char_in*, char);
  bool (*eof)(struct gwen_core*, struct gwen_char_in*);
  intptr_t data[];
} *input;

// character output interface
typedef struct gwen_char_out {
  void (*putc)(struct gwen_core*, struct gwen_char_out*, char);
  intptr_t data[];
} *output;

extern struct gwen_char_in std_input;
extern struct gwen_char_out std_output, std_error;

void
  transmit(gwen_core, struct gwen_char_out*, gwen_word),
  report(gwen_core, struct gwen_char_out*, gwen_status),
  reset_stack(gwen_core);
void gwen_close(gwen_core);
gwen_core gwen_open(void);
gwen_status
  eval(gwen_core),
  read1i(gwen_core, input);
gwen_word pop1(struct gwen_core*);
