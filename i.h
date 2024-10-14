#include <stdint.h>
#include <stdbool.h>
#include <string.h>

// thanks !!
// basic data type
typedef intptr_t word, *heap, *stack;
// one thread of execution
typedef struct l_core *l_core, *state, *core;
typedef union cell *cell, *thread;
typedef enum l_status {
  Eof = -1,
  Ok = 0,
  Oom = 1,
} l_status, status, vm(core, thread, heap, stack);

// also represented as a union for structured access
typedef struct symbol *symbol;

// character input interface
typedef struct char_in {
  int (*getc)(core, struct char_in*);
  void (*ungetc)(core, struct char_in*, char);
  bool (*eof)(core, struct char_in*);
  word data[];
} *input;

// character output interface
typedef struct char_out {
  void (*putc)(core, struct char_out*, char);
  word data[];
} *output;

extern struct char_in std_input;
extern struct char_out std_output, std_error;

void
  transmit(core, output, word),
  report(core, output, status),
  reset_stack(core),
  l_close(core);
core l_open(void);
status
  eval(core),
  read1i(core, input);
word pop1(core);
