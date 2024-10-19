#ifndef _gwen_h
#define _gwen_h
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

// thanks !!
// basic data types
typedef intptr_t gwen_word, *gwen_stack, *gwen_heap;
typedef union gwen_cell *gwen_cell;
typedef struct gwen_pair *gwen_pair;
typedef struct gwen_string *gwen_string;
typedef struct gwen_symbol *gwen_symbol;
typedef struct gwen_table *gwen_table;
typedef struct gwen_core *gwen_core;
typedef enum gwen_status {
  Eof = -1,
  Ok = 0,
  Oom = 1,
} gwen_status;

// character input interface
typedef struct gwen_input {
  int (*getc)(struct gwen_core*, struct gwen_input*);
  void (*ungetc)(struct gwen_core*, struct gwen_input*, char);
  bool (*eof)(struct gwen_core*, struct gwen_input*);
  intptr_t data[];
} *gwen_input;

// character output interface
typedef struct gwen_output {
  void (*putc)(struct gwen_core*, struct gwen_output*, char);
  intptr_t data[];
} *gwen_output;

typedef bool gwen_gc_method(gwen_core, size_t);

// runtime core data structure -- 1 core = 1 thread of execution
struct gwen_core {
  // vm registers
  gwen_cell ip; // instruction pointer
  gwen_heap hp; // heap pointer
  gwen_stack sp; // stack pointer
  // environment
  gwen_table dict, macro; // global environment and macros
  gwen_word rand; // random seed
  gwen_symbol symbols; // internal symbols
                  //
  gwen_input in;
  gwen_output out, err;

  // memory management
  bool (*please)(struct gwen_core*, uintptr_t); // gc routine
  gwen_word len, // size of each pool
            *pool, // on pool
            *loop; // off pool
  struct gwen_mm { // gc save list
    gwen_word *addr; // stack address of value
    struct gwen_mm *next; // prior list
  } *safe;
  union { // gc state
    uintptr_t t0; // end time of last gc
    gwen_heap cp; }; };

gwen_gc_method gwen_static_gc;

#ifdef __STDC_HOSTED__
#include <stdio.h>
#include <stdlib.h>
#define gwen_malloc malloc
#define gwen_free free
gwen_status read1f(gwen_core, FILE*);
gwen_core gwen_open(void);
void gwen_close(gwen_core);
gwen_gc_method gwen_dynamic_gc;
extern struct gwen_input std_input;
extern struct gwen_output std_output, std_error;
#else
// otherwise you need to define these
void *gwen_malloc(size_t), gwen_free(void*);
#endif

void
  transmit(gwen_core, gwen_output, gwen_word),
  report(gwen_core, gwen_output, gwen_status),
  reset_stack(gwen_core);
gwen_status
  gwen_ini(gwen_core, gwen_gc_method*, size_t, gwen_word*, gwen_input, gwen_output, gwen_output),
  eval(gwen_core),
  read1i(gwen_core, gwen_input);
gwen_word
  pop1(gwen_core);
#endif
