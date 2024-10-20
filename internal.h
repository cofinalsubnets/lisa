#ifndef _gwen_internal_h
#define _gwen_internal_h
#include "gwen.h"
#include <stdbool.h>

typedef gwen_word word, *gwen_stack, *gwen_heap;
typedef union gwen_cell *gwen_cell, *gwen_thread;

#ifdef GwenCanUseTco
#define Vm(n, ...) gwen_status n(gwen_core f, gwen_cell _ip, gwen_heap _hp, gwen_stack _sp, ##__VA_ARGS__)
#define Hp _hp
#define Sp _sp
#define Ip _ip
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#else
#define Vm(n, ...) gwen_status n(gwen_core f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f)
#define Unpack(f)
#endif
typedef Vm(gwen_vm);
typedef gwen_vm vm;

// basic data types
typedef struct gwen_pair *gwen_pair;
typedef struct gwen_string *gwen_string;
typedef struct gwen_symbol *gwen_symbol;
typedef struct gwen_table *gwen_table;
// runtime core data structure -- 1 core = 1 thread of execution
struct gwen_core {
  // vm registers
  union gwen_cell {
    gwen_vm *ap;
    gwen_word x;
    gwen_cell m; } *ip;
  gwen_heap hp; // heap pointer
  gwen_stack sp; // stack pointer
  // environment
  gwen_table dict, macro; // global environment and macros
  gwen_symbol symbols; // internal symbols
                  //
  // memory management
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

// thanks !!
typedef union gwen_cell *cell, *thread;
typedef FILE *gwen_file;
typedef gwen_file input, output, gwen_input, gwen_output;
typedef uintptr_t gwen_size;

typedef bool
  gwen_equal_function(gwen_core, gwen_word, gwen_word);
typedef gwen_word
  gwen_copy_function(gwen_core, gwen_word, gwen_word*, gwen_word*),
  gwen_hash_function(gwen_core, gwen_word);
typedef void
  gwen_evac_function(gwen_core, gwen_word, gwen_word*, gwen_word*),
  gwen_print_function(gwen_core, FILE*, gwen_word);
// basic data type method table
typedef struct gwen_type {
  gwen_copy_function *copy;
  gwen_evac_function *evac;
  gwen_equal_function *equal;
  gwen_print_function *emit;
  gwen_hash_function *hash;
} *typ;

#define Oom GwenStatusOom
#define Ok GwenStatusOk
#define Eof GwenStatusEof

#endif
