#ifndef _li_h
#define _li_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

_Static_assert(-1 >> 1 == -1, "signed shift");
_Static_assert(sizeof(size_t) == sizeof(void*), "size_t");

struct V;
enum status {
  Eof = -1, Ok,
  DomainError, ArityError,
  NameError, SyntaxError,
  SystemError, OomError
} li_ini(struct V*);

void li_fin(struct V*), li_unwind(struct V*);

#endif
