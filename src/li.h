#ifndef _l_h
#define _l_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct V;
enum status {
  Eof = -1, Ok,
  DomainError, ArityError,
  NameError, SyntaxError,
  SystemError, OomError
} li_ini(struct V*);

void li_fin(struct V*), li_unwind(struct V*);

#endif
