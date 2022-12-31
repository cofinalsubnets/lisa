#ifndef _li_h
#define _li_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

_Static_assert(-1 >> 1 == -1, "signed shift");
_Static_assert(sizeof(size_t) == sizeof(void*), "size_t");

struct V;
enum status { Eof = -1, Ok,
              DomainError, ArityError,
              NameError, SyntaxError,
              SystemError, OomError }
  la_ini(struct V*),
  la_go(struct V*);
void la_fin(struct V*);

#endif
