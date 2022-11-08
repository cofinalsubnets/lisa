#ifndef _la_str_h
#define _la_str_h

struct str {
  struct header head;
  size_t len;
  char text[];
};

str strof(la, const char*);

static Inline str ini_str(void *_, size_t len) {
  str s = _;
  s->head.disp = disp;
  s->head.mtbl = &mtbl_str;
  s->len = len;
  return s; }

#endif
