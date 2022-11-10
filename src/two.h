#ifndef _la_two_h
#define _la_two_h

struct two {
  struct header head;
  ob a, b;
};

#define A(o) ((two)(o))->a
#define B(o) ((two)(o))->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))

two pair(la, ob, ob);
intptr_t lidx(ob, ob);
size_t llen(ob);

static Inline two ini_two(void *_, ob a, ob b) {
  two w = _;
  w->head.disp = disp;
  w->head.mtbl = &mtbl_two;
  w->a = a, w->b = b;
  return w; }

#endif
