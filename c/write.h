#ifndef _write_h
#define _write_h
u0 emit(lips, obj, FILE*);
u1 write_file(lips, const char*, const char*);
static Inline u0 emsep(lips v, obj x, FILE *o, char s) {
  emit(v, x, o), fputc(s, o); }
#endif
