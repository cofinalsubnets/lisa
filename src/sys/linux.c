#include "../i.h"

void *l_malloc(size_t n) { return malloc(n); }
void l_free(void *n) { return free(n); }
