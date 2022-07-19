#include <stdio.h> // FIXME use syscalls
// read/write s-expressions
ob rx(pt, FILE*), rxq(pt, FILE*);
void tx(pt, FILE*, ob);
