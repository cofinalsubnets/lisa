#ifndef _lisa_h
#define _lisa_h

enum la_status;
struct la_carrier;

enum la_status la_open(struct la_carrier*);
void la_close(struct la_carrier*);

#endif
