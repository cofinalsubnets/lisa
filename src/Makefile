include ../config.mk

CC ?= gcc
CPPFLAGS ?= -DPREFIX=\"$(PREFIX)\"
# fixnums need sign extended bitshifts.
# other things tend to break TCO ...
CFLAGS ?= -std=c99 -g -O2 -flto\
	-Wall -Wstrict-prototypes\
	-Wno-shift-negative-value\
	-fno-stack-protector\
	-fno-unroll-loops

# build config
#
n=lips#                          | binary name
b=$n.bin#                        | dev binary

# target directory
T=../bin
# dist binary
$T/$n: $T/$b
	strip -o $@ $^
# dev binary
$T/$b: $(patsubst %.c,%.o, $(sort $(wildcard *.c)))
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^
