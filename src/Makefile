default:
	make -C .. test

%:
	make -C .. $@

LC_COLLATE=C
h=$(sort $(wildcard *.h))
c=$(sort $(wildcard *.c))
o=$($c:.c=.o)

CFLAGS ?=\
	-std=c99 -g -O2 -flto -fpic -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector

../build/%: ../build/%.dbg
	strip --strip-unneeded -o $@ $^

../build/%.dbg: $o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

%.o: %.c $h Makefile
	$(CC) -c -o $@ $(CFLAGS) $(CPPFLAGS) $<
