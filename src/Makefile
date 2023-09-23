Makefile=Makefile
nom=lisa
#build
LC_COLLATE=C
c=$(sort $(wildcard *.c))
h=$(sort $(wildcard *.h))
CPPFLAGS += -Dtesting
o=$(c:.c=.o)
CFLAGS ?=\
	-std=gnu11 -g -Os -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector

test: $(nom)
	./$(nom) </dev/null

%.o: %.c $h $(Makefile)
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $< -o $@
$(nom): $o $h
	$(CC) -o $@ $o $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)

clean:
	rm -r `git check-ignore *`

.PHONY: test clean
