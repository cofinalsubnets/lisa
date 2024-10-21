Makefile=Makefile
nom=gwen
suff=gw
bin=./$(nom).tco.bin
bin2=./$(nom).tcn.bin
boot=lib/pre.$(suff)
test=$(sort $(wildcard test/*.$(suff)))
bbt=$(bin) $(boot) $(test)
b2bt=$(bin2) $(boot) $(test)

#build
c=$(sort $(wildcard *.c))
h=$(sort $(wildcard *.h))
o=$(c:.c=.tco.o)
o2=$(c:.c=.tcn.o)
CC=clang
CFLAGS=\
	-std=gnu11 -g -O2 -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-asynchronous-unwind-tables -fno-stack-protector

default: test_all
test_all: $(bin) $(bin2) test test2
libgwen.tco.a: gwen.tco.o
	ar rcs libgwen.tco.a gwen.tco.o
libgwen.tcn.a: gwen.tcn.o
	ar rcs libgwen.tcn.a gwen.tcn.o
$(bin): main.tco.o libgwen.tco.a
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) main.tco.o -L. -lgwen.tco -o $@
$(bin2): main.tcn.o libgwen.tcn.a
	$(CC) $(CPPFLAGS) $(CFLAGS) -DGwenCanUseTco=0 $(LDFLAGS) main.tcn.o -L. -lgwen.tcn -o $@
%.tcn.o: %.c $h $(Makefile)
	$(CC) -c $(CPPFLAGS) $(CFLAGS) -DGwenCanUseTco=0 $(LDFLAGS) $< -o $@
%.tco.o: %.c $h $(Makefile)
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $< -o $@

test: $(bin)
	@echo '[tail call optimized]'
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(bbt)"
test2: $(bin2)
	@echo '[trampolining]'
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(b2bt)"

# install
DESTDIR ?= $(HOME)
PREFIX ?= .local
dest=$(DESTDIR)/$(PREFIX)/
bins=$(dest)bin/$(nom)
slib=$(dest)lib/lib$(nom).a
libs=$(addprefix $(dest)lib/$(nom)/,$(notdir $(wildcard lib/*)))
header=$(dest)include/$(nom).h
#docs=$(dest)share/man/man1/$(nom).1
files=$(bins) $(libs) $(slib) $(docs) $(header)
install: $(files)
uninstall:
	rm -f $(files)
$(dest)bin/$(nom): $(bin)
	install -D -m 755 $^ $@
$(dest)share/%: %
	install -D -m 644 $^ $@
$(dest)lib/$(nom)/%: lib/%
	install -D -m 644 $^ $@
$(dest)lib/lib$(nom).a: lib$(nom).tco.a
	install -D -m 644 $^ $@
$(header): $(nom).h
	install -D -m 644 $^ $@

# other tasks
#
clean:
	rm -r `git check-ignore * */*`
# valgrind detects some memory errors
valg: $(bin)
	valgrind --error-exitcode=1 $(bbt)
# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) * test/* lib/*
# size of binaries
bits: $(bin) $(bin2)
	du -h $^
disasm: $(bin)
	rizin -A $^
# profiling on linux
perf.data: $(bin)
	perf record $(bbt)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<
repl: $(bin)
	rlwrap $(bin) -i $(boot)

.PHONY: default clean test test-lots\
	install uninstall\
 	sloc bits valg perf bench flame disasm repl
