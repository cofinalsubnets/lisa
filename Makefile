Makefile=Makefile
nom=lem
suff=l
bin=./$(nom)
boot=lib/pre.$(suff)
test=test/*.$(suff)
bbt=$(bin) $(boot) $(test)

#build
c=$(sort $(wildcard *.c))
h=$(sort $(wildcard *.h))
o=$(c:.c=.o)
CFLAGS ?=\
	-std=gnu11 -g -Os -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector

default: test
$(bin): $o $h
	$(CC) -o $@ $o $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)
%.o: %.c $h $(Makefile)
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $< -o $@

test: $(bin)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(bbt)"

# install
DESTDIR ?= $(HOME)
PREFIX ?= .local
dest=$(DESTDIR)/$(PREFIX)/
bins=$(dest)bin/$(nom)
#libs=$(addprefix $(dest)lib/$(nom)/,$(notdir $(boot)))
#docs=$(dest)share/man/man1/$(nom).1
files=$(bins) $(libs) $(docs)
install: $(files)
uninstall:
	rm -f $(files)
$(dest)bin/%: %
	install -D -m 755 $^ $@
$(dest)share/%: %
	install -D -m 644 $^ $@
$(dest)lib/$(nom)/%: lib/%
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
bits: $(bin)
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
repl: $(nom)
	rlwrap $(bin) -i $(boot)

.PHONY: default clean test test-lots\
	install uninstall\
 	sloc bits valg perf bench flame disasm repl
