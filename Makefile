Makefile=Makefile
nom=gwen
sys=linux

#build
c=$(sort $(wildcard src/*.c)) src/sys/$(sys).c
h=$(sort $(wildcard src/*.h)) src/sys/$(sys).h
o=$(c:.c=.o)
CFLAGS ?=\
	-std=gnu11 -g -Os -Wall\
	-Dl_sys=$(sys)\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector

default: test
$(nom): $o $h
	$(CC) -o $@ $o $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)
%.o: %.c $h $(Makefile)
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $< -o $@

boot=lib/pre.gw
test=test/*.gw
runtests=./$(nom) $(boot) $(test)
test: $(nom)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(runtests)"

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
valg: $(nom)
	valgrind --error-exitcode=1 $(runtests)

# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) src/* test/* lib/*

# size of binaries
bits: $(nom)
	du -h $^

disasm: $(nom)
	rizin -A $^

# profiling on linux
perf.data: $(nom) $(boot)
	perf record $(runtests)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<
repl: $(nom)
	rlwrap ./$(nom) -i lib/pre.gw

.PHONY: default clean test test-lots\
	install uninstall\
 	sloc bits valg perf bench flame disasm repl
