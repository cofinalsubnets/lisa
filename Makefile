Makefile=Makefile
nom=lisa

default: test

$(nom): src/$(nom)
	mv $^ $@
src/$(nom):
	make -C src $(nom)



testcmd=./$(nom) </dev/null
test: $(nom)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(testcmd)"
# run the tests a lot of times to try and catch GC bugs :(
test-lots: $(nom)
	for n in {1..2048}; do $(testcmd) || exit 1; done

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
	valgrind --error-exitcode=1 $(testcmd)

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
	perf record $(testcmd)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<

.PHONY: default clean test test-lots\
	install uninstall\
 	sloc bits valg perf bench flame disasm
