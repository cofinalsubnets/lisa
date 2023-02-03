this=Makefile
nom=li
suff=la
boot=lib/boot.$(suff)

# default task
test=./$(nom) -_ $(boot) test/*.$(suff)
test: $(nom)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(test)"

#build
LC_COLLATE=C
c=$(sort $(wildcard src/*.c))
h=$(sort $(wildcard src/*.h))
o=$(c:.c=.o)
CFLAGS ?=\
	-std=c99 -g -O2 -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector
src/%.o: src/%.c $h $(this)
	$(CC) $(CFLAGS) -c $< -o $@
$(nom): $o $h
	$(CC) $(CFLAGS) -o $@ $o

# install
DESTDIR ?= $(HOME)
PREFIX ?= .local
dest=$(DESTDIR)/$(PREFIX)/
bins=$(dest)bin/$(nom)
libs=$(addprefix $(dest)lib/$(nom)/,$(notdir $(boot)))
docs=$(dest)share/man/man1/$(nom).1
files=$(bins) $(libs) $(docs)
install: $(files)
uninstall:
	rm -f $(files)
$(dest)bin/%: %
	install -D $^ $@
$(dest)share/%: %
	install -D $^ $@
$(dest)lib/$(nom)/%: lib/%
	install -D $^ $@

# for vim
vim_files=$(addprefix $(VIMPREFIX)/,syntax/$(nom).vim ftdetect/$(nom).vim)
install-vim: $(vim_files)
uninstall-vim:
	rm -f $(vim_files)
VIMPREFIX ?= $(HOME)/.vim
$(VIMPREFIX)/%: vim/%
	install -D $^ $@

# other tasks
#
clean:
	rm -f `git check-ignore * */*`
repl: $(nom)
	which rlwrap && rlwrap $(test) -i || $(test) -i
# profile on linux with perf
perf: perf.data
	perf report
perf.data: $(nom) $(boot)
	perf record $(test)
# valgrind detects some memory errors
valg: $(nom)
	valgrind --error-exitcode=1 $(test)
# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) src/* test/* lib/*
# size of binaries
bits: $(nom)
	du -h $^
# run the tests a lot of times to try and catch nondeterministic bugs :(
test-lots: $(nom)
	for n in {1..2048}; do $(test) || exit 1; done
# flame graph
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<

.PHONY: test test-lots repl clean sloc bits valg perf\
	install uninstall install-vim uninstall-vim flame
