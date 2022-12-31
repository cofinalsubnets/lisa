nom=sen
suff=la
boot=lib/boot.$(suff)

CC ?= gcc
CFLAGS ?=\
	-std=c11 -g -O2 -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector
cc=$(CC) $(CFLAGS)

# installation
DESTDIR ?= $(HOME)
PREFIX ?= .local

run_tests=./$(nom) -_ $(boot) test/*.$(suff)

test: $(nom)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(run_tests)"

LC_COLLATE=C
c=$(sort $(wildcard *.c))
h=$(sort $(wildcard *.h))
o=$(c:.c=.o)

%.o: %.c $h makefile
	$(cc) -c $^

$(nom): $o $h
	$(cc) -o $@ $o

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

# vim stuff
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
	which rlwrap && rlwrap $(run_tests) -i || $(run_tests) -i

# profile on linux with perf
perf: perf.data
	perf report
perf.data: $(nom) $(boot)
	perf record $(run_tests)

# valgrind detects some memory errors
valg: $(nom)
	valgrind --error-exitcode=1 $(run_tests)

# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) *

# size of binaries
bits: $(nom)
	du -h $^

# run the tests a lot of times to try and catch nondeterministic bugs :(
test-lots: $(nom)
	for n in {1..2048}; do $(run_tests) || exit 1; done

.PHONY: test test-lots repl clean sloc bits valg perf\
	install uninstall install-vim uninstall-vim
