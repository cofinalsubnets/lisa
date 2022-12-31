nom=sen
suff=la
boot=lib/boot.$(suff)
lib=$(boot)

CC ?= gcc
CFLAGS ?=\
	-std=c99 -g -O2 -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector
cc=$(CC) $(CFLAGS)

lib_dir=lib

# installation
DESTDIR ?= $(HOME)
PREFIX ?= .local

run_tests=./$(nom) -_ $(boot) test/*.$(suff)

test: $(nom)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(run_tests)"

$(nom): $(nom).c
	$(cc) -o $@ $^

# run the tests a lot of times to try and catch nondeterministic bugs :(
test-lots: $(debug_build)
	for n in {1..2048}; do $(run_tests) || exit 1; done

bins=$(dest)bin/$(nom)
libs=$(addprefix $(dest)lib/$(nom)/,$(notdir $(boot)))
docs=$(dest)share/man/man1/$(nom).1
files=$(bins) $(libs) $(docs)
install: $(files)
uninstall:
	rm -f $(files)

$(dest)bin/%: %
	install -D $^ $@
$(dest)share/man/man1/%: %
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

repl: $(debug_build)
	which rlwrap && rlwrap $(run_tests) -i || $(run_tests) -i

# profiling on linux with perf
perf: perf.data
	perf report
perf.data: $(debug_build) $(lib)
	perf record $(run_tests)

# valgrind detects some memory errors
valg: $(debug_build)
	valgrind --error-exitcode=1 $(run_tests)

# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) *

# size of binaries
bits: $(nom)
	du -h $^

.PHONY: test test-lots repl clean\
	install uninstall\
 	install-vim uninstall-vim\
 	sloc bits valg perf
