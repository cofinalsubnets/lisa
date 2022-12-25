nom=lisa
suff=la

LC_COLLATE=C
h=$(sort $(wildcard src/*.h))
c=$(sort $(wildcard src/*.c))
o=$(c:.c=.o)

boot=lib/boot.la

CFLAGS ?=\
	-std=c99 -g -O2 -flto -fpic -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector

build_dir=build
test_dir=test
doc_dir=doc
lib_dir=lib

release_build=$(nom)
debug_build=$(release_build).dbg

boot=$(lib_dir)/boot.$(suff)
lib=$(boot)


# installation
DESTDIR ?= $(HOME)
PREFIX ?= .local

run_tests=./$(debug_build) -_ $(boot) $(test_dir)/*.$(suff)

test: $(debug_build)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(run_tests)"

# run the tests a lot of times to try and catch nondeterministic bugs :(
test-lots: $(debug_build)
	for n in {1..2048}; do $(run_tests) || exit 1; done

$(release_build): $(debug_build)
	strip --strip-unneeded -o $@ $^

$(debug_build): $o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

src/%.o: src/%.c $h Makefile
	$(CC) -c -o $@ $(CFLAGS) $(CPPFLAGS) $<

dest=$(DESTDIR)/$(PREFIX)/
bin_files=$(dest)bin/$(nom)
lib_files=$(addprefix $(dest)lib/$(nom)/,$(notdir $(lib)))
doc_files=$(dest)share/man/man1/$(nom).1
all_files=$(bin_files) $(lib_files) $(doc_files)
install: $(all_files)
uninstall:
	rm -f $(all_files)

$(dest)bin/%: %
	install -D $^ $@
$(dest)share/man/man1/%: $(doc_dir)/%
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
bits: $(release_build) $(debug_build)
	du -h $^

.PHONY: test test-lots repl clean\
	install uninstall\
 	install-vim uninstall-vim\
 	sloc bits valg perf
