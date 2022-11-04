# for consistent sorting
LC_COLLATE=C

nom=lisa
suff=la

build_dir=build
test_dir=test
doc_dir=doc
lib_dir=lib

bin_release=$(build_dir)/$(nom)
bin_debug=$(build_dir)/$(nom).dbg

boot=$(lib_dir)/$(nom).$(suff)
lib=$(boot)

# installation
DESTDIR ?= $(HOME)
PREFIX ?= .local
dest_dir=$(DESTDIR)/$(PREFIX)

testcmd=$(bin_debug) -_ $(boot) $(test_dir)/*.$(suff)

test: $(bin_debug)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(testcmd)"

# run the tests a lot of times to try and catch nondeterministic bugs :(
test-lots:
	for n in {1..2048}; do make -s test || exit 1; done

# build
# tested with gcc & clang
CFLAGS ?=\
	-std=c11 -g -O2 -flto -fpic -Wall -Werror\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector -fno-align-functions

src_h=$(wildcard src/*.h)
src_c=$(wildcard src/*.c)
obj=$(addprefix $(build_dir)/,$(notdir $(src_c:.c=.o)))

$(build_dir)/%.o: src/%.c $(src_h) Makefile
	$(CC) -c -o $@ $(CFLAGS) $(CPPFLAGS) $<

$(bin_debug): $(obj)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

$(bin_release): $(bin_debug)
	strip --strip-unneeded -o $@ $^

install_files=\
	$(dest_dir)/bin/$(nom)\
	$(addprefix $(dest_dir)/lib/$(nom)/,$(notdir $(lib)))\
	$(dest_dir)/share/man/man1/$(nom).1

install: $(install_files)
uninstall:
	rm -f $(install_files)

$(dest_dir)/bin/%: $(build_dir)/%
	install -D $^ $@
$(dest_dir)/share/man/man1/%: $(doc_dir)/%
	install -D $^ $@
$(dest_dir)/lib/$(nom)/%: $(lib_dir)/%
	install -D $^ $@

# vim stuff
vim_dir=vim
VIMPREFIX ?= $(HOME)/.vim
vim_files=$(addprefix $(VIMPREFIX)/,syntax/$(nom).vim ftdetect/$(nom).vim)
install-vim: $(vim_files)
uninstall-vim:
	rm -f $(vim_files)
$(VIMPREFIX)/%: $(vim_dir)/%
	install -D $^ $@

# other tasks
#
clean:
	rm -f `git check-ignore * */*`

repl: $(bin_debug)
	which rlwrap && rlwrap $(testcmd) -i || $(testcmd) -i

# profiling on linux with perf
perf: perf.data
	perf report
perf.data: $(bin_debug) $(lib)
	perf record $(testcmd)

# valgrind detects some memory errors
valg: $(bin_debug)
	valgrind --error-exitcode=1 $(testcmd)
# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) *
# size of binaries
bits: $(bin_release) $(bin_debug)
	du -h $^

.PHONY: test test-lots repl clean\
	install uninstall\
 	install-vim uninstall-vim\
 	sloc bits valg perf
