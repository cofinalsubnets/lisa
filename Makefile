nom=lisa
lang=$(nom)
suff=la
srcdir=src
builddir=build
testdir=test

bin_rel=$(builddir)/$(nom)
bin_debug=$(builddir)/$(nom).bin

lib=lib/$(nom).$(suff)

testcmd=$(bin_debug) -_ $(lib) $(testdir)/*.$(suff)

test: $(bin_debug)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(testcmd)"

# run the tests a lot of times to try and catch nondeterministic bugs
lots=2048
test_lots:
	for n in {1..$(lots)}; do make -s test || exit 1; done

# build
# tested with gcc & clang
CPPFLAGS ?=\
	-DPREF=\"$(DESTDIR)/$(PREFIX)\" -DLANG=\"$(lang)\" -DSUFF=\"$(suff)\"
CFLAGS ?=\
	-std=c11 -g -O2 -flto -Wall -Werror\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector
# set locale for sorting
LC_COLLATE=C
src_h=$(sort $(wildcard $(srcdir)/*.h))
src_c=$(sort $(wildcard $(srcdir)/*.c))
$(builddir)/%.o: $(srcdir)/%.c $(src_h) Makefile
	$(CC) -c -o $@ $(CFLAGS) $(CPPFLAGS) $<
$(bin_debug): $(addprefix $(builddir)/,$(notdir $(src_c:.c=.o)))
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^
$(bin_rel): $(bin_debug)
	strip -o $@ $^

# installation
DESTDIR ?= $(HOME)
PREFIX ?= .local

manpage=doc/$(lang).1
dest=$(DESTDIR)/$(PREFIX)
install: install_bin install_doc install_lib
uninstall:
	rm -f $(dest)/bin/$(notdir $(bin_rel))
	rm -f $(dest)/lib/$(nom)/$(notdir $(lib))
	rm -f $(dest)/share/man/man1/$(notdir $(manpage))

install_bin: $(bin_rel)
	install -D $^ $(dest)/bin/$(notdir $(bin_rel))
install_lib: $(lib)
	install -D $^ $(dest)/lib/$(nom)/$(notdir $(lib))
install_doc: $(manpage)
	install -D $(manpage) $(dest)/share/man/man1/$(notdir $(manpage))


# vim stuff
VIMPREFIX ?= $(HOME)/.vim
vim_srcdir=vim
vim_files=$(addprefix $(VIMPREFIX)/,syntax/$(nom).vim ftdetect/$(nom).vim)
install-vim: $(vim_files)
uninstall-vim:
	rm -f $(vim_files)
$(VIMPREFIX)/%: $(vim_srcdir)/%
	install -D $^ $@

# other tasks
#
clean:
	rm -f `git check-ignore * */*`
repl: $(bin_debug)
	which rlwrap && rlwrap $(testcmd) -i || $(testcmd) -i
# profile on linux with perf
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
bits: $(bin_rel) $(bin_debug)
	du -h $^

.PHONY: test repl install install-vim uninstall uninstall-vim sloc bits valg perf clean test_lots
