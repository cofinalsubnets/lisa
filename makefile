lang=lisa
suff=la
lib=lib/$(lang)/$(lang).$(suff)

testcmd=bin/$(lang).bin -_ $(lib) test/*.$(suff)
test: bin/$(lang).bin
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(testcmd)"
repl: bin/$(lang)
	which rlwrap && rlwrap $(testcmd) -i || $(testcmd) -i

# build
# tested with gcc, clang, and compcert
CPPFLAGS=\
	-DPREF=\"$(DESTDIR)/$(PREFIX)\" -DLANG=\"$(lang)\" -DSUFF=\"$(suff)\"
CFLAGS=\
	-std=c99 -g -Os -flto\
	-Wall -Werror -Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector -fno-unroll-loops -fno-align-functions
# set locale for sorting
LC_COLLATE=C
h=$(sort $(wildcard c/*.h))
c=$(sort $(wildcard c/*.c))
%.o: %.c $h makefile
	$(CC) -c -o $@ $(CFLAGS) $(CPPFLAGS) $(@:.o=.c)
bin/$(lang).bin: $(c:.c=.o)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^
bin/$(lang): bin/$(lang).bin
	strip -o $@ $^
clean:
	rm -f `git check-ignore * */*`

# installation
DESTDIR ?= $(HOME)
PREFIX ?= .local
bin=bin/$(lang)
doc=share/man/man1/$(lang).1
whither=$(DESTDIR)/$(PREFIX)/
installed_files=$(addprefix $(whither),$(bin) $(lib) $(doc))
install: $(installed_files)
uninstall:
	rm -f $(installed_files)
$(whither)%: %
	install -D $^ $@

# vim stuff
VIMPREFIX ?= $(HOME)/.vim
vim_files=$(addprefix $(VIMPREFIX)/,syntax/$(lang).vim ftdetect/$(lang).vim)
install-vim: $(vim_files)
uninstall-vim:
	rm -f $(vim_files)
$(VIMPREFIX)/%: vim/%
	install -D $^ $@

# other useful tasks
#
# profile on linux with perf
perf: perf.data
	perf report
perf.data: bin/$(lang).bin $(lib)
	perf record $(testcmd)
# valgrind detects some memory errors
valg: bin/$(lang).bin
	valgrind --error-exitcode=1 $(testcmd)
# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) *
# size of binaries
bits: bin/$(lang) bin/$(lang).bin
	du -h $^

.PHONY: test repl install install-vim uninstall uninstall-vim sloc bits valg perf clean
