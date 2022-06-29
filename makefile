LC_COLLATE=C
lang=lisa
suff=la
bin=bin/$(lang)
doc=share/man/man1/$(lang).1
lib=lib/$(lang)/$(lang).$(suff)
run=bin/$(lang).bin -_ $(lib) test/*.$(suff)
h=$(sort $(wildcard c/*.h))
c=$(sort $(wildcard c/*.c))
DESTDIR ?= $(HOME)
PREFIX ?= .local
CPPFLAGS=\
	-DPREF=\"$(DESTDIR)/$(PREFIX)\" -DLANG=\"$(lang)\" -DSUFF=\"$(suff)\"
CFLAGS=\
	-std=c17 -g -Os -flto\
	-Wall -Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector -fno-unroll-loops
VIMPREFIX ?= $(HOME)/.vim
vim_files=$(addprefix $(VIMPREFIX)/,syntax/$(lang).vim ftdetect/$(lang).vim)
whither=$(DESTDIR)/$(PREFIX)/
installed_files=$(addprefix $(whither),$(bin) $(lib) $(doc))

test: bin/$(lang).bin
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(run)"

repl: bin/$(lang)
	which rlwrap && rlwrap $(run) -i || $(run) -i

clean:
	rm -f `git check-ignore * */*`

bin/$(lang): bin/$(lang).bin
	strip -o $@ $^

bin/$(lang).bin: $(c:.c=.o)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

# installation
install: $(installed_files)

uninstall:
	rm -f $(installed_files)

# profile on linux with perf
perf: perf.data
	perf report

perf.data: bin/$(lang).bin $(lib)
	perf record $(run)

# valgrind detects some memory errors
valg: bin/$(lang).bin
	valgrind --error-exitcode=1 $(run)

# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) *
# size of binaries
bits: bin/$(lang) bin/$(lang).bin
	du -h $^

# vim
install-vim: $(vim_files)
uninstall-vim:
	rm -f $(vim_files)

$(whither)%: %
	install -D $^ $@
%.o: %.c $h makefile
	$(CC) -c -o $@ $(CFLAGS) $(CPPFLAGS) $(@:.o=.c)
$(VIMPREFIX)/%: vim/%
	install -D $^ $@

.PHONY: test
