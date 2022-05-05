# standardize locale for sorting
LC_ALL=C

self=makefile
nom=yo
suff=yo
bins=bin/$(nom)
docs=share/man/man1/$(nom).1

boot=lib/$(nom)/$(nom).$(suff)
libs=$(boot)

tests=$(sort $(wildcard test/*.$(suff)))

headers=$(sort $(wildcard c/*.h))
sources=$(sort $(wildcard c/*.c))
objects=$(patsubst %.c,%.o,$(sources))

PREFIX ?= $(HOME)/.local
VIMPREFIX ?= $(HOME)/.vim

CPPFLAGS=-DPREF=\"$(PREFIX)\" -DLANG=\"$(nom)\" -DSUFF=\"$(suff)\"
CFLAGS=-std=c99 -g -O2 -flto -Wall -Werror\
	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector -fno-unroll-loops\
	-fno-inline -fno-align-functions

run_repl=bin/$(nom).bin -_i $(libs) test/*.$(suff)
run_tests=bin/$(nom).bin -_ $(libs) $(tests)

where=$(DESTDIR)$(PREFIX)/
files=$(addprefix $(where),$(bins) $(libs) $(docs))

default: test
%.o: %.c $(headers) $(self)
	$(CC) -c -o $@ $(CFLAGS) $(CPPFLAGS) $(@:.o=.c)

bin/$(nom): bin/$(nom).bin
	strip -o $@ $^
bin/$(nom).bin: $(objects) $(headers)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(objects)

$(where)%: %
	install -D $^ $@

install: $(files)
uninstall:
	rm -f $(files)

vimfiles=syntax/$(nom).vim ftdetect/$(nom).vim

$(VIMPREFIX)/%: vim/%
	install -D $^ $@

install-vim: $(addprefix $(VIMPREFIX)/,$(vimfiles))
uninstall-vim:
	rm -f $(addprefix $(VIMPREFIX)/,$(vimfiles))

clean:
	rm -rf `git check-ignore * */*`

test: bin/$(nom).bin
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(run_tests)"

perf: perf.data
	perf report
perf.data: bin/$(nom).bin $(libs)
	perf record $(run_tests)

valg: bin/$(nom).bin
	valgrind --error-exitcode=1 $(run_tests)

sloc:
	cloc --force-lang=Lisp,$(suff) *

bits: bin/$(nom) bin/$(nom).bin
	du -h $^

repl: bin/$(nom)
	which rlwrap && rlwrap $(run_repl) || $(run_repl)

.PHONY: default test clean perf valg sloc bits repl install uninstall install-vim uninstall-vim
