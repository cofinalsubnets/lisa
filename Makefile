LC_ALL=C

nom=lips
bins=bin/$(nom)
docs=share/man/man1/$(nom).1

boot=lib/$(nom)/prelude.$(nom)
libs=$(boot)
tests=$(sort $(wildcard test/*.$(nom)))

headers=$(sort $(wildcard c/*.h))
sources=$(sort $(wildcard c/*.c))
objects=$(patsubst %.c,%.o,$(sources))

ifeq ($(shell id -u), 0)
PREFIX ?= /usr/local
V ?= /usr/share/vim/vimfiles
else
PREFIX ?= $(HOME)/.local
V ?= $(HOME)/.vim
endif

CPPFLAGS=-DPREFIX=\"$(PREFIX)\"
CFLAGS=-std=gnu17 -g -O2 -flto -Wall -Werror\
	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector -fno-unroll-loops\
	-fno-align-functions

run_repl=bin/$(nom).bin -_i $(libs) test/*.$(nom)
run_tests=bin/$(nom).bin -_ $(libs) $(tests)

where=$(DESTDIR)$(PREFIX)/
files=$(addprefix $(where),$(bins) $(libs) $(docs))

test: bin/$(nom).bin
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(run_tests)"

bin/$(nom): bin/$(nom).bin
	strip -o $@ $^
bin/$(nom).bin: $(objects) $(headers)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $(objects)

$(where)%: %
	install -D $^ $@

install: $(files)
uninstall:
	rm -f $(files)

vimfiles=syntax/lips.vim ftdetect/lips.vim

$V/%: vim/%
	install -D $^ $@

install-vim: $(addprefix $V/,$(vimfiles))
uninstall-vim:
	rm -f $(addprefix $V/,$(vimfiles))

clean:
	rm -rf `git check-ignore * */*`

perf: perf.data
	perf report
perf.data: bin/$(nom).bin $(libs)
	perf record $(run_tests)

valg: bin/$(nom).bin
	valgrind --error-exitcode=1 $(run_tests)

sloc:
	cloc --force-lang=Lisp,$(nom) *

bits: bin/$(nom) bin/$(nom).bin
	du -h $^

repl: bin/$(nom)
	which rlwrap && rlwrap $(run_repl) || $(run_repl)

.PHONY:\
 	test clean perf valg sloc bits repl\
	install uninstall install-vim uninstall-vim
