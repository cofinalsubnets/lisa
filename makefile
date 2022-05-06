include config.mk

self=makefile
bin=bin/$(lang)
doc=share/man/man1/$(lang).1
lib=lib/$(lang)/$(lang).$(suff)
h=$(sort $(wildcard c/*.h))
c=$(sort $(wildcard c/*.c))
PREFIX ?= $(HOME)/.local
#CC=clang
CPPFLAGS=-DPREF=\"$(PREFIX)\" -DLANG=\"$(lang)\" -DSUFF=\"$(suff)\"
CFLAGS=-std=c99 -g -O2 -flto -Wall -Werror\
	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector -fno-unroll-loops\
	-fno-inline -fno-align-functions

# run the tests
test=bin/$(lang).bin -_ $(lib) test/*.$(suff)
test: bin/$(lang).bin
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(test)"
# interact
repl: bin/$(lang)
	which rlwrap && rlwrap $(test) -i || $(test) -i

where=$(DESTDIR)$(PREFIX)/
files=$(addprefix $(where),$(bin) $(lib) $(doc))
install: $(files)
uninstall:
	rm -f $(files)
$(where)%: %
	install -D $^ $@

bin/$(lang): bin/$(lang).bin
	strip -o $@ $^
bin/$(lang).bin: $(c:.c=.o)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^
%.o: %.c $h $(self)
	$(CC) -c -o $@ $(CFLAGS) $(CPPFLAGS) $(@:.o=.c)

clean:
	rm `git check-ignore * */*`

# profile on linux with perf
perf: perf.data
	perf report
perf.data: bin/$(lang).bin $(lib)
	perf record $(test)

# valgrind can detect some memory errors
valg: bin/$(lang).bin
	valgrind --error-exitcode=1 $(test)

# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) *
# size of binaries
bits: bin/$(lang) bin/$(lang).bin
	du -h $^

# vim syntax files
install-vim:
	make -C vim $@
uninstall-vim:
	make -C vim $@

.PHONY: test clean perf valg sloc bits repl\
 	install uninstall install-vim uninstall-vim
