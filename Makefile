Makefile=Makefile
nom=lisa
#build
LC_COLLATE=C
c=$(sort $(wildcard src/*.c))
h=$(sort $(wildcard src/*.h))
CPPFLAGS += -Dtesting
o=$(c:.c=.o)
CFLAGS ?=\
	-std=gnu11 -g -Os -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector

testcmd=./$(nom) --self-test
test: $(nom)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(testcmd)"
# run the tests a lot of times to try and catch GC bugs :(
test-lots: $(nom)
	for n in {1..2048}; do $(testcmd) || exit 1; done

src/%.o: src/%.c $h $(Makefile)
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $< -o $@
$(nom): $o $h
	$(CC) -o $@ $o $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)

# install
DESTDIR ?= $(HOME)
PREFIX ?= .local
dest=$(DESTDIR)/$(PREFIX)/
bins=$(dest)bin/$(nom)
#libs=$(addprefix $(dest)lib/$(nom)/,$(notdir $(boot)))
#docs=$(dest)share/man/man1/$(nom).1
files=$(bins) $(libs) $(docs)
install: $(files)
uninstall:
	rm -f $(files)
$(dest)bin/%: %
	install -D -m 755 $^ $@
$(dest)share/%: %
	install -D -m 644 $^ $@
$(dest)lib/$(nom)/%: lib/%
	install -D -m 644 $^ $@

# for vim
VIMPREFIX ?= $(HOME)/.vim
vim_files=$(addprefix $(VIMPREFIX)/,syntax/$(nom).vim ftdetect/$(nom).vim)
install-vim: $(vim_files)
uninstall-vim:
	rm -f $(vim_files)
$(VIMPREFIX)/%: vim/%
	install -D -m 644 $^ $@

# other tasks
#
clean:
	rm -r `git check-ignore * */*`
repl: $(nom)
	which rlwrap && rlwrap $(testcmd) -i || $(testcmd) -i

# valgrind detects some memory errors
valg: $(nom)
	valgrind --error-exitcode=1 $(testcmd)

# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) src/* test/* lib/*

# size of binaries
bits: $(nom)
	du -h $^

disasm: $(nom)
	rizin -A $^

# profiling on linux
perf.data: $(nom) $(boot)
	perf record $(testcmd)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<

.PHONY: test test-lots repl clean sloc bits valg perf bench\
	install uninstall install-vim uninstall-vim flame disasm
