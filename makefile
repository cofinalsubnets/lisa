m=makefile
n=lips
b=$n.bin
p=prelude.lips
v="`git rev-parse HEAD`-git"
r=./$b -_ $p
tests=$r test/*

CC=gcc
CFLAGS=-std=gnu17 -g -O2 -flto\
	-Wall -Wstrict-prototypes\
	-Wno-shift-negative-value\
	-fno-stack-protector\
	-fno-unroll-loops
c=$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS)

test: $n
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(tests)"
$b: $(patsubst %.c,%.o,$(wildcard *.c))
	$c -o $@ $^
$n: $b
	strip -o $@ $^

# install
DESTDIR ?= ~
PREFIX ?= /.local
bindir=$(DESTDIR)$(PREFIX)/bin
libdir=$(DESTDIR)$(PREFIX)/lib/lips
bins=$(bindir)/$n
libs=$(libdir)/$p
$(bindir):
	mkdir -p $(bindir)
$(libdir):
	mkdir -p $(libdir)
$(bins): $n $(bindir)
	cp $^
$(libs): $p $(libdir)
	cp $^
install: $(bins) $(libs)
uninstall:
	rm -f $(bins)
	rm -rf $(libdir)

# vim stuff
VIMDIR ?= ~/.vim
$(VIMDIR)/syntax:
	mkdir -p $@
$(VIMDIR)/ftdetect:
	mkdir -p $@
$(VIMDIR)/ftdetect/$n.vim: vim/ftdetect/$n.vim $(VIMDIR)/ftdetect
	cp $^
$(VIMDIR)/syntax/$n.vim: vim/syntax/$n.vim $(VIMDIR)/syntax
	cp $^
install-vim: $(VIMDIR)/syntax/$n.vim $(VIMDIR)/ftdetect/$n.vim
uninstall-vim:
	rm -f $(VIMDIR)/{syntax,ftdetect}/$n.vim

# tasks
clean:
	@which git && rm -rf `git check-ignore *` || rm -f *.o $n $b perf.data
perf: perf.data
	@which perf && perf report || echo "perf not installed"
perf.data: $b $p
	@which perf && perf record ./$(tests) || echo "perf not installed"
valg: $b
	@which && valgrind $(tests) || echo "valgrind not installed"
sloc:
	@which cloc && cloc --by-file --force-lang=Lisp,$n *.{c,h,$n} || echo "cloc not installed"
bins: $n $b
	stat -c "%n %sB" $^
repl: $b
	@which rlwrap >/dev/null && rlwrap $r -i || $r -i

.PHONY: test clean perf valg sloc bins install install-vim uninstall uninstall-vim repl
