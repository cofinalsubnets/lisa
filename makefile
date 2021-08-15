n=lips
b=$n.bin
p=prelude.lips
r=./$b -_ $p
t=$r test/*

# C compiler & flags
CC=gcc # gcc seems to do better than clang
# fixnums need sign extended bitshifts.
# other things tend to break TCO ...
CFLAGS=-std=gnu17 -g -O2 -flto\
	-Wall -Wstrict-prototypes\
	-Wno-shift-negative-value\
	-fno-stack-protector\
	-fno-unroll-loops

# determine install path
ifeq ($(shell whoami), root)
	DESTDIR?=/
	PREFIX?=/usr/local
else
	DESTDIR?=${HOME}
	PREFIX?=/.local
endif

test: $n
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $t"
$b: $(patsubst %.c,%.o, $(wildcard *.c))
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^
$n: $b
	strip -o $@ $^

# install
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

define WX
	which $1 1>/dev/null 2>&1 && $2 || $3
endef
define W
  $(call WX, $1, $2, echo "$1 not installed")
endef

# tasks
clean:
	@$(call WX, git, rm -rf `git check-ignore *`, rm -f *.o $n $b perf.data)
perf: perf.data
	@$(call W, perf, perf report)
perf.data: $b $p
	@$(call W, perf, perf record ./$t)
valg: $b
	@$(call W, valgrind, valgrind $t)
sloc:
	@$(call WX, cloc, cloc --by-file --force-lang=Lisp,$n *.{c,h,$n}, wc -l *.c *.h *.lips)
bins: $n $b
	stat -c "%n %sB" $^
repl: $b
	@$(call WX, rlwrap, rlwrap $r -i, $r -i)

.PHONY: test clean perf valg sloc bins install install-vim uninstall uninstall-vim repl
