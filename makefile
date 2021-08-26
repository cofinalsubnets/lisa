n=lips
m=$n.1
b=$n.bin
p=prelude.lips
r=./$b -_ $p
t=$r test/*
_=,

# C compiler & flags
CC=gcc # gcc seems to do better than clang
# fixnums need sign extended bitshifts.
# other things tend to break TCO ...
CFLAGS=-std=gnu17 -g -O2 -flto\
	-Wall -Wstrict-prototypes\
	-Wno-shift-negative-value\
	-fno-stack-protector\
	-fno-unroll-loops

# for sorting
LC_ALL=C

# determine install path
ifeq ($(shell whoami), root)
	DESTDIR ?= /
	PREFIX ?= /usr/local
else
	DESTDIR ?= ${HOME}
	PREFIX ?= /.local
endif

MANPREFIX ?= $(PREFIX)/share/man

test: $n
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $t"
$b: $(patsubst %.c,%.o, $(sort $(wildcard *.c)))
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^
$n: $b
	strip -o $@ $^

# install
bin=$(DESTDIR)$(PREFIX)/bin
lib=$(DESTDIR)$(PREFIX)/lib/lips
man=$(DESTDIR)$(MANPREFIX)/man1
bins=$(bin)/$n
libs=$(lib)/$p
mans=$(man)/$m
$(bin):
	mkdir -p $@
$(lib):
	mkdir -p $@
$(man):
	mkdir -p $@
$(bins): $n $(bin)
	cp $^
$(libs): $p $(lib)
	cp $^
$(mans): $m $(man)
	cp $^
install: $(bins) $(libs) $(mans)
uninstall:
	rm -f $(bins) $(libs) $(mans)

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

define Wx
	which $1 1>/dev/null 2>&1 && $2 || $3
endef
define W
  $(call Wx, $1, $2, echo "$1 not installed")
endef

# tasks
clean:
	@$(call Wx, git, rm -rf `git check-ignore *`, rm -f *.o $n $b perf.data)
perf: perf.data
	@$(call W, perf, perf report)
perf.data: $b $p
	@$(call W, perf, perf record ./$t)
valg: $b
	@$(call W, valgrind, valgrind $t)
sloc:
	@$(call Wx, cloc, cloc --by-file --force-lang=Lisp$_$n *.{c$_h$_$n}, wc -l *.c *.h *.lips)
bins: $n $b
	stat -c "%n %sB" $^
repl: $b
	@$(call Wx, rlwrap, rlwrap $r -i, $r -i)

.PHONY: test clean perf valg sloc bins install install-vim uninstall uninstall-vim repl
