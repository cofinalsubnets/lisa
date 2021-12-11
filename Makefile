include config.mk

n=lips
b=$n.bin

docs=share/man/man1/lips.1
libs=lib/lips/prelude.lips
files=bin/$n $(libs) $(docs)

# command to run the tests
t=bin/$b -_ $(libs) $(sort $(wildcard test/*))

test: bin/$b
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $t"

# install target
D=$(DESTDIR)$(PREFIX)
$D/%: %
	mkdir -p $(dir $@)
	cp -r $(firstword $^) $(dir $@)
# install rules
install: $(addprefix $D/,$(files))
uninstall:
	rm -f $(addprefix $D/,$(files))

# vim
V=$(VIMDIR)
vimfiles=syntax/$n.vim ftdetect/$n.vim
install-vim: $(addprefix $V/,$(vimfiles))
uninstall-vim:
	rm -f $(addprefix $V/,$(vimfiles))
$V/%/$n.vim: vim/%/$n.vim $V/%
	cp $^
$V/%:
	mkdir -p $@

# misc tasks
#
clean:
	rm -rf `git check-ignore * */*`
perf: perf.data
	perf report
perf.data: bin/$b $(libs)
	perf record ./$t
valg: bin/$b
	valgrind $t
sloc:
	cloc --force-lang=Lisp,$n *
bits: bin/$n bin/$b
	stat -c "%n %sB" $^
repl: bin/$n
	rlwrap $^ -_i $(libs) test/00-helpers.lips test/kanren.lips test/nf.lips


CC ?= gcc
CPPFLAGS ?= -DPREFIX=\"$(PREFIX)\"
# fixnums need sign extended bitshifts.
# other things tend to break TCO ...
CFLAGS ?= -std=c99 -g -O2 -flto\
	-Wall -Wstrict-prototypes\
	-Wno-shift-negative-value\
	-fno-stack-protector\
	-fno-unroll-loops

# build config
#
n=lips
b=$n.bin
# target directory
# dist binary
bin/$n: bin/$n.bin
	strip -o $@ $^
# dev binary
bin/$n.bin: $(patsubst %.c,%.o, $(sort $(wildcard src/*.c)))
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^

.PHONY:\
 	test clean perf valg sloc bits install\
 	enstall-vim uninstall uninstall-vim repl
