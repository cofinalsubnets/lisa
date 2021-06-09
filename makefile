m=makefile
n=lips
b=$n.bin
p=prelude.lips
t=$(wildcard test/*)
v="`git rev-parse HEAD`-git"
run=./$b -_ $p
tcmd=$(run) test/*

CC=gcc
CFLAGS=-std=gnu17 -g -Os -flto\
 	-Wall -Werror -Wstrict-prototypes\
 	-Wno-shift-negative-value\
	-fno-stack-protector\
	-fno-unroll-loops

test: $n
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(tcmd)"

# build
$b: $(patsubst %.c,%.o,$(wildcard *.c))
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $^
$n: $b
	strip -o $@ $^

# install
prefix ?= ~/.local/
bins=$(prefix)bin/
libs=$(prefix)lib/lips/
b0=$(bins)$n
l0=$(libs)prelude.lips

$(bins):
	mkdir -p $(bins)
$(libs):
	mkdir -p $(libs)
$(b0): $n $(bins)
	cp $^
$(l0): $p $(libs)
	cp $^

install: $(b0) $(l0)

# vim stuff
install-vim:
	make -sC vim

# tasks
clean:
	rm -rf `git check-ignore *`
perf: perf.data
	perf report
perf.data: $b $t $p
	perf record ./$(tcmd)
valg: $b $t
	valgrind $(tcmd)
sloc:
	which cloc >/dev/null && cloc --by-file --force-lang=Lisp,$n *.{c,h,$n} || cat *.c | grep -v ' *//.*' | grep -v '^$$' | wc -l
bins: $n $b
	stat -c "%n %sB" $^
repl: $n
	which rlwrap >/dev/null && rlwrap $(run) -i || $(run) -i

.PHONY: test clean perf valg sloc bins install vim repl
