m=makefile
n=lips
b=$n.bin
p=prelude.lips
t=$(wildcard test/*)
h=$(wildcard *.h)
s=$(wildcard *.c)
o=$(s:.c=.o)
v="`git rev-parse HEAD`-git"
run=./$b -_ $p
tcmd=$(run) $t
rcmd=$(run) -i

c=gcc -std=gnu17 -g -Os -flto\
	-Wall -Wno-shift-negative-value\
	-Wstrict-prototypes\
	-fno-stack-protector\
	-fno-unroll-loops

test: $n
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(tcmd)"

# build
$n: $b
	strip -o $n $b
$b: $m $o
	$c -o $b $o
.c.o:
	$c -c $^

# install
pref=~/.local/
bins=$(pref)bin/
libs=$(pref)lib/lips/
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
	which cloc >/dev/null && cloc --by-file --force-lang=Lisp,$n *.{c,h,$n} || cat $s | grep -v ' *//.*' | grep -v '^$$' | wc -l
bins: $n $b
	stat -c "%n %sB" $^
repl: $n
	which rlwrap >/dev/null && rlwrap $(rcmd) || $(rcmd)

.PHONY: test clean perf valg sloc bins install vim repl
