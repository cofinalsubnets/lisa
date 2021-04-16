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

# C compiler
# gcc and clang both work. other compilers, tcc etc., don't.
# gcc generates faster and smaller code on x86 and ARM.
#
# why these compiler flags :
# - fixnums need sign-extended bit shifting.
# - inlining bloats code and GCC even does it for tail calls,
#   which is silly. turn it off by default.
# - stack smash protection also hurts tco.
c=gcc -g -O2 -flto -std=gnu17\
	-Wall -Wno-shift-negative-value\
	-Wstrict-prototypes\
	-fno-inline -fno-stack-protector\
	-fno-unroll-loops -fpie

test: $n
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(tcmd)"

# build
$n: $b
	@strip -o $@ $b
	@stat -c "$@ %sB" $@
$b: $m $h $o
	@$c -o $@ $o
	@stat -c "$@ %sB" $@
.c.o:
	@$c -c $<
	@stat -c "$@ %sB" $@

# install
pref=~/.local
bins=$(pref)/bin
libs=$(pref)/lib/lips
b0=$(bins)/$n
l0=$(libs)/prelude.lips

$(bins):
	@mkdir -p $(bins)
$(libs):
	@mkdir -p $(libs)
$(b0): $n $(bins)
	@cp $^
	@echo $@
$(l0): $p $(libs)
	@cp $^
	@echo $@

install: $(b0) $(l0)

# vim stuff
install-vim:
	@make -sC vim

# tasks
clean:
	@rm -rf `git check-ignore *`
perf: perf.data
	@perf report
perf.data: $b $t $p
	@perf record ./$(tcmd)
valg: $b $t
	@valgrind $(tcmd)
sloc:
	@which cloc >/dev/null && cloc --force-lang=Lisp,l --force-lang=Lisp,$n . || cat $s $h $m | grep -v ' *//.*' | grep -v '^$$' | wc -l
bins: $o $n $b
	@stat -c "%n %sB" $^
bench: $b
	@ruby ./bench.rb "$(run)"
repl: $n
	@which rlwrap >/dev/null && rlwrap $(rcmd) || $(rcmd)

.PHONY: test clean perf valg sloc bins install vim bench repl
