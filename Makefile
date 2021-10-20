include config.mk

T=bin
n=lips
b=$n.bin

docs=share/man/man1/$n.1
p=lib/$n/*.$n

# command to run the tests
t=$T/$b -_ $p $(sort $(wildcard test/*))

test: $T/$b
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $t"
$T/%: $T
	make -C src ../$@

# install target
D=$(DESTDIR)$(PREFIX)
$D/%: %
	mkdir -p $(dir $@)
	cp -r $(dir $^) $D
# install rules
install: $D/$T/$n $D/$p $D/$(docs)
uninstall:
	rm -f $D/$T/$n $D/$p $D/$(docs)

# vim
#
V=$(VIMDIR)
install-vim: $V/syntax/$n.vim $V/ftdetect/$n.vim
uninstall-vim:
	rm -f $V/{syntax,ftdetect}/$n.vim
$V/%:
	mkdir -p $@
$V/%/$n.vim: vim/%/$n.vim $V/%
	cp $^

# misc tasks
#
clean:
	rm -rf `git check-ignore * */*`
perf: perf.data
	perf report
perf.data: $T/$b $p
	perf record ./$t
valg: $T/$b
	valgrind $t
sloc:
	cloc --force-lang=Lisp,$n *
bits: $T/$n $T/$b
	stat -c "%n %sB" $^
repl: $T/$n
	rlwrap $T/$n -_i $p

.PHONY:\
 	test clean perf valg sloc bits install\
 	install-vim uninstall uninstall-vim repl
