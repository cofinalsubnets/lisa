include config.mk

T=bin
#
n=lips#                          | binary name
b=$n.bin
m=share/man/man1/$n.1

p=lib/$n/*.$n#                    | boot script
r=$T/$n -_ $p#                    | dev run command
t=$T/$b -_ $p $(sort $(wildcard test/*))# | test command

# compiler stuff ...
# build rules
#
# run tests
test: $T/$b
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $t"

$T/$n:
	make -C src ../$T/$n
$T/$b:
	make -C src ../$T/$b


# install config
#
D=$(DESTDIR)$(PREFIX)

# install rules
#
install: $D/bin/$n $D/$p $D/$m install-vim
uninstall: uninstall-vim
	rm -f $D/bin/$n $D/$p $D/$n
$D/%:
	mkdir -p $@
$D/$T/$n: $T/$n $D/bin
	cp $^
$D/$p: $p $D/lib/lips
	cp $^
$D/$m: $m $D/share/man/man1
	cp $^

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
valg: $b
	valgrind $t
sloc:
	cloc --force-lang=Lisp,$n *
bits: $T/$n $T/$b
	stat -c "%n %sB" $^
repl: $b
	rlwrap $r -i, $r -i

.PHONY:\
 	test clean perf valg sloc bits install\
 	install-vim uninstall uninstall-vim repl
