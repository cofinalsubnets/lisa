include config.mk

n=lips
b=$n.bin

bin=bin
docs=share/man/man1/$n.1
libs=$(sort $(wildcard lib/$n/*.$n))

# command to run the tests
t=$(bin)/$b -_ $(libs) $(sort $(wildcard test/*))

test: $(bin)/$b
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $t"
$(bin)/%:
	make -C src ../$@

# install target
D=$(DESTDIR)$(PREFIX)
$D/%: %
	mkdir -p $(dir $@)
	cp -r $(dir $^) $D/$(firstword $(subst /, ,$^))
# install rules
install: $D/$(bin)/$n $D/$(libs) $D/$(docs)
uninstall:
	rm -f $D/$(bin)/$n $D/$(libs) $D/$(docs)

# vim
#
V=$(VIMDIR)
install-vim: $V/syntax/$n.vim $V/ftdetect/$n.vim
uninstall-vim:
	rm -f $V/{syntax,ftdetect}/$n.vim
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
perf.data: $(bin)/$b $(libs)
	perf record ./$t
valg: $(bin)/$b
	valgrind $t
sloc:
	cloc --force-lang=Lisp,$n *
bits: $(bin)/$n $(bin)/$b
	stat -c "%n %sB" $^
repl: $(bin)/$n 
	rlwrap $^ -_i $(libs) test/00-helpers.lips test/kanren.lips

.PHONY:\
 	test clean perf valg sloc bits install\
 	install-vim uninstall uninstall-vim repl
