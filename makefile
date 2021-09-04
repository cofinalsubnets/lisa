include config.mk
# build config
#
n=lips#                          | binary name
m=$n.1#                          | manpage
b=$n.bin#                        | dev binary
p=prelude.$n#                    | boot script
r=./$b -_ $p#                    | dev run command
t=$r $(sort $(wildcard test/*))# | test command
#
# compiler stuff ...
# build rules
#
# run tests
test: $n
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $t"
#
# dev binary
$b: $(patsubst %.c,%.o, $(sort $(wildcard *.c)))
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^
#
# dist binary
$n: $b
	strip -o $@ $^

# install config
#
D=$(DESTDIR)$(PREFIX)
B=$D/bin
L=$D/lib/lips
M=$D/share/man/man1
V=~/.vim

# install rules
#
install: $B/$n $L/$p $M/$m
uninstall:
	rm -f $B/$n $L/$p $M/$m
$D/%:
	mkdir -p $@
$B/$n: $n $B
	cp $^
$L/$p: $p $L
	cp $^
$M/$m: $m $M
	cp $^

# vim
#
install-vim: $V/syntax/$n.vim $V/ftdetect/$n.vim
uninstall-vim:
	rm -f $V/{syntax,ftdetect}/$n.vim
$V/%:
	mkdir -p $@
$V/%/$n.vim: vim/%/$n.vim $V/%
	cp $^

# misc tasks
#
# conditional execution helpers
define Wx
	which $1 1>/dev/null 2>&1 && $2 || $3
endef
define W
  $(call Wx, $1, $2, echo "$1 not installed")
endef
_=,# for commas in function calls :(

clean:
	@$(call Wx, git, rm -rf `git check-ignore *`, rm -f *.o $n $b perf.data)
perf: perf.data
	@$(call W, perf, perf report)
perf.data: $b $p
	@$(call W, perf, perf record ./$t)
valg: $b
	@$(call W, valgrind, valgrind $t)
sloc:
	@$(call W, cloc, cloc --by-file --force-lang=Lisp$_$n makefile *.{c$_h$_$n} test/*)
bits: $n $b
	stat -c "%n %sB" $^
repl: $b
	@$(call Wx, rlwrap, rlwrap $r -i, $r -i)

.PHONY:\
 	test clean perf valg sloc bits install\
 	install-vim uninstall uninstall-vim repl
