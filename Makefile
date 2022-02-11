LC_ALL=C

n=lips
b=$n.bin

bins=bin/lips
docs=share/man/man1/lips.1
libs=lib/lips/prelude.lips
files=$(bins) $(libs) $(docs)

# command to run the tests
t=bin/$b -_ $(libs) $(sort $(wildcard test/*))

test: bin/$b
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $t"

ifeq ($(shell whoami), root)
PREFIX ?= /usr/local
else
PREFIX ?= $(HOME)/.local
endif

CC = gcc
CPPFLAGS = -DPREFIX=\"$(PREFIX)\"
CFLAGS = -std=gnu17 -g -O2 -flto -Wall -Werror\
	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-stack-protector -fno-unroll-loops\
	-fno-inline -fno-align-functions

# build config
#
# target directory
# dist binary
bin/$n: bin/$n.bin
	strip -o $@ $^
# dev binary
bin/$n.bin: $(patsubst %.c,%.o, $(sort $(wildcard src/*.c)))
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^

# install target
D=$(DESTDIR)$(PREFIX)
$D/%: %
	install -D $^ $@
# install rules
install: $(addprefix $D/,$(files))
uninstall:
	rm -f $(addprefix $D/,$(files))

install-vim:
	make -C vim install
uninstall-vim:
	make -C vim uninstall

# misc tasks
#
clean:
	rm -rf `git check-ignore * */*`
perf: perf.data
	perf report
perf.data: bin/$b $(libs)
	perf record ./$t
valg: bin/$b
	valgrind --error-exitcode=1 $t
sloc:
	cloc --force-lang=Lisp,$n *
bits: bin/$n bin/$b
	du -h $^
repl_cmd=bin/$n.bin -_i $(libs) test/{00-helpers,01-k,k1,nf}.lips
repl: bin/$n
	which rlwrap && rlwrap $(repl_cmd) || $(repl_cmd)

.PHONY:\
 	test clean perf valg sloc bits repl\
	install uninstall install-vim uninstall-vim
