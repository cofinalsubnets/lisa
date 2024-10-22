nom=gwen
optimized_binary=gwen.bin
trampoline_binary=gwen.notco.bin
prelude=prelude.gw
tests=$(sort $(wildcard test/*.gw))

test_all: test_optimized test_trampoline
test_optimized: $(optimized_binary)
	@echo '[optimized]'
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time ./$< $(prelude) $(tests)"
test_trampoline: $(trampoline_binary)
	@echo '[trampolining]'
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time ./$< $(prelude) $(tests)"

#build
CC=gcc
CFLAGS=-std=gnu11 -g -O2 -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-asynchronous-unwind-tables -fno-stack-protector
cc=$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)

gwen.bin: main.c gwen.o
	$(cc) $^ -o $@
gwen.o: gwen.c gwen.h
	$(cc) -c $< -o $@
gwen.notco.bin: main.c gwen.notco.o
	$(cc) -DGwenCanUseTco=0 $^ -o $@
gwen.notco.o: gwen.c gwen.h
	$(cc) -c -DGwenCanUseTco=0 $< -o $@

# installlation

# all the local files to install
source_binary=gwen.bin
source_static_library=libgwen.a
source_prelude=prelude.gw
source_c_header=gwen.h
source_manpage=gwen.1
source_files=$(source_binary) $(source_static_library)\
						 $(source_prelude) $(source_c_header)\
						 $(source_manpage)

$(source_static_library): gwen.o
	ar rcs $@ $<
$(source_manpage): gwen.1.md
	pandoc -f markdown -t man $< > $@

# default install to the user's home directory under ~/.local/
DESTDIR ?= $(HOME)
PREFIX ?= .local
dest=$(DESTDIR)/$(PREFIX)

# all the target files
target_binary=$(dest)/bin/gwen
target_static_library=$(dest)/lib/libgwen.a
target_prelude=$(dest)/lib/gwen/prelude.gw
target_c_header=$(dest)/include/gwen.h
target_manpage=$(dest)/share/man/man1/gwen.1
target_files=$(target_binary) $(target_static_library)\
						 $(target_prelude) $(target_c_header)\
						 $(target_manpage)

$(target_binary): $(source_binary)
	install -D -m 755 -s $< $@
$(target_static_library): $(source_static_library)
	install -D -m 644 $< $@
$(target_manpage): $(source_manpage)
	install -D -m 644 $< $@
$(target_prelude): $(source_prelude)
	install -D -m 644 $< $@
$(target_c_header): $(source_c_header)
	install -D -m 644 $< $@

# require source files first so everything is built before trying to install anything
install: $(source_files) $(target_files)
uninstall:
	rm -f $(target_files)

# other tasks
#
clean:
	rm -r `git check-ignore * */*`
# valgrind detects some memory errors
valg: $(optimized_binary)
	valgrind --error-exitcode=1 ./$(optimized_binary) $(prelude) $(tests)
# approximate lines of code
sloc:
	cloc --force-lang=Lisp,gw * test/* lib/*
# size of binaries
bits: $(optimized_binary) $(trampoline_binary)
	du -h $^
disasm: $(optimized_binary)
	rizin -A $<
# profiling on linux
perf.data: $(optimized_binary)
	perf record ./$(optimized_binary) $(prelude) $(tests)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<
repl: $(optimized_binary) $(prelude)
	rlwrap ./$(optimized_binary) -i $(prelude)

.PHONY: default clean test test-lots\
	install uninstall\
 	sloc bits valg perf bench flame disasm repl
