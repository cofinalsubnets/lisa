nom=lisa
suff=la

build_dir=build
test_dir=test
doc_dir=doc
lib_dir=lib

debug_build=build/$(nom).dbg

boot=$(lib_dir)/boot.$(suff)
lib=$(boot)


# installation
DESTDIR ?= $(HOME)
PREFIX ?= .local

run_tests=$(debug_build) -_ $(boot) $(test_dir)/*.$(suff)

test: build/lisa.dbg
	make -C test

test-lots:
	make -C test lots

build/%:
	make -C src ../$@

dest_dir=$(DESTDIR)/$(PREFIX)/
install_bin=\
	$(dest_dir)bin/$(nom)
install_lib=\
	$(addprefix $(dest_dir)lib/$(nom)/,$(notdir $(lib)))\
install_doc=\
	$(dest_dir)share/man/man1/$(nom).1

install_files=$(install_bin) $(install_lib) $(install_doc)
install: $(install_files)
uninstall:
	rm -f $(install_files)

$(dest_dir)bin/%: build/%
	install -D $^ $@
$(dest_dir)share/man/man1/%: $(doc_dir)/%
	install -D $^ $@
$(dest_dir)lib/$(nom)/%: lib/%
	install -D $^ $@

# vim stuff
vim_files=$(addprefix $(VIMPREFIX)/,syntax/$(nom).vim ftdetect/$(nom).vim)

install-vim: $(vim_files)
uninstall-vim:
	rm -f $(vim_files)

VIMPREFIX ?= $(HOME)/.vim
$(VIMPREFIX)/%: vim/%
	install -D $^ $@

# other tasks
#
clean:
	rm -f `git check-ignore * */*`

repl: build/$(nom).dbg
	which rlwrap && rlwrap $(run_tests) -i || $(run_tests) -i

# profiling on linux with perf
perf: perf.data
	perf report
perf.data: $(debug_build) $(lib)
	perf record $(run_tests)

# valgrind detects some memory errors
valg: build/$(nom).dbg
	valgrind --error-exitcode=1 $(run_tests)
# approximate lines of code
sloc:
	cloc --force-lang=Lisp,$(suff) *
# size of binaries
bits: build/$(nom) build/$(nom).dbg
	du -h $^

.PHONY: test test-lots repl clean\
	install uninstall\
 	install-vim uninstall-vim\
 	sloc bits valg perf
