ifeq ($(shell whoami), root)
V ?= /usr/share/vim/vimfiles
else
V ?= $(HOME)/.vim
endif

vimfiles=syntax/lips.vim ftdetect/lips.vim

$V/%: %
	install -D $^ $@

install: $(addprefix $V/,$(vimfiles))
uninstall:
	rm -f $(addprefix $V/,$(vimfiles))

.PHONY: install uninstall
