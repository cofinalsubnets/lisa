include ../config.mk

VIMPREFIX ?= $(HOME)/.vim
vimfiles=$(addprefix $(VIMPREFIX)/,syntax/$(lang).vim ftdetect/$(lang).vim)
$(VIMPREFIX)/%: %
	install -D $^ $@
install-vim: $(vimfiles)
uninstall-vim:
	rm -f $(vimfiles)
