nom=gwen
VIMPREFIX ?= $(HOME)/.vim
vim_files=$(addprefix $(VIMPREFIX)/,syntax/$(nom).vim ftdetect/$(nom).vim)

install: $(vim_files)
uninstall:
	rm -f $(vim_files)
$(VIMPREFIX)/%: %
	install -D -m 644 $^ $@

.PHONY: install uninstall
