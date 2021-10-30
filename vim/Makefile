include ../config.mk

V=$(VIMDIR)
files=$V/syntax/$n.vim $V/ftdetect/$n.vim
install: $(files)
uninstall:
	rm -f $(files)
$V/%:
	mkdir -p $@
$V/%/$n.vim: %/$n.vim $V/ftdetect/
	cp $^

.PHONY: install
