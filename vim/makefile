n=lips

# vim stuff
vim=~/.vim

install: $(vim)/syntax/$n.vim $(vim)/ftdetect/$n.vim

$(vim)/ftdetect:
	mkdir -p $@
$(vim)/syntax:
	mkdir -p $@
$(vim)/ftdetect/$n.vim: ftdetect/$n.vim $(vim)/ftdetect/
	cp $^
$(vim)/syntax/$n.vim: syntax/$n.vim $(vim)/syntax/
	cp $^

.PHONY: install
