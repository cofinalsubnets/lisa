ifeq ($(shell whoami), root)
PREFIX ?= /usr/local
VIMDIR ?= /usr/share/vim/vimfiles
else
PREFIX ?= $(HOME)/.local
VIMDIR ?= $(HOME)/.vim
endif

LC_ALL=C
