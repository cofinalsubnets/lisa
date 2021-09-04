ifeq ($(shell whoami), root)
PREFIX ?= /usr/local
else
PREFIX ?= $(HOME)/.local
endif

# fixnums need sign extended bitshifts.
# other things tend to break TCO ...
CFLAGS ?= -std=gnu17 -g -O2 -flto\
	-Wall -Wstrict-prototypes\
	-Wno-shift-negative-value\
	-fno-stack-protector\
	-fno-unroll-loops

CPPFLAGS ?= -DPREFIX=\"$(PREFIX)\"

CC ?= gcc

LC_ALL=C
