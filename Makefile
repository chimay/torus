# vim: set filetype=make:

FICHIERS_ELC = $(patsubst %.el,%.elc,$(wildcard *.el))

.DEFAULT_GOAL := all
.PHONY: all install

all: $(FICHIERS_ELC)

clean:
	rm -f *.elc

%.elc: %.el
	emacs --batch \
		--eval "(add-to-list 'load-path \"$(shell pwd)\")" \
		--eval '(byte-compile-file "$<")' --kill
