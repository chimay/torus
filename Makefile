
.DEFAULT_GOAL := all
.PHONY: all install

all:
	emacs --batch --eval '(byte-compile-file "torus.el")' --kill

clean:
	rm -f *.elc
