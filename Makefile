
QUOTES_DIR := $(shell pwd)

quotes: quotes.asd quotes.lisp
	sbcl --eval '(pushnew (truename "$(QUOTES_DIR)/") asdf:*central-registry*)' \
	     --eval '(ql:quickload :quotes)' \
	     --eval '(asdf:make :quotes)' \
	     --eval '(quit)'
