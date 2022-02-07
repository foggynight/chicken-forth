.PHONY: all
all:
	csc -o chicken-forth -O5 -d0 chicken-forth.scm

.PHONY: debug
debug:
	csc -o chicken-forth -O0 -d3 chicken-forth.scm
