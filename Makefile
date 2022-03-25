.PHONY: all
all:
	csc -o chicken-forth -O5 -d0 -strict-types chicken-forth.scm

.PHONY: static
static:
	csc -o chicken-forth -O5 -d0 -static -strict-types chicken-forth.scm

.PHONY: debug
debug:
	csc -o chicken-forth -O0 -d3 -strict-types chicken-forth.scm
