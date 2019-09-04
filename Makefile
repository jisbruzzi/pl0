default: main

main: main.native

test: test.native

%.native: 
	ocamlbuild -lib str -use-ocamlfind $@
	mv $@ $*

.PHONY: test default
