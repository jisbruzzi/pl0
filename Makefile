default: main

main: main.native

test: test.native

clean: 
	rm -rf _build

%.native: 
	ocamlbuild -lib str -use-ocamlfind $@
	mv $@ $*

.PHONY: test default
