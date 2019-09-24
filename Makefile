default: main

main: main.native

test: test.native
	./test

clean: 
	rm -rf _build

%.native: 
	ocamlbuild -lib str -use-ocamlfind $@
	mv $@ $*

%.byte: 
	ocamlbuild -lib str -use-ocamlfind $@
	mv $@ $*

.PHONY: test default
