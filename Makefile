all:
	ocamlbuild -libs graphics life.native

clean:
	ocamlbuild -clean
