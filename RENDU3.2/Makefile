all:
	ocamlbuild -libs unix,str main.native;
	ln -fs main.native resol

byte: 
	ocamlbuild -v main.byte

clean: 
	ocamlbuild -clean
