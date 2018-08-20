test:
	ocamlbuild -pkg oUnit -pkg graphics -pkg ANSITerminal -use-ocamlfind test.byte && ./test.byte

compile:
	ocamlbuild -pkg graphics -pkg ANSITerminal -use-ocamlfind main.byte

play:
	ocamlbuild -pkg graphics -pkg ANSITerminal -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean

zip:
	zip src.zip *.ml* README.txt Makefile
