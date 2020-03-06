test:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal game_test.byte && ./game_test.byte

play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
