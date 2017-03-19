snick : lexer.cmx parser.cmx snick.cmx
	ocamlopt -o snick lexer.cmx parser.cmx snick.cmx

lexer.cmx : lexer.ml parser.cmi
	ocamlopt -c lexer.ml

lexer.ml : lexer.mll
	ocamllex lexer.mll

parser.cmx : parser.ml parser.cmi
	ocamlopt -c parser.ml

parser.ml : parser.mly
	ocamlyacc parser.mly

parser.cmi : parser.mli
	ocamlopt -c parser.mli

parser.mli : parser.mly
	ocamlyacc parser.mly

snick.cmx : snick.ml
	ocamlopt -c snick.ml

clean : 
	rm ./snick *.cmx *.o *.mli *.cmi lexer.ml parser.ml