parser_lexer:
	opam config exec -- ocamlyacc while_parser.mly
	opam config exec -- ocamllex while_lexer.mll

clean:
	rm -f while_lexer.ml while_parser.ml while_parser.mli
