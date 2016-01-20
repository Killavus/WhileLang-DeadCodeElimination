#mod_use "while_ast.ml";;
#mod_use "while_parser.ml";;
#mod_use "while_lexer.ml";;

let main argv argc =
  let input_file = open_in "examples/example.while" in
  let lexer_buffer = Lexing.from_channel input_file in
  let ast = While_parser.prog While_lexer.read lexer_buffer in 
  ast;;

