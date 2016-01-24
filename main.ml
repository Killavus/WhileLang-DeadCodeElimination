open Data_flow;;
open Lv_analysis;;

let main argv argc =
  let input_file = open_in "examples/example.while" in
  let lexer_buffer = Lexing.from_channel input_file in
  let ast = While_parser.prog While_lexer.read lexer_buffer in 
  let (dfg, max_lab) = build_data_flow_graph ast in
  perform dfg max_lab;;
