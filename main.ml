#mod_use "while_ast.ml";;
#mod_use "while_parser.ml";;
#mod_use "while_lexer.ml";;
#mod_use "data_flow.ml";;
#mod_use "lv_analysis.ml";;
#mod_use "unparser.ml";;

open Data_flow;;
open Lv_analysis;;
open Unparser;;

let debug_lv lv_analysis max_lab = 
    print_endline "Live Variables analysis results: ";
    List.iter (fun n ->
      let print_set_contents = 
        IdentSet.iter (fun x -> print_string (x ^ " ")) in
      print_string "Entry (";
      print_string (string_of_int n);
      print_string "): ";
      print_set_contents (fst (CIMap.find n lv_analysis));
      print_newline();
      print_string "Exit (";
      print_string (string_of_int n);
      print_string "): ";
      print_set_contents (snd (CIMap.find n lv_analysis));
      print_newline()) (range (max_lab-1));;

let debug_dfg dfg max_lab =
  print_endline "Data flow graph:";
  List.iter (fun n ->
    let print_children =
      List.iter (fun l -> print_string ((string_of_int l) ^ " ")) in
    print_string ((string_of_int n) ^ ": ");
    print_children (EBSet.find n dfg).children;
    print_newline()) (range (max_lab-1));;

let input_fname =
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else
    "examples/example.while";;

let main argv argc =
  let input_file = open_in input_fname in
  let lexer_buffer = Lexing.from_channel input_file in
  let ast = While_parser.prog While_lexer.read lexer_buffer in 
  let (dfg, max_lab) = build_data_flow_graph ast in
  let lv_analysis = perform dfg max_lab in
    debug_lv lv_analysis max_lab;
    debug_dfg dfg max_lab;
    let unparsed_ast = unparse ast in
      let ast_ = While_parser.prog While_lexer.read (Lexing.from_string unparsed_ast) in
      print_string (string_of_bool (ast = ast_));; 

main () ();;
