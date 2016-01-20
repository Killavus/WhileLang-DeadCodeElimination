{
    open Lexing
    open While_parser
    exception SyntaxError of string

    let next_line lexer_buffer =
      let current_position = lexer_buffer.lex_curr_p in
        lexer_buffer.lex_curr_p <-
            { current_position with pos_bol = lexer_buffer.lex_curr_pos;
	                            pos_lnum = current_position.pos_lnum + 1            
            };;    
}

let digit = ['0'-'9']
let integer_const = '-'? digit digit*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
    parse
    | white { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | "true" { TRUE }
    | "false" { FALSE }
    | "skip" { SKIP }
    | "while" { WHILE }
    | "do" { DO }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "not" { LOG_NEG }
    | "and" { LOGOP_AND }
    | "or" { LOGOP_OR }
    | ">=" { RELOP_GTE }
    | "<=" { RELOP_LTE }
    | ":=" { ASSIGN }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | ';' { SEMICOL }
    | '>' { RELOP_GT }
    | '<' { RELOP_LT }
    | '=' { RELOP_EQ }
    | '+' { ARITHOP_PLUS }
    | '-' { ARITHOP_MINUS }
    | '/' { ARITHOP_DIV }
    | '*' { ARITHOP_MULT }
    | integer_const { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | ident { IDENT (Lexing.lexeme lexbuf) }
    | _ { raise (SyntaxError ("Unexpected token: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }
