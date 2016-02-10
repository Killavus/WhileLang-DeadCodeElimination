open While_ast;;
open Utils;;

let aexpr_need_paren aexpr =
  match aexpr with
  | AIdent _ -> false
  | AConst _ -> false
  | _ -> true;;

let bexpr_need_paren bexpr =
  match bexpr with
  | BConst _ -> false
  | _ -> true;;

let rec unparse_aexpr aexpr =
  match aexpr with
  | AConst n -> string_of_int n
  | AIdent id -> id
  | Op (op, aexpr_l, aexpr_r) ->
      let sign = (match op with
                  | Add -> " + "
                  | Sub -> " - "
                  | Div -> " / "
                  | Mult -> " * ") in
      (if aexpr_need_paren aexpr_l then
        "(" ^ unparse_aexpr aexpr_l ^ ")"
      else
        unparse_aexpr aexpr_l) ^ sign ^ 
      (if aexpr_need_paren aexpr_r then
        "(" ^ unparse_aexpr aexpr_r ^ ")"
       else
        unparse_aexpr aexpr_r);;

let rec unparse_bexpr bexpr =
  match bexpr with
  | BConst t ->
      if t then "true" else "false"
  | Neg bexpr_ -> "not " ^ 
      (if bexpr_need_paren bexpr_ then
        "(" ^ unparse_bexpr bexpr_ ^ ")"
      else
        unparse_bexpr bexpr_)
  | RelOp (op, aexpr_l, aexpr_r) ->
      let sign = (match op with
                  | LessThan -> " < "
                  | LessThanEq -> " <= "
                  | GreaterThan -> " > "
                  | GreaterThanEq -> " >= "
                  | Equal -> " = ") in
      unparse_aexpr aexpr_l ^ sign ^ unparse_aexpr aexpr_r
  | BoolOp (op, bexpr_l, bexpr_r) ->
      let oper = (match op with
                  | And -> " and "
                  | Or -> " or ") in
      (if bexpr_need_paren bexpr_l then
        "(" ^ unparse_bexpr bexpr_l ^ ")"
       else
         unparse_bexpr bexpr_l) ^ oper ^ 
      (if bexpr_need_paren bexpr_r then
        "(" ^ unparse_bexpr bexpr_r ^ ")"
       else
         unparse_bexpr bexpr_r);;

let indentate indent =
  List.fold_right (^) (repeat "\t" indent) "";;

let rec unparse_stmt stmt level =
  match stmt with
  | IfStmt (cond, truebody, falsebody, _) ->
      indentate level ^ 
      "if " ^ unparse_bexpr cond ^ " then\n" ^
        (let tbody = unparse_stmt truebody (level + 1) ^ "\n" in
          if tbody = "\n" then 
            indentate (level+1) ^ "skip\n" 
          else 
            tbody) ^
      indentate level ^ 
      "else\n" ^
        (let fbody = unparse_stmt falsebody (level + 1) ^ "\n" in
          if fbody = "\n" then
            indentate (level + 1) ^ "skip\n"
          else
            fbody)
  | WhileStmt (cond, body, _) ->
      indentate level ^
      "while " ^ unparse_bexpr cond ^ " do\n" ^
        (let bodyup = unparse_stmt body (level + 1) ^ "\n" in
          if bodyup = "\n" then
            indentate (level + 1) ^ "skip\n"
          else
            bodyup)
  | AssignStmt (id, aexpr, _) ->
      indentate level ^ id ^ " := " ^ unparse_aexpr aexpr
  | SkipStmt (_) -> indentate level ^ "skip"
  | CompStmt (stmt1, stmt2, _) ->
      let needs_semicol_s1 = (match stmt1 with
                              | IfStmt _ -> false
                              | WhileStmt _ -> false
                              | DeadExpr -> false
                              | _ -> true) and
          needs_semicol_s2 = (match stmt2 with
                              | DeadExpr -> false
                              | _ -> true) in
      unparse_stmt stmt1 level ^ 
      (if needs_semicol_s1 && needs_semicol_s2 then ";\n" else "") ^
      unparse_stmt stmt2 level
  | DeadExpr -> "";;

let unparse ast = unparse_stmt ast 0;;
