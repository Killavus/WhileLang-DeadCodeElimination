type ident = string;;

type bool_type = And | Or;;
type arith_type = Add | Sub | Div | Mult;;
type rel_type = LessThan | LessThanEq | GreaterThan | GreaterThanEq | Equal;;

type arith_expr =
    AConst of int |
    AIdent of ident |
    Op of (arith_type * arith_expr * arith_expr);;

type bool_expr =
    BConst of bool |
    BIdent of ident |
    Neg of bool_expr |
    RelOp of (rel_type * arith_expr * arith_expr) |
    BoolOp of (bool_type * bool_expr * bool_expr);;

type stmt =
    IfStmt of (bool_expr * stmt * stmt) | 
    WhileStmt of (bool_expr * stmt) |
    CompStmt of (stmt * stmt) |
    AssignStmt of (ident * arith_expr) |
    SkipStmt;;

