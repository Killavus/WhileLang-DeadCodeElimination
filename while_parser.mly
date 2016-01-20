%{ open While_ast %}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token LOG_NEG LOGOP_AND LOGOP_OR
%token IF THEN ELSE 
%token WHILE DO 
%token SKIP
%token ASSIGN
%token ARITHOP_PLUS ARITHOP_MINUS ARITHOP_DIV ARITHOP_MULT
%token RELOP_EQ RELOP_LT RELOP_LTE RELOP_GT RELOP_GTE
%token LPAREN RPAREN SEMICOL
%token EOF

%right LOG_NEG
%left ARITHOP_PLUS ARITHOP_MINUS LOGOP_AND
%left ARITHOP_MULT ARITHOP_DIV LOGOP_OR

%right SEMICOL

%start prog
%type <stmt> prog
%type <stmt> stm
%type <stmt> while_stm
%type <stmt> if_stm
%type <bool_expr> bool_exp
%type <arith_expr> arith_exp

%%

prog:
 | stm EOF { $1 }
 | EOF { SkipStmt }

stm:
  | if_stm; prog { CompStmt ($1, $2) }
  | while_stm; prog { CompStmt ($1, $2) }
  | stm; SEMICOL; stm { CompStmt ($1, $3) }
  | IDENT; ASSIGN; arith_exp { AssignStmt ($1, $3) }
  | SKIP { SkipStmt }

if_stm:
  | IF; bool_exp; THEN; stm; ELSE; stm { IfStmt ($2, $4, $6) }

while_stm:
  | WHILE; bool_exp; DO; stm { WhileStmt ($2, $4) }    

bool_exp:
  | LPAREN; bool_exp_without_ident; RPAREN { $2 }
  | bool_exp_without_ident { $1 }
  | IDENT { Ident $1 }

bool_exp_without_ident:
  | arith_exp; RELOP_EQ; arith_exp { RelOp (Equal, $1, $3) }
  | arith_exp; RELOP_LT; arith_exp { RelOp (LessThan, $1, $3) }
  | arith_exp; RELOP_LTE; arith_exp { RelOp (LessThanEq, $1, $3) }
  | arith_exp; RELOP_GT; arith_exp { RelOp (GreaterThan, $1, $3) }
  | arith_exp; RELOP_GTE; arith_exp { RelOp (GreaterThanEq, $1, $3) }
  | bool_exp; LOGOP_AND; bool_exp { BoolOp (And, $1, $3) }
  | bool_exp; LOGOP_OR; bool_exp { BoolOp (Or, $1, $3) }
  | LOG_NEG; bool_exp { Neg $2 }
  | TRUE { Const true }
  | FALSE { Const false };

arith_exp:
  | LPAREN; arith_exp_without_ident; RPAREN { $2 }
  | arith_exp_without_ident { $1 }
  | IDENT { Ident $1 }

arith_exp_without_ident:
  | arith_exp; ARITHOP_PLUS; arith_exp { Op (Add, $1, $3) }
  | arith_exp; ARITHOP_MINUS; arith_exp { Op (Sub, $1, $3) }
  | arith_exp; ARITHOP_MULT; arith_exp { Op (Mult, $1, $3) }
  | arith_exp; ARITHOP_DIV; arith_exp { Op (Div, $1, $3) }
  | INT { Const $1 }  