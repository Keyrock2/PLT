%{
  open Ast
%}
%token <int> NUMBER
%token <string> ID
%token PLUS MINUS STAR SLASH
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token TILDE EQUAL
%token LET IN FUN ARROW
%token EOF
%start <Ast.expr> parse
%%

parse:
  | e=expr EOF { e }
;

expr:
  | LET x=ID xs=nonempty_list(ID) EQUAL e1=expr IN e2=expr
      { Let (x, List.fold_right (fun p body -> Lambda (p, body)) xs e1, e2) }
  | LET x=ID EQUAL e1=expr IN e2=expr { Let (x, e1, e2) }
  | additive_expr { $1 }
;

additive_expr:
  | e1=additive_expr PLUS  e2=mul_expr { Add (e1, e2) }
  | e1=additive_expr MINUS e2=mul_expr { Sub (e1, e2) }
  | mul_expr { $1 }
;

mul_expr:
  | e1=mul_expr STAR  e2=app_expr { Mul (e1, e2) }
  | e1=mul_expr SLASH e2=app_expr { Div (e1, e2) }
  | app_expr { $1 }
;

app_expr:
  | e1=app_expr e2=atom { App (e1, e2) }
  | MINUS n=NUMBER      { Num (- n) }
  | atom                { $1 }
;

atom:
  | n=NUMBER                                               { Num n }
  | x=ID                                                   { Id x }
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS              { e }
  | TILDE LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS        { Sub (Num 0, e) }
  | LEFT_PARENTHESIS FUN xs=nonempty_list(ID) ARROW e=expr RIGHT_PARENTHESIS
      { List.fold_right (fun x body -> Lambda (x, body)) xs e }
;
