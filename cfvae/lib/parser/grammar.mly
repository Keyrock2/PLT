%{
  open Ast
%}
%token <int> NUMBER
%token <string> ID
%token PLUS MINUS STAR SLASH
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token TILDE EQUAL LT GT GEQ LEQ
%token AND OR NOT
%token LET IN FUN ARROW
%token TRUE FALSE IF THEN ELSE
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
  | IF e1=expr THEN e2=expr ELSE e3=expr { Cond (e1, e2, e3) }
  | FUN xs=nonempty_list(ID) ARROW e=expr
      { List.fold_right (fun x body -> Lambda (x, body)) xs e }
  | or_expr { $1 }
;

or_expr:
  | e1=or_expr OR e2=and_expr { Cond (e1, Bool true, Cond (e2, Bool true, Bool false)) }
  | and_expr { $1 }
;

and_expr:
  | e1=and_expr AND e2=not_expr { Cond (e1, Cond (e2, Bool true, Bool false), Bool false) }
  | not_expr { $1 }
;

not_expr:
  | NOT e=not_expr { Cond (e, Bool false, Bool true) }
  | cmp_expr { $1 }
;

cmp_expr:
  | e1=cmp_expr LT  e2=additive_expr { Lt (e1, e2) }
  | e1=cmp_expr GT  e2=additive_expr { Lt (e2, e1) }
  | e1=cmp_expr GEQ e2=additive_expr { Cond (Lt (e1, e2), Bool false, Bool true) }
  | e1=cmp_expr LEQ e2=additive_expr { Cond (Lt (e2, e1), Bool false, Bool true) }
  | e1=cmp_expr EQUAL e2=additive_expr
      { Cond (Lt (e1, e2), Bool false, Cond (Lt (e2, e1), Bool false, Bool true)) }
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
  | TRUE                                                   { Bool true }
  | FALSE                                                  { Bool false }
  | x=ID                                                   { Id x }
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS              { e }
  | TILDE LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS        { Sub (Num 0, e) }
;
