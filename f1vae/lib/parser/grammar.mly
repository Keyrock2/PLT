%{
  open Ast
%}
%token <int> NUMBER
%token <string> ID
%token PLUS MINUS STAR SLASH
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token TILDE EQUAL COMMA SEMISEMI
%token LET IN FUN
%token EOF
%nonassoc IN
%left PLUS MINUS
%left STAR SLASH
%start <Ast.prog> parse
%%

parse:
  | p=prog EOF { p }
;
prog:
  | decls=list(decl) e=expr { Prog (decls, e) }
;
decl:
  | FUN f=ID LEFT_PARENTHESIS params=separated_list(COMMA, ID) RIGHT_PARENTHESIS EQUAL e=expr SEMISEMI
    { FunDef (f, params, e) }
;
expr:
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  | e1=expr STAR e2=expr { Mul (e1, e2) }
  | e1=expr SLASH e2=expr { Div (e1, e2) }
  | LET x=ID EQUAL e1=expr IN e2=expr { Let (x, e1, e2) }
  | f=ID LEFT_PARENTHESIS args=separated_list(COMMA, expr) RIGHT_PARENTHESIS { Call (f, args) }
  | n=NUMBER { Num n }
  | MINUS n=NUMBER { Num (-1 * n) }
  | x=ID { Id x }
  | e=paren_expr { e }
  ;
paren_expr:
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS { e }
  | TILDE LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS { Sub (Num 0, e) }
  ;
