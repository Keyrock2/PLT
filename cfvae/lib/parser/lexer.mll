{
  open Grammar

  exception LexingError of string
}

(* Decimal number *)
let digit = ['0'-'9']
let number = digit+

(* White space *)
let ws = [' ''\t''\n']*

(* Identifier *)
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "(" { LEFT_PARENTHESIS }
  | ")" { RIGHT_PARENTHESIS }
  | "~" { TILDE }
  | "=" { EQUAL }
  | "->" { ARROW }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "let"   { LET }
  | "in"    { IN }
  | "fun"   { FUN }
  | "not"   { NOT }
  | "<="    { LEQ }
  | ">="    { GEQ }
  | "<"     { LT }
  | ">"     { GT }
  | "&&"    { AND }
  | "||"    { OR }
  | number as n { NUMBER (int_of_string n) }
  | id as s { ID s }
  | ws { read lexbuf }
  | eof { EOF }
  | _ { raise (LexingError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
