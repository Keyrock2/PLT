{
  open Grammar

  exception LexingError of string
}

(* Decimal number *)
let pos_digit = ['1'-'9']
let digit = ['0'-'9']
let pos_number = pos_digit digit*
let number = "0" | pos_number

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
  | "let" { LET }
  | "in"  { IN }
  | number as n { NUMBER (int_of_string n) }
  | id as s { ID s }
  | ws { read lexbuf }
  | eof { EOF }
  | _ { raise (LexingError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
