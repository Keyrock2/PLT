let parse (s : string) : Ast.expr =
  let lbuf = Lexing.from_string s in
  Grammar.parse Lexer.read lbuf
