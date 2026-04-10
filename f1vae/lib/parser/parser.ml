let parse (s : string) : Ast.prog =
  let lbuf = Lexing.from_string s in
  Grammar.parse Lexer.read lbuf
