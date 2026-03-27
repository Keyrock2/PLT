open Ast
open Value

(* iterate AST node *)
let rec eval (e: expr) : int =
  match e with
  | Num n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) ->
      let v2 = eval e2 in
      (* if v2 = 0 then raise Division_by_zero else eval e1 / v2 *)
      if v2 = 0 then raise Division_by_zero
      else eval e1 / v2

(* input string parse and evaluate *)
let interp (p: string) : Value.t =
  let ast = Parser.parse p in
  NumV (eval ast)
