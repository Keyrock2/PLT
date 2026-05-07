module F = Format
open Ast
open Env

type t =
  | NumV of int
  | ClosureV of string * Ast.expr * t Env.t
  | BoolV of bool

let rec pp fmt = function
  | NumV n -> F.fprintf fmt "(NumV %d)" n
  | BoolV b -> F.fprintf fmt "(BoolV %b)" b
  | ClosureV (x, e, _env) ->
      F.fprintf fmt "ClosureV (\"%s\", %a, <env>)" x Ast.pp e

let rec compare v1 v2 =
  match v1, v2 with
  | NumV n1, NumV n2 -> n1 = n2
  | BoolV b1, BoolV b2 -> b1 = b2
  | ClosureV (x1, e1, env1), ClosureV (x2, e2, env2) ->
      String.equal x1 x2 && e1 = e2 && env1 = env2
  | _ -> false
