module F = Format
open Ast
open Env

(* 함수(ClosureV)를 값으로 취급하기 위해 Value.t 확장 *)
type t =
  | NumV of int
  | ClosureV of string * Ast.expr * t Env.t

let rec pp fmt = function
  | NumV n -> F.fprintf fmt "(NumV %d)" n
  | ClosureV (x, e, _env) ->
      F.fprintf fmt "ClosureV (\"%s\", %a, <env>)" x Ast.pp e

(* Alcotest 검증을 위한 비교 함수 *)
let rec compare v1 v2 =
  match v1, v2 with
  | NumV n1, NumV n2 -> n1 = n2
  | ClosureV (x1, e1, env1), ClosureV (x2, e2, env2) ->
      String.equal x1 x2 && e1 = e2 && env1 = env2
  | _ -> false
