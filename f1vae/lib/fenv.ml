open Ast

(* 함수를 찾지 못했을 때의 예외 *)
exception Unknown_function of string

(* (함수이름 * (파라미터리스트 * 함수몸체)) list *)
type t = (string * (string list * expr)) list

let empty : t = []

let add (f : string) (params : string list) (body : expr) (fenv : t) : t =
  let fenv_without_f = List.remove_assoc f fenv in
  (f, (params, body)) :: fenv_without_f

let rec find (f : string) (fenv : t) : string list * expr =
  match fenv with
  | [] -> raise (Unknown_function f)
  | (k, v) :: tl ->
      if String.equal k f then v
      else find f tl
