open Value

(* 변수를 찾지 못했을 때의 예외 *)
exception Unbound_identifier of string

type t = (string * Value.t) list

let empty : t = []

let rec find (x : string) (env : t) : Value.t =
  match env with
  | [] -> raise (Unbound_identifier x)
  | (k, v) :: tl ->
      if String.equal k x then v
      else find x tl

let add (x : string) (v : Value.t) (env : t) : t =
  let env_without_x = List.remove_assoc x env in
  (x, v) :: env_without_x
