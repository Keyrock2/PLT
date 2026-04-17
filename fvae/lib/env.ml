(* 변수를 찾지 못했을 때의 예외 *)
exception Unbound_identifier of string

(* 다형 타입으로 정의하여 Value.t와의 순환 참조를 방지합니다. *)
type 'a t = (string * 'a) list

let empty : 'a t = []

let rec find (x : string) (env : 'a t) : 'a =
  match env with
  | [] -> raise (Unbound_identifier x)
  | (k, v) :: tl ->
      if String.equal k x then v
      else find x tl

let add (x : string) (v : 'a) (env : 'a t) : 'a t =
  let env_without_x = List.remove_assoc x env in
  (x, v) :: env_without_x
