exception Unbound_identifier of string

(* 추상 메모리의 형태 정의 *)
type t = (string * Value.t) list

(* 빈 추상 메모리 *)
let empty : t = []

(* 변수 검색 함수 *)
let rec find (x : string) (env : t) : Value.t =
  match env with
  | [] -> raise (Unbound_identifier x)
  | (k, v) :: tl ->
      if String.equal k x then v
      else find x tl

(* 변수 추가 (기존에 있다면 제거 후 맨 앞에 추가) *)
let add (x : string) (v : Value.t) (env : t) : t =
  (* 기존 변수를 안전하게 제거 *)
  let env_without_x = List.remove_assoc x env in
  (x, v) :: env_without_x
