exception Unbound_identifier of string

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
