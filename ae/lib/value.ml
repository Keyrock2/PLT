module F = Format

(* AE result only contains NumV *)
type t = NumV of int

(* Alcotest Pretty-printer *)
let pp fmt = function
  | NumV i -> F.fprintf fmt "(NumV %d)" i

(* function compare ae *)
let compare v1 v2 =
  match v1, v2 with
  | NumV n1, NumV n2 -> n1 = n2
