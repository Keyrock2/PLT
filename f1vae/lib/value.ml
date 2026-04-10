module F = Format

type t = NumV of int

let pp fmt = function
  | NumV n -> F.fprintf fmt "(NumV %d)" n

let compare v1 v2 =
  match v1, v2 with
  | NumV n1, NumV n2 -> n1 = n2
