module F = Format

type expr = Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Id of string
  | Let of string * expr * expr

let rec pp fmt e =
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Add (e1, e2) -> F.fprintf fmt "%a + %a" pp e1 pp e2
  | Sub (e1, e2) -> F.fprintf fmt "%a - %a" pp e1 pp e2
  | Mul (e1, e2) -> F.fprintf fmt "%a * %a" pp e1 pp e2
  | Div (e1, e2) -> F.fprintf fmt "%a / %a" pp e1 pp e2
  | Id x -> F.fprintf fmt "%s" x
  | Let (x, e1, e2) -> F.fprintf fmt "let %s = %a in %a" x pp e1 pp e2

let rec pp_ast fmt e =
  match e with
  | Num n -> F.fprintf fmt "(Num %d)" n
  | Add (e1, e2) -> F.fprintf fmt "(Add, %a, %a)" pp_ast e1 pp_ast e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub, %a, %a)" pp_ast e1 pp_ast e2
  | Mul (e1, e2) -> F.fprintf fmt "(Mul, %a, %a)" pp_ast e1 pp_ast e2
  | Div (e1, e2) -> F.fprintf fmt "(Div, %a, %a)" pp_ast e1 pp_ast e2
  | Id x -> F.fprintf fmt "(Id %s)" x
  | Let (x, e1, e2) -> F.fprintf fmt "(Let %s, %a, %a)" x pp_ast e1 pp_ast e2
