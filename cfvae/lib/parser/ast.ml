module F = Format

type expr = Num of int
  | Bool of bool
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lt of expr * expr
  | Id of string
  | Let of string * expr * expr
  | Lambda of string * expr
  | App of expr * expr
  | Cond of expr * expr * expr

let rec pp fmt e =
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Bool b -> F.fprintf fmt "%b" b
  | Add (e1, e2) -> F.fprintf fmt "%a + %a" pp e1 pp e2
  | Sub (e1, e2) -> F.fprintf fmt "%a - %a" pp e1 pp e2
  | Mul (e1, e2) -> F.fprintf fmt "%a * %a" pp e1 pp e2
  | Div (e1, e2) -> F.fprintf fmt "%a / %a" pp e1 pp e2
  | Lt (e1, e2) -> F.fprintf fmt "%a < %a" pp e1 pp e2
  | Id x -> F.fprintf fmt "%s" x
  | Let (x, e1, e2) -> F.fprintf fmt "let %s = %a in %a" x pp e1 pp e2
  | Lambda (x, e) -> F.fprintf fmt "(fun %s -> %a)" x pp e
  | App (e1, e2) -> F.fprintf fmt "(%a %a)" pp e1 pp e2
  | Cond (e1, e2, e3) -> F.fprintf fmt "if %a then %a else %a" pp e1 pp e2 pp e3

let rec pp_ast fmt e =
  match e with
  | Num n -> F.fprintf fmt "(Num %d)" n
  | Bool b -> F.fprintf fmt "(Bool %b)" b
  | Add (e1, e2) -> F.fprintf fmt "(Add, %a, %a)" pp_ast e1 pp_ast e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub, %a, %a)" pp_ast e1 pp_ast e2
  | Mul (e1, e2) -> F.fprintf fmt "(Mul, %a, %a)" pp_ast e1 pp_ast e2
  | Div (e1, e2) -> F.fprintf fmt "(Div, %a, %a)" pp_ast e1 pp_ast e2
  | Lt (e1, e2) -> F.fprintf fmt "(Lt, %a, %a)" pp_ast e1 pp_ast e2
  | Id x -> F.fprintf fmt "(Id %s)" x
  | Let (x, e1, e2) -> F.fprintf fmt "(Let %s, %a, %a)" x pp_ast e1 pp_ast e2
  | Lambda (x, e) -> F.fprintf fmt "(Lambda %s, %a)" x pp_ast e
  | App (e1, e2) -> F.fprintf fmt "(App, %a, %a)" pp_ast e1 pp_ast e2
  | Cond (e1, e2, e3) -> F.fprintf fmt "(Cond, %a, %a, %a)" pp_ast e1 pp_ast e2 pp_ast e3
