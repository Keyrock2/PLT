module F = Format

type expr = Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Id of string
  | Let of string * expr * expr
  | Call of string * expr list

type fdef = FunDef of string * string list * expr

type prog = Prog of fdef list * expr

let rec pp fmt e =
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Add (e1, e2) -> F.fprintf fmt "%a + %a" pp e1 pp e2
  | Sub (e1, e2) -> F.fprintf fmt "%a - %a" pp e1 pp e2
  | Mul (e1, e2) -> F.fprintf fmt "%a * %a" pp e1 pp e2
  | Div (e1, e2) -> F.fprintf fmt "%a / %a" pp e1 pp e2
  | Id x -> F.fprintf fmt "%s" x
  | Let (x, e1, e2) -> F.fprintf fmt "let %s = %a in %a" x pp e1 pp e2
  | Call (f, args) ->
      F.fprintf fmt "%s(%a)" f
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp) args

let pp_decl fmt (FunDef (f, params, body)) =
  F.fprintf fmt "fun %s(%s) = %a" f (String.concat ", " params) pp body

let pp_prog fmt (Prog (decls, e)) =
  List.iter (fun d -> F.fprintf fmt "%a\n" pp_decl d) decls;
  F.fprintf fmt "%a" pp e

let rec pp_ast fmt e =
  match e with
  | Num n -> F.fprintf fmt "(Num %d)" n
  | Add (e1, e2) -> F.fprintf fmt "(Add, %a, %a)" pp_ast e1 pp_ast e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub, %a, %a)" pp_ast e1 pp_ast e2
  | Mul (e1, e2) -> F.fprintf fmt "(Mul, %a, %a)" pp_ast e1 pp_ast e2
  | Div (e1, e2) -> F.fprintf fmt "(Div, %a, %a)" pp_ast e1 pp_ast e2
  | Id x -> F.fprintf fmt "(Id %s)" x
  | Let (x, e1, e2) -> F.fprintf fmt "(Let %s, %a, %a)" x pp_ast e1 pp_ast e2
  | Call (f, args) ->
      F.fprintf fmt "(Call %s, [%a])" f
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "; ") pp_ast) args
