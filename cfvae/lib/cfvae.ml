open Ast
open Value
open Env

type error_msg =
  | Not_a_number of string
  | Not_a_function of string
  | Not_a_bool of string

exception Type_error of error_msg
exception Division_by_zero of string

let parse (s : string) : expr =
  Parser.parse s

let rec interp_expr (env : Value.t Env.t) (e : expr) : Value.t =
  let get_num expr_node =
    match interp_expr env expr_node with
    | NumV n -> n
    | _ ->
        let err_str = Format.asprintf "%a" Ast.pp expr_node in
        raise (Type_error (Not_a_number err_str))
  in

  match e with
  | Num n -> NumV n
  | Bool b -> BoolV b
  | Id x -> Env.find x env
  | Add (e1, e2) -> NumV (get_num e1 + get_num e2)
  | Sub (e1, e2) -> NumV (get_num e1 - get_num e2)
  | Mul (e1, e2) -> NumV (get_num e1 * get_num e2)
  | Div (e1, e2) ->
      let n1 = get_num e1 in
      let n2 = get_num e2 in
      if n2 = 0 then
        let err_str = Format.asprintf "%a" Ast.pp e in
        raise (Division_by_zero err_str)
      else NumV (n1 / n2)
  | Lt (e1, e2) ->
      let n1 = get_num e1 in
      let n2 = get_num e2 in
      BoolV (n1 < n2)
  | Cond (e1, e2, e3) ->
      (match interp_expr env e1 with
       | BoolV true -> interp_expr env e2
       | BoolV false -> interp_expr env e3
       | _ ->
           let err_str = Format.asprintf "%a" Ast.pp e1 in
           raise (Type_error (Not_a_bool err_str)))
  | Let (x, e1, e2) ->
      let v1 = interp_expr env e1 in
      interp_expr (Env.add x v1 env) e2
  | Lambda (x, body) ->
      ClosureV (x, body, env)
  | App (e1, e2) ->
      let v1 = interp_expr env e1 in
      (match v1 with
       | ClosureV (x, body, closure_env) ->
           let v2 = interp_expr env e2 in
           interp_expr (Env.add x v2 closure_env) body
       | _ ->
           let err_str = Format.asprintf "%a" Ast.pp e1 in
           raise (Type_error (Not_a_function err_str)))

let interp (s : string) : Value.t =
  interp_expr Env.empty (parse s)
