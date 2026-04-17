open Ast
open Value
open Env

type type_error =
  | Not_a_number of string
  | Not_a_function of string

exception Type_error of type_error
exception Division_by_zero of string

let parse (s : string) : expr =
  Parser.parse s

let rec interp_expr (env : Value.t Env.t) (e : expr) : Value.t =
  (* 사칙연산 수행 전, 피연산자가 숫자인지(Not_a_number) 검증하는 헬퍼 함수 *)
  let get_num expr_node =
    match interp_expr env expr_node with
    | NumV n -> n
    | ClosureV _ ->
        (* 함수(Closure)가 연산에 들어왔을 경우 해당 AST를 문자열로 변환하여 예외 발생 *)
        let err_str = Format.asprintf "%a" Ast.pp expr_node in
        raise (Type_error (Not_a_number err_str))
  in

  match e with
  | Num n -> NumV n
  | Id x -> Env.find x env
  | Add (e1, e2) -> NumV (get_num e1 + get_num e2)
  | Sub (e1, e2) -> NumV (get_num e1 - get_num e2)
  | Mul (e1, e2) -> NumV (get_num e1 * get_num e2)
  | Div (e1, e2) ->
      let n1 = get_num e1 in
      let n2 = get_num e2 in
      if n2 = 0 then
        (* 수식(e) 자체를 출력하여 Division_by_zero 예외 발생 *)
        let err_str = Format.asprintf "%a" Ast.pp e in
        raise (Division_by_zero err_str)
      else
        NumV (n1 / n2)
  | Let (x, e1, e2) ->
      let v1 = interp_expr env e1 in
      interp_expr (Env.add x v1 env) e2
  | Lambda (x, body) ->
      (* FVAE의 핵심코드로 함수 자체를 캡처하여 값(ClosureV)으로 반환 *)
      ClosureV (x, body, env)
  | App (e1, e2) ->
      (* 함수 적용 (Call) 로직 *)
      let v1 = interp_expr env e1 in
      match v1 with
      | ClosureV (x, body, closure_env) ->
          let v2 = interp_expr env e2 in
          (* 함수가 정의될 당시의 메모리(closure_env)에 파라미터를 추가해 실행 (Lexical Scope) *)
          interp_expr (Env.add x v2 closure_env) body
      | NumV _ ->
          (* 첫 번째 expr이 숫자로 계산된 경우 Not_a_function 예외 발생 *)
          let err_str = Format.asprintf "%a" Ast.pp e1 in
          raise (Type_error (Not_a_function err_str))

let interp (s : string) : Value.t =
  interp_expr Env.empty (parse s)
