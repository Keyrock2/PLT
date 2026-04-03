open Ast
open Value
open Env

(* VAE에서 요구하는 사용자 정의 예외 *)
exception Division_by_zero of string

(* AST와 Env를 받아 실제 값을 평가하는 핵심 보조 함수 *)
let rec eval (env : Env.t) (e : expr) : Value.t =
  match e with
  | Num n -> NumV n
  | Id x -> Env.find x env
  | Add (e1, e2) ->
      let NumV n1 = eval env e1 in
      let NumV n2 = eval env e2 in
      NumV (n1 + n2)
  | Sub (e1, e2) ->
      let NumV n1 = eval env e1 in
      let NumV n2 = eval env e2 in
      NumV (n1 - n2)
  | Mul (e1, e2) ->
      let NumV n1 = eval env e1 in
      let NumV n2 = eval env e2 in
      NumV (n1 * n2)
  | Div (e1, e2) ->
      let NumV n1 = eval env e1 in
      let NumV n2 = eval env e2 in
      if n2 = 0 then
        let err_msg = Format.asprintf "%a" Ast.pp (Div (e1, e2)) in
        raise (Division_by_zero err_msg)
      else
        NumV (n1 / n2)
  | Let (x, e1, e2) ->
      (* 1. e1을 현재 env에서 평가하여 v1을 얻음 *)
      let v1 = eval env e1 in
      (* 2. env에 변수 x와 값 v1을 추가하여 새로운 env' 생성 *)
      let env' = Env.add x v1 env in
      (* 3. 확장된 env'에서 e2 평가 *)
      eval env' e2

(* 입력 문자열을 파싱하고 빈 메모리에서 평가를 시작하는 메인 함수 *)
let interp (s : string) : Value.t =
  let ast = Parser.parse s in
  eval Env.empty ast
