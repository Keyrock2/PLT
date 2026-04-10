open Ast
open Value
open Env
open Fenv

exception Division_by_zero of string
exception Wrong_number_of_arguments of int * int

let parse (s : string) : prog =
  Parser.parse s

(* 함수 정의 해석: fenv에 함수 정보를 추가합니다. *)
let interp_def (FunDef (f, params, body) : fdef) (fenv : Fenv.t) : Fenv.t =
  Fenv.add f params body fenv

(* 수식 해석: 핵심 인터프리터 *)
let rec interp_expr (fenv : Fenv.t) (env : Env.t) (e : expr) : Value.t =
  match e with
  | Num n -> NumV n
  | Id x -> Env.find x env
  | Add (e1, e2) ->
      let NumV n1 = interp_expr fenv env e1 in
      let NumV n2 = interp_expr fenv env e2 in
      NumV (n1 + n2)
  | Sub (e1, e2) ->
      let NumV n1 = interp_expr fenv env e1 in
      let NumV n2 = interp_expr fenv env e2 in
      NumV (n1 - n2)
  | Mul (e1, e2) ->
      let NumV n1 = interp_expr fenv env e1 in
      let NumV n2 = interp_expr fenv env e2 in
      NumV (n1 * n2)
  | Div (e1, e2) ->
      let NumV n1 = interp_expr fenv env e1 in
      let NumV n2 = interp_expr fenv env e2 in
      if n2 = 0 then
        (* Ast.pp를 이용해 수식을 문자열로 반환하여 예외 발생 *)
        let err_msg = Format.asprintf "%a" Ast.pp (Div (e1, e2)) in
        raise (Division_by_zero err_msg)
      else
        NumV (n1 / n2)
  | Let (x, e1, e2) ->
      let v1 = interp_expr fenv env e1 in
      let env' = Env.add x v1 env in
      interp_expr fenv env' e2
  | Call (f, args) ->
      (* Fenv에서 함수 탐색 *)
      let (params, body) = Fenv.find f fenv in
      let expected_arity = List.length params in
      let actual_arity = List.length args in

      (* 파라미터 개수(Arity) 불일치 시 예외 발생 *)
      if expected_arity <> actual_arity then
        raise (Wrong_number_of_arguments (expected_arity, actual_arity))
      else
        (* 인자(args)들을 현재 환경(env)에서 미리 계산 (Call-by-value) *)
        let arg_vals = List.map (interp_expr fenv env) args in

        (* 새로운 함수 호출을 위한 빈 환경(Env.empty) 생성 및 파라미터 맵핑 *)
        let call_env =
          List.fold_left2 (fun acc param arg_val -> Env.add param arg_val acc)
          Env.empty params arg_vals
        in

        (* 함수 몸체 계산 *)
        interp_expr fenv call_env body

(* 전체 프로그램 해석: 모든 함수 선언 후 마지막 expr 평가 *)
let interp_prog (Prog (decls, expr) : prog) : Value.t =
  let final_fenv = List.fold_left (fun acc def -> interp_def def acc) Fenv.empty decls in
  interp_expr final_fenv Env.empty expr

(* 최종 호출 함수 *)
let interp (s : string) : Value.t =
  interp_prog (parse s)
