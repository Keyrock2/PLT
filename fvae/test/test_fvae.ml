open Fvae
open Value
open Env
open Alcotest

(* Value 모듈에 직접 구현한 pp를 사용해 testable 생성 *)
let value_testable = testable Value.pp Value.compare

(* 기본 Lambda 생성 *)
let test_normal1 () =
  check value_testable "Basic Lambda"
  (ClosureV ("x", Ast.Add (Ast.Id "x", Ast.Num 1), Env.empty))
  (interp "(fun x -> x + 1)")

(* 커링을 이용한 다중 함수 호출 *)
let test_normal2 () =
  check value_testable "Curried Call"
  (NumV 3) (interp "let f x y = x + y in f 1 2")

(* 스코프가 다른 람다 호출 *)
let test_normal3 () =
  check value_testable "Scope Call"
  (NumV 4) (interp "let x = 1 in let f x = x + 1 in f 3")

(* 정적 스코프(Lexical Scoping) 동작 확인 *)
let test_normal4 () =
  check value_testable "Lexical Scope"
  (NumV 10) (interp "let x = 10 in let f = (fun y -> x) in let x = 20 in f 0")

(* 함수 자체를 인자로 넘기는 고차 함수 (First-class function) *)
let test_normal5 () =
  check value_testable "Higher Order"
  (NumV 5) (interp "let f = (fun g -> g 2) in let add3 = (fun x -> x + 3) in f add3")

(* 일반 사칙연산 *)
let test_normal6 () =
  check value_testable "Basic Math"
  (NumV 8) (interp "let x = 2 in (x * 3) + 2")


(* 0으로 나누는 경우 : Division_by_zero *)
let test_exn1 () =
  check_raises "Div by zero EXN"
  (Fvae.Division_by_zero "1 / 0")
  (fun () -> ignore (interp "1 / 0"))

(* 사칙연산에서 expr이 함수로 계산되는 경우 : Not_a_number *)
let test_exn2 () =
  check_raises "Not a number EXN"
  (Fvae.Type_error (Fvae.Not_a_number "(fun x -> x)"))
  (fun () -> ignore (interp "1 + (fun x -> x)"))

(* App의 첫 번째 expr이 숫자로 계산되는 경우 : Not_a_function *)
let test_exn3 () =
  check_raises "Not a function EXN"
  (Fvae.Type_error (Fvae.Not_a_function "1"))
  (fun () -> ignore (interp "1 2"))

(* 정의되지 않은 변수 접근 : Unbound_identifier *)
let test_exn4 () =
  check_raises "Unbound ID EXN"
  (Env.Unbound_identifier "y")
  (fun () -> ignore (interp "(fun x -> y) 1"))


let () =
  run "FVAE Interpreter Tests"
    [
      ("Interp Tests", [
        test_case "T1: Basic Lambda" `Quick test_normal1;
        test_case "T2: Curried Call" `Quick test_normal2;
        test_case "T3: Scope Call" `Quick test_normal3;
        test_case "T4: Lexical Scope" `Quick test_normal4;
        test_case "T5: Higher Order" `Quick test_normal5;
        test_case "T6: Basic Math" `Quick test_normal6;
        test_case "T7: Div by zero" `Quick test_exn1;
        test_case "T8: Not a number" `Quick test_exn2;
        test_case "T9: Not a function" `Quick test_exn3;
        test_case "T10: Unbound ID" `Quick test_exn4;
      ]);
    ]
