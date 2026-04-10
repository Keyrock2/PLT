open F1vae
open Value
open Env
open Fenv
open Alcotest

let value_testable = testable Value.pp Value.compare

(* 일반 함수 호출 테스트 *)
let test_call1 () =
  check value_testable "Call 1" (NumV 3) (interp "fun f(x) = x + 1;; f(2)")

(* 다중 파라미터 호출 *)
let test_call2 () =
  check value_testable "Multi Params" (NumV 5) (interp "fun add(a, b) = a + b;; add(2, 3)")

(* 파라미터 없는 함수 호출 *)
let test_call3 () =
  check value_testable "No Params" (NumV 10) (interp "fun f() = 10;; f()")

(* 함수 중첩 호출 테스트 *)
let test_call4 () =
  check value_testable "Nested Calls" (NumV 7)
  (interp "fun f(x) = x + 1;; fun g(y) = y * 2;; f(g(3))")

(* 동일 이름의 함수 선언 시 덮어쓰기 *)
let test_call5 () =
  check value_testable "Function Shadowing" (NumV 20)
  (interp "fun f(x) = x + 1;; fun f(x) = x * 10;; f(2)")

(* 변수 Shadowing과 함수 호출 혼합 *)
let test_call6 () =
  check value_testable "Variable Shadowing" (NumV 5)
  (interp "fun f(x) = let x = 5 in x;; f(10)")

(* [Exception] Division_by_zero 테스트 *)
let test_exn1 () =
  check_raises "Division by Zero" (F1vae.Division_by_zero "x / 0")
  (fun () -> ignore (interp "fun f(x) = x / 0;; f(1)"))

(* [Exception] Unknown_function 테스트 (정의되지 않은 함수) *)
let test_exn2 () =
  check_raises "Unknown Function" (Fenv.Unknown_function "g")
  (fun () -> ignore (interp "fun f(x) = x + 1;; g(1)"))

(* [Exception] Wrong_number_of_arguments 테스트 (Arity 불일치, 기대 2 실제 1) *)
let test_exn3 () =
  check_raises "Wrong Arity" (F1vae.Wrong_number_of_arguments (2, 1))
  (fun () -> ignore (interp "fun f(x, y) = x + y;; f(1)"))

(* [Exception] Unbound_identifier 테스트 (Lexical Scope 검증) *)
(* 함수 바깥의 let 변수 y는 함수 몸체에서 접근할 수 없음 *)
let test_exn4 () =
  check_raises "Lexical Scope Bound" (Env.Unbound_identifier "y")
  (fun () -> ignore (interp "fun f(x) = x + y;; let y = 10 in f(5)"))

let () =
  run "F1VAE Interpreter"
    [
      ("interp", [
        test_case "Simple Call" `Quick test_call1;
        test_case "Multi Params" `Quick test_call2;
        test_case "No Params" `Quick test_call3;
        test_case "Nested Calls" `Quick test_call4;
        test_case "Function Shadow" `Quick test_call5;
        test_case "Variable Shadow" `Quick test_call6;
        test_case "Div by zero EXN" `Quick test_exn1;
        test_case "Unknown func EXN" `Quick test_exn2;
        test_case "Wrong arity EXN" `Quick test_exn3;
        test_case "Unbound id EXN" `Quick test_exn4;
      ]);
    ]
