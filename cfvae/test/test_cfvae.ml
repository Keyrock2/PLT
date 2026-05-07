open Cfvae
open Value
open Env
open Alcotest

let value_testable = testable Value.pp Value.compare

(* 기본 BoolV 동작 *)
let test_bool () =
  check value_testable "Basic Bool" (BoolV true) (interp "true")

(* Lt 연산 동작 (참) *)
let test_lt_true () =
  check value_testable "Less Than True" (BoolV true) (interp "1 < 3")

(* If 조건문 (Cond) 참/거짓 동작 *)
let test_cond () =
  check value_testable "If Condition" (NumV 7) (interp "if (1 < 3) then 7 else 10")

(* 논리 연산자 Syntactic sugar (파서가 Cond로 풀어줌) *)
let test_sugar_logic () =
  check value_testable "Logic Sugar" (BoolV false) (interp "true && false")

(* 비교 연산자 Syntactic sugar (파서가 Lt와 Cond로 풀어줌) *)
let test_sugar_cmp () =
  check value_testable "Compare Sugar" (BoolV true) (interp "5 >= 5")

(* 재귀/조합 연산이 섞인 고난도 식 *)
let test_complex () =
  check value_testable "Complex CFVAE"
  (NumV 100) (interp "let x = 10 in if (x <= 10) then (x * 10) else 0")


(* 0으로 나누기 (Division_by_zero) *)
let test_exn1 () =
  check_raises "Div by zero EXN"
  (Cfvae.Division_by_zero "1 / 0")
  (fun () -> ignore (interp "1 / 0"))

(* Cond에서 조건식이 Bool이 아닐 때 (Not_a_bool) *)
let test_exn2 () =
  check_raises "Not a bool EXN"
  (Cfvae.Type_error (Cfvae.Not_a_bool "1"))
  (fun () -> ignore (interp "if 1 then 2 else 3"))

(* 사칙연산/비교 연산에서 숫자가 아닐 때 (Not_a_number) *)
let test_exn3 () =
  check_raises "Not a number EXN"
  (Cfvae.Type_error (Cfvae.Not_a_number "true"))
  (fun () -> ignore (interp "1 + true"))

(* 정의되지 않은 변수 접근 (Unbound_identifier) *)
let test_exn4 () =
  check_raises "Unbound ID EXN"
  (Env.Unbound_identifier "unknown_var")
  (fun () -> ignore (interp "unknown_var"))

let () =
  run "CFVAE Interpreter Tests"
    [
      ("Normal & Sugar Tests", [
        test_case "T1: Basic Bool" `Quick test_bool;
        test_case "T2: Lt True" `Quick test_lt_true;
        test_case "T3: If Condition" `Quick test_cond;
        test_case "T4: Logic Sugar" `Quick test_sugar_logic;
        test_case "T5: Compare Sugar" `Quick test_sugar_cmp;
        test_case "T6: Complex CFVAE" `Quick test_complex;
      ]);
      ("Exception Tests", [
        test_case "T7: Div by zero" `Quick test_exn1;
        test_case "T8: Not a bool" `Quick test_exn2;
        test_case "T9: Not a number" `Quick test_exn3;
        test_case "T10: Unbound ID" `Quick test_exn4;
      ]);
    ]
