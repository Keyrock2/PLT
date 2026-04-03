open Vae
open Value
open Env
open Alcotest

let value_testable = testable Value.pp Value.compare

(* 1. 기본 변수 할당 *)
let test_let1 () = check value_testable "Basic Let" (NumV 1) (interp "let x = 1 in x")

(* 2. 변수 연산 *)
let test_let2 () = check value_testable "Let Add" (NumV 3) (interp "let x = 1 in x + 2")

(* 3. 변수 여러 개 사용 *)
let test_let3 () = check value_testable "Multi Var" (NumV 2) (interp "let x = 5 in let y = 3 in x - y")

(* 4. 변수 가림(Shadowing) 현상 검증 *)
let test_let4 () = check value_testable "Shadowing" (NumV 2) (interp "let x = 1 in let x = 2 in x")

(* 5. 깊은 가림 현상 (Deep Shadowing) *)
let test_let5 () = check value_testable "Deep Shadowing" (NumV 12) (interp "let x = let x = 3 in x + x in x + x")

(* 6. 독립된 Scope 유지 검증 *)
let test_let6 () = check value_testable "Independent Scopes" (NumV 5) (interp "(let x = 2 in x) + (let x = 3 in x)")

(* 7. 복합 연산 내의 변수 평가 *)
let test_let7 () = check value_testable "Complex Ops" (NumV 12) (interp "let a = 10 in let b = 2 in (a * b) / (a - b) + 10")

(* 8. 복잡한 괄호 구조 내의 식별자 *)
let test_let8 () = check value_testable "Nested Parens" (NumV 0) (interp "let x = 10 in (x - 10) * x")

(* 9. Division_by_zero Exception 테스트 *)
let test_exn1_div_zero () =
  check_raises "Div by zero msg" (Vae.Division_by_zero "1 / x")
  (fun () -> ignore (interp "let x = 0 in 1 / x"))

(* 10. Unbound_identifier Exception 테스트 *)
let test_exn2_unbound () =
  check_raises "Unbound identifier" (Env.Unbound_identifier "y")
  (fun () -> ignore (interp "let x = 0 in 1 / y"))


let () =
  run "VAE Interpreter"
    [
      ("Let & Id Tests", [
        test_case "Test 1: Basic Let" `Quick test_let1;
        test_case "Test 2: Let Add" `Quick test_let2;
        test_case "Test 3: Multi Var" `Quick test_let3;
        test_case "Test 4: Shadowing" `Quick test_let4;
        test_case "Test 5: Deep Shadowing" `Quick test_let5;
        test_case "Test 6: Independent Scopes" `Quick test_let6;
        test_case "Test 7: Complex Ops" `Quick test_let7;
        test_case "Test 8: Nested Parens" `Quick test_let8;
        test_case "Test 9: Div by zero EXN" `Quick test_exn1_div_zero;
        test_case "Test 10: Unbound EXN" `Quick test_exn2_unbound;
      ]);
    ]
