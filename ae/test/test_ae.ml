open Ae
open Value
open Alcotest

let value_testable = testable Value.pp Value.compare

(* 1. Add (덧셈) 테스트 4개 *)
let test_add1 () = check value_testable "add 1" (NumV 5) (interp "2 + 3")
let test_add2 () = check value_testable "add 2" (NumV 0) (interp "-2 + 2")
let test_add3 () = check value_testable "add 3" (NumV 100) (interp "50 + 50")
let test_add4 () = check value_testable "add 4" (NumV 200) (interp "100 + 100")

(* 2. Sub (뺄셈) 테스트 4개 *)
let test_sub1 () = check value_testable "sub 1" (NumV 1) (interp "3 - 2")
let test_sub2 () = check value_testable "sub 2" (NumV (-5)) (interp "0 - 5")
let test_sub3 () = check value_testable "sub 3" (NumV 10) (interp "20 - 10")
let test_sub4 () = check value_testable "sub 4" (NumV 0) (interp "10 - 10")

(* 3. Mul (곱셈) 테스트 4개 *)
let test_mul1 () = check value_testable "mul 1" (NumV 6) (interp "2 * 3")
let test_mul2 () = check value_testable "mul 2" (NumV 0) (interp "0 * 100")
let test_mul3 () = check value_testable "mul 3" (NumV (-10)) (interp "2 * -5")
let test_mul4 () = check value_testable "mul 4" (NumV 200) (interp "2 * 100")

(* 4. Div (나눗셈) 테스트 4개 (0으로 나누기 예외 포함) *)
let test_div1 () = check value_testable "div 1" (NumV 2) (interp "4 / 2")
let test_div2 () = check value_testable "div 2" (NumV 11) (interp "121 / 11")
let test_div3 () = check value_testable "div 3" (NumV (-2)) (interp "-10 / 5")
let test_div4_zero () =
  check_raises "div by zero" Division_by_zero (fun () -> ignore (interp "10 / 0"))

let () =
  run "AE Interpreter"
    [
      ("interp", [
        test_case "Add test 1" `Quick test_add1;
        test_case "Add test 2" `Quick test_add2;
        test_case "Add test 3" `Quick test_add3;
        test_case "Add test 4" `Quick test_add4;
        test_case "Sub test 1" `Quick test_sub1;
        test_case "Sub test 2" `Quick test_sub2;
        test_case "Sub test 3" `Quick test_sub3;
        test_case "Sub test 4" `Quick test_sub4;
        test_case "Mul test 1" `Quick test_mul1;
        test_case "Mul test 2" `Quick test_mul2;
        test_case "Mul test 3" `Quick test_mul3;
        test_case "Mul test 4" `Quick test_mul4;
        test_case "Div test 1" `Quick test_div1;
        test_case "Div test 2" `Quick test_div2;
        test_case "Div test 3" `Quick test_div3;
        test_case "Div test 4 (Zero)" `Quick test_div4_zero;
      ]);
    ]
