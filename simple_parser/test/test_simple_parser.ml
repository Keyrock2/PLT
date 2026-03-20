open Simple_parser
open Alcotest

(* val check : testable -> msg -> expected -> actual -> testcase *)
(* =========================================== *)
(*       Define testable for token & expr      *)
(* =========================================== *)
let token = testable pp_token compare_token
let expr = testable pp_expr compare_expr


(* =========================================== *)
(*                  Test lex                   *)
(* =========================================== *)
let test_lex1 () =
  let s = "1 + 2 + 3" in
  (check (list token)) "test_lex1"
  [NUMBER "1"; OP_PLUS; NUMBER "2"; OP_PLUS; NUMBER "3"]
  (String.split_on_char ' ' s |> List.map lex)

let test_lex3 () =
  let s = "4 * 5 / 2" in
  (check (list token)) "test_lex3"
  [NUMBER "4"; OP_MULT; NUMBER "5"; OP_DIV; NUMBER "2"]
  (String.split_on_char ' ' s |> List.map lex)

let test_lex4 () =
  let s = "let a = 10 * 2 in a" in
  (check (list token)) "test_lex4"
  [KW_LET; IDENT "a"; OP_EQ; NUMBER "10"; OP_MULT; NUMBER "2"; KW_IN; IDENT "a"]
  (String.split_on_char ' ' s |> List.map lex)

let test_lex5 () =
  let s = "x + 2 * 3 - y / 4" in
  (check (list token)) "test_lex5"
  [IDENT "x"; OP_PLUS; NUMBER "2"; OP_MULT; NUMBER "3"; OP_MINUS; IDENT "y"; OP_DIV; NUMBER "4"]
  (String.split_on_char ' ' s |> List.map lex)


(* =========================================== *)
(*                 Test parse                  *)
(* =========================================== *)
let test_parse1 () =
  let s = "let x = 1 + 2 in 4 + x" in
  (check expr) "test_parse1"
  (LetIn ("x", Plus (Num "1", Num "2"), Plus (Num "4", Id "x")))
  (parse s)

let test_parse3 () =
  let s = "1 + 2 * 3" in
  (check expr) "test_parse3"
  (Plus (Num "1", Mult (Num "2", Num "3")))
  (parse s)

let test_parse4 () =
  let s = "4 * 2 / 2" in
  (check expr) "test_parse4"
  (Div (Mult (Num "4", Num "2"), Num "2"))
  (parse s)

let test_parse5 () =
  let s = "1 + 2 + 3 * 4 - 5" in
  (check expr) "test_parse5"
  (Minus (Plus (Plus (Num "1", Num "2"), Mult (Num "3", Num "4")), Num "5"))
  (parse s)


(* =========================================== *)
(*          Test lex w/ string compare         *)
(* =========================================== *)
let test_lex_string1 () =
  let s = "1 + 2 + 3" in
  let res = String.split_on_char ' ' s |> List.map lex in
  (check string) "test_lex_string1"
  "[1; +; 2; +; 3]"
  (F.asprintf "%a" pp_tokens res)


(* =========================================== *)
(*         Test parse w/ string compare        *)
(* =========================================== *)
let test_parse_string1 () =
  let s = "let x = 1 + 2 in 4 + x" in
  let res = parse s in
  (check string) "test_parse_string1"
  "(let x = (1 + 2) in (4 + x))"
  (F.asprintf "%a" pp_expr res)


let () =
  run "simple_parser test"
    [
      ("Lex Test",
          [
            test_case "test1" `Quick test_lex1;
            test_case "test3" `Quick test_lex3;
            test_case "test4" `Quick test_lex4;
            test_case "test5" `Quick test_lex5;
          ]);
      ("Parse Test",
          [
            test_case "test1" `Quick test_parse1;
            test_case "test3" `Quick test_parse3;
            test_case "test4" `Quick test_parse4;
            test_case "test5" `Quick test_parse5;
          ]);

      ("Lex Test (w/ string compare)",
          [ test_case "test2" `Quick test_lex_string1; ]);
      ("Parse Test (w/ string compare)",
          [ test_case "test2" `Quick test_parse_string1; ]);
    ]
