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


(* =========================================== *)
(*                 Test parse                  *)
(* =========================================== *)
let test_parse1 () =
  let s = "let x = 1 + 2 in 4 + x" in
  (check expr) "test_parse1"
  (LetIn ("x", Plus (Num "1", Num "2"), Plus (Num "4", Id "x")))
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
          [ test_case "test1" `Quick test_lex1; ]);
      ("Parse Test",
          [ test_case "test1" `Quick test_parse1; ]);

      ("Lex Test (w/ string compare)",
          [ test_case "test2" `Quick test_lex_string1; ]);
      ("Parse Test (w/ string compare)",
          [ test_case "test2" `Quick test_parse_string1; ]);
    ]
