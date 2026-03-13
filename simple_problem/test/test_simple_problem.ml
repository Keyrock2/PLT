open Simple_problem

let test_fib_5 () =
  Alcotest.(check int) "fib 5 is 5" 5 (fib 5)

let test_fib_neg () =
  Alcotest.(check int) "fib -1 is -1" (-1) (fib (-1))

let test_rev_nil () =
  Alcotest.(check (list int)) "rev Nil" [] (rev [])

let test_rev_normal () =
  Alcotest.(check (list int)) "rev 1 2 3 4" [4; 3; 2; 1] (rev [1; 2; 3; 4])

let test_nth_normal () =
  Alcotest.(check int) "nth 1 2 3 1" 2 (nth [1; 2; 3; 4] 1)

let test_nth_fail () =
  try
    let _ = nth [] 0 in
    Alcotest.fail "expected Failure, but no exception was raised"
  with
  | Failure msg -> Alcotest.(check string) "exception msg" "Such an element does not exist" msg

let test_sort_asc () =
  Alcotest.(check (list int)) "incremental" [1; 2; 3; 4; 5] (sort (fun x y -> x < y) [5; 4; 3; 2; 1])

let test_sort_desc () =
  Alcotest.(check (list int)) "descending" [5; 4; 3; 2; 1] (sort (fun x y -> x > y) [1; 2; 3; 4; 5])

let test_check_child_sum_true () =
  let valid_tree = N (10, (N (8, (N (3, Nil, Nil)), (N (5, Nil, Nil)))), (N (2, (N (2, Nil, Nil)), Nil))) in
  Alcotest.(check bool) "true case" true (check_child_sum valid_tree)

let test_check_child_sum_false () =
  let invalid_tree = N (3, (N (3, (N (1, Nil, Nil)), (N (1, Nil, Nil)))), (N (0, Nil, (N (1, Nil, Nil))))) in
  Alcotest.(check bool) "false case" false (check_child_sum invalid_tree)

let () =
  let open Alcotest in
  run "Simple Problem"
    [
      ("fib", [ test_case "fib 5." `Quick test_fib_5; test_case "fib -1." `Quick test_fib_neg ]);
      ("rev", [ test_case "rev Nil." `Quick test_rev_nil; test_case "rev 3 2 1." `Quick test_rev_normal ]);
      ("nth", [ test_case "nth 1 2 3 1." `Quick test_nth_normal; test_case "nth Nil 0." `Quick test_nth_fail ]);
      ("sort", [ test_case "incremental." `Quick test_sort_asc; test_case "descending." `Quick test_sort_desc ]);
      ("check_child_sum", [ test_case "true case." `Quick test_check_child_sum_true; test_case "false case." `Quick test_check_child_sum_false ]);
    ]
