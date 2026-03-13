  type node = N of int * node * node | Nil

  let rec fib n =
    if n < 0 then -1
  else match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib (n - 1) + fib (n - 2)

  let rec rev lst =
    let rec aux acc l =
      match l with
      | [] -> acc
      | h :: t -> aux (h :: acc) t
    in aux [] lst

  let rec nth lst n =
    match lst, n with
    | [], _ -> failwith "Such an element does not exist"
    | h :: _, 0 -> h
    | _ :: t, idx -> nth t (idx - 1)

  let rec sort f lst =
    match lst with
    | [] -> []
    | h :: t ->
        let rec insert x l =
          match l with
          | [] -> [x]
          | y :: ys -> if f x y then x :: y :: ys else y :: insert x ys
        in
        insert h (sort f t)

  let rec check_child_sum n = match n with
    | Nil -> true
    | N (_, Nil, Nil) -> true
    | N (v, l, r) ->
      let child_val node = match node with
        | Nil -> 0
        | N (cv, _, _) -> cv
      in (v = child_val l + child_val r) && check_child_sum l && check_child_sum r
