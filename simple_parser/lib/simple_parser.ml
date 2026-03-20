module F = Format

(* ================================================ *)
(*          Function lex : string -> token          *)
(* ================================================ *)
type token =
  | IDENT of string   (* [a-z][a-zA-Z_'] *)
  | NUMBER of string  (* [1-9][0-9]* *)
  | KW_LET            (* 'let' *)
  | KW_IN             (* 'in' *)
  | OP_EQ             (* '=' *)
  | OP_PLUS           (* '+' *)
  | OP_MINUS          (* '-' *)
  | OP_MULT           (* '*' *)
  | OP_DIV            (* '/' *)

let compare_token t1 t2 =
  match t1, t2 with
  | IDENT s1, IDENT s2
  | NUMBER s1, NUMBER s2 -> String.equal s1 s2
  | KW_LET, KW_LET
  | KW_IN, KW_IN
  | OP_EQ, OP_EQ
  | OP_PLUS, OP_PLUS
  | OP_MINUS, OP_MINUS
  | OP_MULT, OP_MULT
  | OP_DIV, OP_DIV -> true
  | _ -> false

let pp_token fmt token =
  match token with
  | IDENT s -> F.fprintf fmt "%s" s
  | NUMBER s -> F.fprintf fmt "%s" s
  | KW_LET -> F.fprintf fmt "let"
  | KW_IN -> F.fprintf fmt "in"
  | OP_EQ -> F.fprintf fmt "="
  | OP_PLUS -> F.fprintf fmt "+"
  | OP_MINUS -> F.fprintf fmt "-"
  | OP_MULT -> F.fprintf fmt "*"
  | OP_DIV -> F.fprintf fmt "/"

let pp_tokens fmt tokens =
  F.fprintf fmt "[%a]"
    (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "; ") pp_token) tokens

type state =
  | Q0  (* start *)
  | Q1
  | Q2
  | Q3  (* KW_LET *)
  | Q4  (* IDENT *)
  | Q5
  | Q6  (* NUMBER *)
  | Q7
  | Q8  (* KW_IN *)

let transfer (c: char) (s: state) : state =
  match s, c with
  (* CASE : KW_LET *)
  | Q0, 'l' -> Q1
  | Q1, 'e' -> Q2
  | Q2, 't' -> Q3
  | Q3, 'a'..'z' | Q3, 'A'..'Z' | Q3, '_' | Q3, '\'' -> Q4

  (* CASE : KW_IN *)
  | Q0, 'i' -> Q7
  | Q7, 'n' -> Q8
  | Q8, 'a'..'z' | Q8, 'A'..'Z' | Q8, '_' | Q8, '\'' -> Q4

  (* CASE : NUMBER *)
  | Q0, '1'..'9' -> Q6
  | Q6, '0'..'9' -> Q6

  (* OTHERS *)
  | Q0, 'a'..'h' | Q0, 'j' | Q0, 'k' | Q0, 'm'..'z' -> Q4
  | Q4, 'a'..'z' | Q4, 'A'..'Z' | Q4, '_' | Q4, '\'' -> Q4

  | _, _ -> failwith "Transfer Error"

let rec get_state (clist: char list) (s: state) : state =
  match clist with
  | [] -> s
  | hd::tl -> transfer hd s |> get_state tl

let tokenize (st: state) (s: string) : token =
  match st with
  | Q3 -> KW_LET
  | Q4 -> IDENT s
  | Q6 -> NUMBER s
  | Q8 -> KW_IN
  | _ -> failwith "Tokenize Error"


let lex (s: string) : token =
  match s with
  | "=" -> OP_EQ
  | "+" -> OP_PLUS
  | "-" -> OP_MINUS
  | "*" -> OP_MULT
  | "/" -> OP_DIV
  | _ ->
      let clist : char list = String.to_seq s |> List.of_seq in
      try
        let st: state = get_state clist Q0 in
        tokenize st s
      with _ ->
        failwith (F.asprintf "Lexing error: %s" s)


(* ================================================ *)
(*         Function parse : string -> expr          *)
(* ================================================ *)
type expr =
  | LetIn of string * expr * expr   (* KW_LET IDENT OP_EQ e KW_IN e *)
  | Plus of expr * expr             (* e OP_PLUS e *)
  | Minus of expr * expr            (* e OP_MINUS e *)
  | Mult of expr * expr             (* e OP_MULT e *)
  | Div of expr * expr              (* e OP_DIV e *)
  | Num of string                   (* NUMBER *)
  | Id of string                    (* IDENT *)

let rec compare_expr e1 e2 =
  match e1, e2 with
  | LetIn (s1, ex1, ex2), LetIn (s2, ex3, ex4) ->
      String.equal s1 s2
      && compare_expr ex1 ex3
      && compare_expr ex2 ex4
  | Plus (e1, e2), Plus (e3, e4)
  | Minus (e1, e2), Minus (e3, e4)
  | Mult (e1, e2), Mult (e3, e4)
  | Div (e1, e2), Div (e3, e4) ->
      compare_expr e1 e3
      && compare_expr e2 e4
  | Num s1, Num s2
  | Id s1, Id s2 ->
      String.equal s1 s2
  | _ -> false

let rec pp_expr fmt e =
  match e with
  | LetIn (s, e1, e2) ->
      F.fprintf fmt "(let %s = %a in %a)" s pp_expr e1 pp_expr e2
  | Plus (e1, e2) ->
      F.fprintf fmt "(%a + %a)" pp_expr e1 pp_expr e2
  | Minus (e1, e2) ->
      F.fprintf fmt "(%a - %a)" pp_expr e1 pp_expr e2
  | Mult (e1, e2) ->
      F.fprintf fmt "(%a * %a)" pp_expr e1 pp_expr e2
  | Div (e1, e2) ->
      F.fprintf fmt "(%a / %a)" pp_expr e1 pp_expr e2
  | Num n ->
      F.fprintf fmt "%s" n
  | Id s ->
      F.fprintf fmt "%s" s

type parse_stack_elem =
  | T of token
  | E of expr

let pp_parse_stack_elem fmt elt =
  match elt with
  | T t -> F.fprintf fmt " $%a$ " pp_token t
  | E e -> F.fprintf fmt "(%a)" pp_expr e

module PStack = struct
  type t = parse_stack_elem list

  let empty = []

  let push (v: parse_stack_elem) (m: t) : t = v :: m
  let pop (m: t) : (parse_stack_elem * t) =
    match m with
    | [] -> failwith "stack is empty"
    | hd::tl -> hd, tl

  let fold_left = List.fold_left
  let length = List.length
  let is_empty = List.is_empty

  let pp fmt (s : t) =
    F.fprintf fmt "[%a]"
    (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt " :: ") pp_parse_stack_elem) s

end

let get_expr (elem : parse_stack_elem) : expr =
  match elem with
  | E exp -> exp
  | T _ -> failwith "Not a Expr"

let token_to_elem (t : token) : parse_stack_elem =
  match t with
  | IDENT s -> E (Id s)
  | NUMBER s -> E (Num s)
  | KW_LET
  | KW_IN
  | OP_EQ
  | OP_PLUS
  | OP_MINUS
  | OP_MULT
  | OP_DIV -> T t

let shift (elem : parse_stack_elem) (stack : PStack.t) : PStack.t =
  PStack.push elem stack

let transfer
  (input : string)                  (* Input string *)
  (s : PStack.t)                    (* Stack *)
  (tokens : parse_stack_elem list)  (* Input buffer *)
  : PStack.t * parse_stack_elem list =
      match tokens with
      | [] ->
          (* If input buffer is empty && #Stack_elements > 1 => reduce *)
          begin
            match s with
            | E e2 :: T KW_IN :: E e1 :: T OP_EQ :: E (Id id) :: T KW_LET :: s->
                (* Reduce the top 5 stack elements to a LetIn expression *)
                let s' = PStack.push (E (LetIn (id, e1, e2))) s in
                s', tokens

            | E e2 :: T OP_PLUS :: E e1 :: s ->
                (* Reduce the top 3 stack elements to a Plus expression *)
                let s' = PStack.push (E (Plus (e1, e2))) s in
                s', tokens

            | E e2 :: T OP_MINUS :: E e1 :: s ->
                (* Reduce the top 3 stack elements to a Minus expression *)
                let s' = PStack.push (E (Minus (e1, e2))) s in
                s', tokens

            | E e2 :: T OP_MULT :: E e1 :: s ->
                let s' = PStack.push (E (Mult (e1, e2))) s in s', tokens
            | E e2 :: T OP_DIV :: E e1 :: s ->
                let s' = PStack.push (E (Div (e1, e2))) s in s', tokens
            | _ ->
                (* Fail: Input buffer is empty, but there is not exactly one expr on the stack. *)
                failwith (F.asprintf "Parsing error: %s" input)
          end

      | token::others ->
          (* If input buffer is not empty => reduce or shift *)
          begin
            match token, s with
            (* KW_IN forces reduction of all arithmetic operators *)
            | T KW_IN, (E e2 :: T OP_MULT :: E e1 :: s) ->
                let s' = PStack.push (E (Mult (e1, e2))) s in s', tokens
            | T KW_IN, (E e2 :: T OP_DIV :: E e1 :: s) ->
                let s' = PStack.push (E (Div (e1, e2))) s in s', tokens
            | T KW_IN, (E e2 :: T OP_PLUS :: E e1 :: s) ->
                (* Reduce the top 3 stack elements to a Plus expression *)
                let s' = PStack.push (E (Plus (e1, e2))) s in
                s', tokens

            | T KW_IN, (E e2 :: T OP_MINUS :: E e1 :: s) ->
                (* Reduce the top 3 stack elements to a Minus expression *)
                let s' = PStack.push (E (Minus (e1, e2))) s in
                s', tokens

            (* When lookahead is + or -, reduce any arithmetic operator on stack first (left-associative & precedence) *)
            | T OP_PLUS, (E e2 :: T OP_MULT :: E e1 :: s)
            | T OP_MINUS, (E e2 :: T OP_MULT :: E e1 :: s) ->
                let s' = PStack.push (E (Mult (e1, e2))) s in s', tokens
            | T OP_PLUS, (E e2 :: T OP_DIV :: E e1 :: s)
            | T OP_MINUS, (E e2 :: T OP_DIV :: E e1 :: s) ->
                let s' = PStack.push (E (Div (e1, e2))) s in s', tokens
            | T OP_PLUS, (E e2 :: T OP_PLUS :: E e1 :: s)
            | T OP_MINUS, (E e2 :: T OP_PLUS :: E e1 :: s) ->
                (* Reduce, since + is left-associative *)
                let s' = PStack.push (E (Plus (e1, e2))) s in
                s', tokens

            | T OP_PLUS, (E e2 :: T OP_MINUS :: E e1 :: s)
            | T OP_MINUS, (E e2 :: T OP_MINUS :: E e1 :: s) ->
                (* Reduce, since - is left-associative *)
                let s' = PStack.push (E (Minus (e1, e2))) s in
                s', tokens

            (* When lookahead is * or /, reduce ONLY if stack top is * or / (left-associative) *)
            | T OP_MULT, (E e2 :: T OP_MULT :: E e1 :: s)
            | T OP_DIV, (E e2 :: T OP_MULT :: E e1 :: s) ->
                let s' = PStack.push (E (Mult (e1, e2))) s in s', tokens
            | T OP_MULT, (E e2 :: T OP_DIV :: E e1 :: s)
            | T OP_DIV, (E e2 :: T OP_DIV :: E e1 :: s) ->
                let s' = PStack.push (E (Div (e1, e2))) s in s', tokens

            | _ ->
                (* Other Cases : shift *)
                (shift token s), others
          end


let rec loop
  (input : string) (s : PStack.t) (tokens : parse_stack_elem list) : expr =
    match s, tokens with
    | [E e], [] -> e
    | _ ->
        let s', tokens' = transfer input s tokens in
        loop input s' tokens'


let parse (s : string) : expr =
  let word_list : string list = String.split_on_char ' ' s in
  let token_list : token list =
    List.map (fun word -> lex word) word_list
  in
  let elem_list : parse_stack_elem list =
    List.map (fun token -> token_to_elem token) token_list
  in
  loop s PStack.empty elem_list
