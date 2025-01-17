type t =
  | Int of int
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Mod of t * t
  | Min of t * t
  | Max of t * t

let op_of_char = function
  | '+' -> fun a b -> Add (a, b)
  | '-' -> fun a b -> Sub (a, b)
  | '*' -> fun a b -> Mul (a, b)
  | '/' -> fun a b -> Div (a, b)
  | '%' -> fun a b -> Mod (a, b)
  | 'v' -> fun a b -> Min (a, b)
  | 'A' -> fun a b -> Max (a, b)
  | _ -> failwith "unexpected character"

let read_polish_string str =
  let rec aux pos =
    if pos = String.length str then failwith "wrong format of string"
    else
      let c = str.[pos] in
      if c = ' ' then aux (pos + 1)
      else if
        c = '+' || c = '*' || c = '-' || c = '/' || c = '%' || c = 'u'
        || c = 'v' || c = 'A'
      then
        let res1, pos1 = aux (pos + 1) in
        let res2, pos2 = aux (pos1 + 1) in
        ((op_of_char c) res1 res2, pos2)
      else
        let new_pos = read_number pos in
        let num =
          try int_of_string (String.sub str pos (new_pos - pos))
          with _ ->
            failwith (string_of_int pos ^ " , " ^ string_of_int new_pos)
        in
        (Int num, new_pos)
  and read_number pos =
    if pos = String.length str || str.[pos] = ' ' then pos
    else read_number (pos + 1)
  in

  let res, pos = aux 0 in
  (res, String.sub str 0 (min (String.length str) pos))

let read_infix_term str =
  let rec aux_first pos =
    if pos = String.length str then failwith "wrong format of string"
    else if str.[pos] = ' ' then aux_first (pos + 1)
    else
      let res1, pos1 =
        if str.[pos] = '(' then aux_first (pos + 1)
        else
          let p = read_number (pos + 1) in
          try (Int (int_of_string (String.sub str pos (p - pos))), p)
          with _ -> failwith (string_of_int pos ^ " : " ^ string_of_int p)
      in
      let pos1 =
        let t = ref pos1 in
        while !t < String.length str && str.[!t] = ')' do
          t := !t + 1
        done;
        !t
      in
      try
        let op, pos2 = read_op pos1 in
        let res2, pos3 = aux_last pos2 in
        ((op_of_char op) res1 res2, pos3)
      with Invalid_argument _ -> (res1, pos1)
  and read_op pos =
    if str.[pos] = ' ' then read_op (pos + 1) else (str.[pos], pos + 1)
  and aux_last pos =
    if str.[pos] = ' ' then aux_last (pos + 1)
    else if str.[pos] = '(' then aux_first (pos + 1)
    else
      let p = read_number (pos + 1) in
      try (Int (int_of_string (String.sub str pos (p - pos))), go_to_end p)
      with _ -> failwith (string_of_int pos ^ " , " ^ string_of_int p)
  and read_number pos =
    if pos = String.length str || str.[pos] < '0' || str.[pos] > '9' then pos
    else read_number (pos + 1)
  and go_to_end pos =
    if pos = String.length str || str.[pos] = ')' then pos + 1
    else go_to_end (pos + 1)
  in

  let res, pos = aux_first 0 in
  (res, String.sub str 0 (min (String.length str) pos))

(* let get_string_from_type term = 
  match term with 
  | Min(t1, t2) -> "min(" 
  | Add(t1, t2) -> " + "
  | Sub(t1, t2) -> " - "
  | Mul(t1, t2) -> " * "
  | Div(t1, t2) -> " / "
  | Mod(t1, t2) -> " % "
  | Max(t1, t2) -> "max("
  | Int(x) -> failwith "shouldnt be here" *)


(* let rec infix_fold_left term acc =
  match term with 
  | Int(x) -> Int.to_string x 
  | _ ->  *)

let rec infix_string_of_term term =
    match term with 
    | Min(t1, t2) -> "min("^infix_string_of_term t1^","^infix_string_of_term t2^")" 
    | Add(t1, t2) -> ""^infix_string_of_term t1^" + "^infix_string_of_term t2^""
    | Sub(t1, t2) -> ""^infix_string_of_term t1^" - "^infix_string_of_term t2^""
    | Mul(t1, t2) -> ""^infix_string_of_term t1^" * "^infix_string_of_term t2^""
    | Div(t1, t2) -> ""^infix_string_of_term t1^" / "^infix_string_of_term t2^""
    | Mod(t1, t2) -> ""^infix_string_of_term t1^" % "^infix_string_of_term t2^""
    | Max(t1, t2) -> "max("^infix_string_of_term t1^","^infix_string_of_term t2^")" 
    | Int(x) -> string_of_int(x)


let test_infix = infix_string_of_term (Min(Add(Int(1), Int(2)), Sub(Int(1), Int(2))));;
Format.printf "%s@", test_infix;;
      
let rec polish_string_of_term term =
  match term with 
  | Min(t1, t2) -> "min("^polish_string_of_term t1^","^polish_string_of_term t2^")" 
  | Add(t1, t2) -> "+ "^polish_string_of_term t1^" "^polish_string_of_term t2^""
  | Sub(t1, t2) -> "- "^polish_string_of_term t1^" "^polish_string_of_term t2^""
  | Mul(t1, t2) -> "* "^polish_string_of_term t1^" "^polish_string_of_term t2^""
  | Div(t1, t2) -> "/ "^polish_string_of_term t1^" "^polish_string_of_term t2^""
  | Mod(t1, t2) -> "% "^polish_string_of_term t1^" "^polish_string_of_term t2^""
  | Max(t1, t2) -> "max("^polish_string_of_term t1^","^polish_string_of_term t2^")" 
  | Int(x) -> string_of_int(x)
    

let test_polish = polish_string_of_term (Min(Add(Int(1), Int(2)), Sub(Int(1), Int(2))));;
let test2_polish = polish_string_of_term(Mul(Add(Int(1), Int(2)), Int(3)));;
Format.printf "%s@", test_polish;;
Format.printf "%s@", test2_polish;;


let rec eval_term term =
  match term with 
  | Min(t1, t2) -> min (eval_term t1) (eval_term t2)
  | Add(t1, t2) -> eval_term t1 + eval_term t2
  | Sub(t1, t2) -> eval_term t1 - eval_term t2
  | Mul(t1, t2) -> eval_term t1 * eval_term t2 
  | Div(t1, t2) -> eval_term t1 / eval_term t2 
  | Mod(t1, t2) -> eval_term t1 mod eval_term t2 
  | Max(t1, t2) -> max (eval_term t1) (eval_term t2)
  | Int(x) -> x

let test_eval = eval_term (Mul(Add(Int(1), Int(2)), Int(3)));;


let check_and_insert (term: t) v_table num_table = 
  match Hashtbl.find_opt num_table term with 
  | Some value -> Hashtbl.replace num_table term (value+1)
  | None -> Hashtbl.add num_table term 1; Hashtbl.add v_table term (eval_term term);;

let rec treat_eval_term term v_table num_table = 
  match term with 
  | Int(x) -> check_and_insert term v_table num_table
  | Min(t1, t2) 
  | Add(t1, t2)  
  | Sub(t1, t2) 
  | Mul(t1, t2) 
  | Div(t1, t2) 
  | Mod(t1, t2) 
  | Max(t1, t2) -> 
    let _ = treat_eval_term t1 v_table num_table in 
    let _ = treat_eval_term t2 v_table num_table in 
    check_and_insert term v_table num_table




let aux_eval_term term =
  let h1 = Hashtbl.create 0 
  and h2 = Hashtbl.create 0
  in treat_eval_term term h1 h2; (h1, h2);;
  

let eval_term_opt term = aux_eval_term term;;


let (h1, h2) = eval_term_opt (Add(Int(1), (Int(2))));;
Seq.iter
  (fun (a, b) -> Format.printf "%s, %d\n" (infix_string_of_term a) b)
(Hashtbl.to_seq h1);;

Seq.iter
(fun (a, b) -> Format.printf "For h2: %s, %d\n" (infix_string_of_term a) b)
(Hashtbl.to_seq h2)