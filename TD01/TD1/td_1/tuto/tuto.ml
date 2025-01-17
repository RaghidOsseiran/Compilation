(* Commentaries are denoted this way. *)

(*
   To compile, use 'make'.
   To use in toplevel, use '#use "tuto/tuto.ml"' in utop.
*)

(* This lines simply opens a bounding box for display *)
let () = Format.printf "@[<v 0>"
let example = "voici une chaîne"

let sept =
  let quatre = 4 in
  3 + quatre

let () = Format.printf "%s, sept c’est %d@," example sept
let succ x = x + 1
let () = Format.printf "%d is the successor of %d@," (succ 2) 2

let f x =
  let succ x = x + 1 in
  let prec x = x - 1 in
  if x < 0 then prec x else succ x

let () = Format.printf "%d is the \"successor\" of %d@," (f 2) 2
let () = Format.printf "%d is the \"successor\" of %d@," (f (-2)) (-2)
let g f x = f x + 1
let () = Format.printf "%d@," (g int_of_string "25")
let disjunct f g b = if b then f else g

let res =
  (disjunct (fun x -> if x < 0 then 0 else x + 1) (fun x -> x - 1) true) 3

let i x = x + 1

(* TODO *)

   let compo f g = fun x -> f (g x);;

   let () = Format.printf "value of compo is: %d@," ((compo (fun x -> 2*x) (fun x -> x+1)) 4)

   (* Should return 10*)

let rec f1 x =
  let g1 y = if y < 0 then 0 else f1 (y - 1) + 2 in
  if x < 0 then 1 else g1 (x - 2) * 3

let () = Format.printf "f1 25 = %d and g1 is not visible@," (f1 25)

let rec f2 x = if x < 0 then 1 else g2 (x - 2) * 3
and g2 y = if y < 0 then 0 else f2 (y - 1) + 2

let () = Format.printf "f2 25 = %d and g2 25 = %d@," (f2 25) (g2 25)

type toto1 = int * char
type toto2 = None | One of int | Two of toto1
type tree = Leaf of int | Node of tree * tree

let rec sum_of_tree t =
  match t with Leaf a -> a | Node (t1, t2) -> sum_of_tree t1 + sum_of_tree t2

let () =
  Format.printf "%d@," (sum_of_tree (Node (Node (Leaf 1, Leaf 2), Leaf 3)))

let rec silly t =
  match t with
  | Node (Leaf _, _) -> 1
  | Node (_, t1) -> 1 + silly t1
  | _ -> failwith "arg"

(* To try (will fail):
   let () = Format.printf "%d@," (silly (Node(Node(Leaf 1,Leaf 2),Leaf 3)))
*)
let () = Format.printf "%d@," (silly (Node (Leaf 3, Node (Leaf 1, Leaf 2))))
let when_fun a = match a with Some a when a < 0 -> 0 | Some a -> a | None -> 0

let () =
  Format.printf "%d -- %d -- %d@," (when_fun (Some (-4))) (when_fun (Some 5))
    (when_fun None)



   type movement = N | E | W | S 
   type direction = movement * int


   let apply_movement pos (move: direction) =
    let (x, y) = pos in 
    match move with 
    | (dir, speed) -> if speed > 0 then 
      match dir with 
      | N -> (x, y + speed)
      | S -> (x, y - speed)
      | E -> (x + speed, y)
      | W -> (x - speed, y)
    else pos

let list = List.map (fun x -> int_of_string x + 1) [ "12"; "0"; "-2" ]

let () =
  List.iter (Format.printf "%d ++ ") list;
  Format.printf "@,"

let () = Format.printf "%d@," (List.fold_left (fun x y -> x + y) 1 [ 2; 3; 4 ])



   let list_pos = [(4,4);(7,8);(-4,7);(2,-4); (-5,-5)]

   let apply_movement_list move list =
    let rec aux_apply acc list = 
      match list with 
      | [] -> acc 
      | h::tail -> 
        aux_apply (apply_movement move h :: acc) tail
      in aux_apply [] list;;

(* To do:

   let find_farthest_point list =
*)

exception Stop of int * string

let compute_num v =
  if v = 0 then raise (Stop (0, "0"))
  else if v = 42 then raise (Stop (42, "quarante-deux"))
  else if v = 2 then raise (Stop (-45, "six"))
  else if v = -3 then failwith "Et bim"
  else v - 1

(*Dans l’expression suivante, amusez-vous à remplacer la valeur de num par autre chose.*)
let num = 5

let () =
  try Format.printf "ah: %d@," (compute_num num)
  with Stop (n, s) -> Format.printf "Stop : %d , %s@," n s

(*De même, ajoutez une valeur négative dans la liste ci-dessous :*)
let l = [ 1; 2; 3; 4; 5 ]

let () =
  try
    Format.printf "la somme de la liste (toute positive) est : %d @,"
      (List.fold_left
         (fun acc v ->
           if v < 0 then
             raise (Stop (v, "somme partielle " ^ string_of_int acc))
           else acc + v)
         0 l)
  with Stop (n, s) ->
    Format.printf "La liste contient une valeur négative : %d, et :: %s@," n s

(*Closes the bounding box.*)
let () = Format.printf "@]"
