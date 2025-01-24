open Ast
open Abstract_machine

exception Return_exp of value option
exception Non_variable_reference of expr
exception Non_bool_test of expr
exception Non_integer_array_position of expr

let get_tab_pos name pos = name ^ string_of_int pos

(* Commencez à modifier à partir d’ici -- le code présent dans les fonctions n’est là que pour empêcher des warnings à la compilation qui obscurcirait votre sortie. Enlevez-le quand vous commencez à implémenter une fonction.*)

(* Sémantique d’une opération binaire*)
let operation_of_binop (op : binop) (v1 : value) (v2 : value) =
  ignore (op, v1, v2);
  VNone (*à faire*)

(* Sémantique d’une opération unaire*)
let operation_of_unop (op : unop) (v : value) =
  ignore (op, v);
  ignore get_tab_pos;
  VNone (* à faire*)

(* Cette fonction interprète une expression et renvoie sa valeur. Vous devez traiter tous les cas possibles (cf module Ast). Reportez-vous au cours pour une explication de la sémantique. On conseille de traiter parallèlement expressions et instructions par ordre de complexité (décrit dans le cours). Vous pouvez laisser des cas non-traités simplement en leur associant [failwith "todo"] qui fera planter le programme sur ces cas, mais permettra de compiler et de traiter les autres.*)
let rec interpret_expr (map : value Util.Environment.t)
    (map_function : (Ast.argument list * Ast.instruction) Util.Environment.t)
    (expr : Ast.expr) =
  if false then ignore (interpret_expr map map_function expr);
  VNone
(*à remplacer par le code : ce code n’est là que pour que le programme compile sans warning.*)

(* Cette fonction interprète une instruction. Le «and» est là pour qu’elle soit co-récursive avec interpret_expr (à cause des appels de fonctions). Elle ne renvoie rien, mais applique directement des effets de bord sur [map]. Reportez-vous au cours pour la sémantique.*)
and interpret_instruction (map : value Util.Environment.t)
    (map_function : (Ast.argument list * Ast.instruction) Util.Environment.t)
    (instruction : Ast.instruction) =
  ignore (map, map_function, instruction);
  (*à compléter*) ()

(*Cette fonction doit interpréter une déclaration de fonction. Elle consiste simplement à associer la liste des arguments et le corps de la fonction à son nom dans l’environnement [functions].*)
let interpret_func_decl
    (functions : (Ast.argument list * Ast.instruction) Util.Environment.t)
    (func_decl : Ast.function_decl) =
  ignore (functions, func_decl);
  () (*à compléter*)

(* Cette fonction utilitaire vous est fournie : elle permet de mettre la liste des arguments à la même taille que celle des paramètres de la fonction main : s’il n’y en a pas assez, on leur attribue la valeu VNone, s’il y en a trop, on ignore les derniers. Cela permet de rendre la ligne de commande un peu plus résiliente à un mauvais nombre d’argument sur l’exécution d’un programme*)
let normalize_arg_list args vars =
  if List.length args < List.length vars then
    args @ List.init (List.length vars - List.length args) (fun _ -> "")
  else if List.length args > List.length vars then
    List.filteri (fun i _ -> i < List.length vars) args
  else args

(* Cette fonction permet d’exécuter une liste de déclaration de fonctions sur les arguments passés à la ligne de commande, et lance dessus la fonction main. Elle analyse la liste des fonctions, et stocke leurs définitions dans un environnement de fonctions, puis récupère la définition de la fonction nommée "main", crée un environnement de variables à partir de [args] (normalisées avec la fonction ci-dessus) et de ses paramètres et enfin appelle le corps de main sur ces arguments (comme un appel de fonction, sauf qu’ici les arguments sont directement des objets sémantiques et non syntaxique). Elle est au fond similaire à un appel de fonction, mais un peu plus technique, donc on vous la fourni.*)

let interpret_prg prg args =
  let functions = Util.Environment.new_environment () in
  List.iter (interpret_func_decl functions) prg;
  let environnement = Util.Environment.new_environment () in
  let params, body =
    try Option.get (Util.Environment.get functions "main")
    with _ -> failwith "Function main not defined!"
  in
  let vars = List.map (fun (_, _, v) -> v) params in
  let args = normalize_arg_list args vars in
  List.iter2
    (fun v a ->
      Abstract_machine.parse_complex_argument_and_affect environnement v a)
    vars args;
  try interpret_instruction environnement functions body
  with Return_exp _ -> ()
