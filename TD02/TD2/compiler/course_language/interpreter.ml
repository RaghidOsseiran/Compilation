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
  match (op, v1) with 
  | (Add, VInt _) -> add_i v1 v2 
  | (Add, VFloat _) -> add_f v1 v2
  | (Sub,  VInt _) -> sub_i v1 v2 
  | (Sub, VFloat _) -> sub_f v1 v2 
  | (Mul, VInt _) -> mul_i v1 v2 
  | (Mul, VFloat _) -> mul_f v1 v2
  | (Div, VInt _) -> div_i v1 v2 
  | (Div, VFloat _) -> div_f v1 v2
  | (Mod, VInt _) -> mod_i v1 v2   
  | (Mod, VFloat _) -> mod_f v1 v2
  | (And, VBool _) -> and_b v1 v2
  | (Or, VBool _) -> or_b v1 v2
  | (Eq, _) -> eq_m v1 v2
  | (Neq, _) -> not_b(eq_m v1 v2)
  | (Lt, _) -> lt_m v1 v2 
  | (Gt, _) -> and_b(not_b(lt_m v1 v2))(not_b(eq_m v1 v2))
  | (Leq, _) -> or_b (lt_m v1 v2)(eq_m v1 v2)
  | (Geq, _) -> or_b (eq_m v1 v2)(not_b(lt_m v1 v2))
  | _ -> failwith "invalid types given to value"

(* Sémantique d’une opération unaire*)
let operation_of_unop (op : unop) (v : value) =
  match (op, v) with 
  | (UMin, VInt _) -> operation_of_binop Sub (VInt(0)) v
  | (UMin, VFloat _) -> operation_of_binop Sub (VFloat(0.0)) v
  | (UMin, _) -> failwith "not defined for other types"
  | (Not, _) -> not_b v

(* Cette fonction interprète une expression et renvoie sa valeur. Vous devez traiter tous les cas possibles (cf module Ast). Reportez-vous au cours pour une explication de la sémantique. On conseille de traiter parallèlement expressions et instructions par ordre de complexité (décrit dans le cours). Vous pouvez laisser des cas non-traités simplement en leur associant [failwith "todo"] qui fera planter le programme sur ces cas, mais permettra de compiler et de traiter les autres.*)

let get_val_from_ref str_ref env = 
  match (Util.Environment.get_ref env str_ref) with 
  | Some(v_ref) -> v_ref.contents 
  | None -> failwith "couldnt find value at this reference"




let rec interpret_expr (map : value Util.Environment.t)
    (map_function : (Ast.argument list * Ast.instruction) Util.Environment.t)
    (expr : Ast.expr) =
    match expr with 
    | Cst_i (x, _) -> VInt x 
    | Cst_f (f, _) -> VFloat f 
    | Cst_b (b, _) -> VBool b 
    | Var(str, _) ->  get_val_from_ref str map
    | Binop(binop, expr1, expr2, _) -> operation_of_binop binop 
                                      (interpret_expr map map_function expr1) 
                                      (interpret_expr map map_function expr2)
    | Unop(unop, expr, _) -> operation_of_unop unop 
                            (interpret_expr map map_function expr)
    | _ -> failwith "todo"
(*à remplacer par le code : ce code n’est là que pour que le programme compile sans warning.*)

(* Cette fonction interprète une instruction. Le «and» est là pour qu’elle soit co-récursive avec interpret_expr (à cause des appels de fonctions). Elle ne renvoie rien, mais applique directement des effets de bord sur [map]. Reportez-vous au cours pour la sémantique.*)





let rec interpret_instruction (map : value Util.Environment.t)
    (map_function : (Ast.argument list * Ast.instruction) Util.Environment.t)
    (instruction : Ast.instruction) =
    match instruction with 
    | Affect(str, expr, _) -> Util.Environment.add map str 
                              (interpret_expr map map_function expr) 
    | Block(inst_list, _) -> List.fold_left 
                      (fun _ inst -> interpret_instruction map map_function inst) () inst_list
    | IfThenElse(expr, inst1, inst2, _) -> 
          if (interpret_expr map map_function expr = VBool(true)) 
          then interpret_instruction map map_function inst1
          else interpret_instruction map map_function inst2 
    | While(expr, instr, _) -> 
      let rec while_loop () = 
        match interpret_expr map map_function expr with 
        | VBool true -> interpret_instruction map map_function instr;
                        while_loop()
        | VBool false -> ()
        | _ -> failwith "invalid valuation"
      in while_loop()
    | _ -> failwith "todo"

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
