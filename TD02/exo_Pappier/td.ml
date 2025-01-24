type expr_x = Cst of int | Var of string | A of (expr_x * expr_x) | B of (expr_x * expr_x);;
let p = Hashtbl.create 10;;
Hashtbl.add p "x" 5;;
Hashtbl.add p "y" 7;;

let rec evaluate_expr_x (exp:expr_x) = 
  match exp with 
  | Cst(n) -> n 
  | Var(str) -> Hashtbl.find p str 
  | A(exp1, exp2) -> 2 * evaluate_expr_x exp1 + evaluate_expr_x exp2 
  | B(exp1, exp2) -> evaluate_expr_x exp1 - 2 * evaluate_expr_x exp2;;


let result = evaluate_expr_x (A(A(Cst(3),Var("x")),B(Var("y"),B(Cst(4),Cst(3)))));;

let p2 = Hashtbl.create 10;;
Hashtbl.add p2 "x" 4;;
Hashtbl.add p2 "y" 5;;


type binop = Add | Sub | Mul | Div | Mod 
            | And | Or 
            | Eq | Neq | Lt | Gt | Leq | Geq

type unop = UMin | Not

type expr = Var of (string) | Cst_i of (int) | Cst_f of (float) | Cst_b of (bool)
          | Binop of (binop * expr * expr) 
          | Unop of (unop * expr) 
          | Array_val of (string * expr) 
          | Size_tab of (string) 


type instr = Affect of (string * expr)
            | Block of (instr list) 
            | IfThenElse of (expr * instr * instr)
            | While of (expr * instr) (* more to come *)


let add_or_replace (key:string) (value: int) = 
  if Hashtbl.mem p2 key then Hashtbl.replace p2 key value else Hashtbl.add p2 key value;;



let evaluate_binop (op:binop) (val1) (val2) = 
  match op with 
  | Add -> val1 + val2 
  | Sub -> val1 - val2
  | Mul -> val1 * val2
  | Div -> val1 / val2
  | Mod -> val1 mod val2
  | And 
  | Or 
  | Eq | Neq | Lt | Gt | Leq | Geq -> failwith "not done yet"


let evaluate_unop (op: unop) (value) = 
  match op with 
  | UMin -> 0 - value 
  | Not -> failwith "not done yet"

let rec evaluate_expr (exp: expr) = 
  match exp with 
  | Var(str) -> Hashtbl.find p2 str
  | Cst_i(x) -> x
  | Cst_f(x_f) -> failwith "not"
  | Cst_b(b) -> failwith "not"
  | Binop(binop, expr1, expr2) -> evaluate_binop binop (evaluate_expr expr1) (evaluate_expr expr2)
  | Unop(unop, expr) -> evaluate_unop (unop) (evaluate_expr expr)
  | Array_val(str, expr) -> failwith "not done yet"
  | Size_tab(str) -> failwith "not done yet"


let rec handle_instruction (inst: instr) = 
  match inst with 
  | Affect(str, expr) -> add_or_replace str (evaluate_expr expr)
  | Block(inst_list) -> List.fold_left (fun acc inst -> handle_instruction inst) () inst_list
  | IfThenElse(expr, inst1, inst2) -> (* if (evaluate_expr expr) then handle_instruction inst1 else handle_instruction inst2 *)
     failwith "not done yet"
  | While(expr, inst1) -> failwith "not done yet"


let i1 = (Affect("x",Binop(Add,Var("y"),Cst_i(2))));;
let i2 = (Affect("y",Binop(Sub, Var("x"), Cst_i(3))));;
let i3 = Block([i1; i2]);;
(* let call1 = handle_instruction i1;;
let x1 = Hashtbl.find p2 "x";;
let call2 = handle_instruction i2;;
let x2 = Hashtbl.find p2 "y";; *)
let call3 = handle_instruction i3;;
let x3 = Hashtbl.find p2 "x";;
let y3 = Hashtbl.find p2 "y";;
