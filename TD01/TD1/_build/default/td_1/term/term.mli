type t =
  | Int of int
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Mod of t * t
  | Min of t * t
  | Max of t * t

(** Type representing an arithmetical term. For example [Add (Int(1),Int(2))] represents the term [1+2] (in infix representation)*)

val read_polish_string : string -> t * string
(** Reads a string representing a term in polish notation and returns the corresponding term and the portion of the string read. '+' represents addition, '-' substraction, '*' multiplication, '/' division, '%' modulo, 'A' maximum and 'v' minimum.*)

val read_infix_term : string -> t * string
(** Reads a string representing a term in infix notation and returns the corresponding term and the portion of the string read. The term must be entirely parenthesized for the algorithm to work.*)

val infix_string_of_term : t -> string
(** Produces a string that represents the term in infix notation. For example the term [Add(Int(4),Mul(Int(6),Int(5)))] should be displayed as [4 + (6 * 5)]. Parentheses should be used for priorities, but we do not ask that you remove them when the priorities are implied (like in the previous example where [4 + 6 * 5] would be equivalent. *)

val polish_string_of_term : t -> string
(** Produces a string that represents the term in polish notation. For example, the term [Add(Int(4),Mul(Int(6),Int(5))] should be displayed as [+ 4 * 6 5]. Notice that in this representation, parentheses do not exist.*)

val eval_term : t -> int
(** Computes the value of a term. If a division by 0 occurs, the function will crash (by the system division by 0).*)

val eval_term_opt : t -> (t, int) Hashtbl.t * (t, int) Hashtbl.t
(**Computes the value of a term, but evaluates each identical subterm only once. It returns two hashtables : the first one containing for each subterms appearing its value, and the second for each term the number of times it was seen in the algorithm.*)