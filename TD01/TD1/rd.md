make
make tuto
make doc (dont know how to use yet)

dune utop (prefixer le module a utiliser ex: Term.)

example de print: let () = Format.printf "value of compo is: %d@," ((compo (fun x -> 2*x) (fun x -> x+1)) 4)



Example d'exception:


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



On trouve (2 * 3) dans deux type d'expression differente:

1 + (2 * 3) et   (2 * 3) - (1 + (2*3))

y'a qu'un seul deux car on trouve les deux que dans des (2 * 3) eq pour les 3 etc...
 