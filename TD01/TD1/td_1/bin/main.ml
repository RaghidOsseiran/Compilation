module CommandLine = struct
  let arg_spec_list = []
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [program name]\n"

  let parse () =
    let filename = ref None in
    Arg.parse (Arg.align arg_spec_list)
      (fun a ->
        match !filename with
        | None -> filename := Some a
        | Some _ ->
            raise
              (Arg.Bad
                 ("unexpected argument `" ^ a
                ^ "' (multiple inputs are not allowed : you must provide the \
                   input as a single string)")))
      usage_msg;
    !filename
end

let term_string =
  match CommandLine.parse () with
  | Some s -> s
  | None -> failwith "No term provided"

let term, str_read =
  try Term.read_infix_term term_string
  with _ -> (
    try Term.read_polish_string term_string
    with _ ->
      failwith
        "The provided string does not represent a term either in infix or \
         polish mode.")

let () = Format.printf "@[<v 0>term read : %s@," str_read

let () =
  Format.printf "polish representation : %s@,"
    (try Term.polish_string_of_term term
     with _ -> "computation crashed, is it implemented?")

let () =
  Format.printf "infix representation : %s@,"
    (try Term.infix_string_of_term term
     with _ -> "computation crashed, is it implemented?")

let () =
  Format.printf "value of term : %s@,"
    (try string_of_int (Term.eval_term term)
     with _ -> "computation crashed, is it implemented?")

let () = Format.printf "optimal evaluation :@,"

let () =
  try
    let value_table, occurence_table = Term.eval_term_opt term in
    let num_terms = Hashtbl.length value_table in
    let term_seen, num_seen =
      Hashtbl.fold
        (fun key value (t, num) ->
          if value > num then (key, value) else (t, num))
        occurence_table (Term.Int 0, 0)
    in
    Format.printf "Result : %s@,"
      (try string_of_int (Hashtbl.find value_table term)
       with _ -> "the result was not computed.");
    Format.printf "Number explored : %d@," num_terms;
    Format.printf "Most term seen %d times : %s@," num_seen
      (Term.infix_string_of_term term_seen)
  with _ -> Format.printf "computation crashed, is it implemented?@,"
