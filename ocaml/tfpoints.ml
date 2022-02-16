(* permutations of a list, copied from
   https://www2.lib.uchicago.edu/keith/chess960/permutations.html *)
let rec permutations (lst:'a list) : 'a list list =
  let rec insert x l = match l with
    | [] -> [[x]]
    | a::m -> (x::l) :: (List.map (fun y -> a::y) (insert x m))
  in
    match lst with
      | a::m -> List.flatten (List.map (insert a) (permutations m))
      | _ -> [lst]

(* run the whole search task *)
let run_all (inputs:int list) =
  (* intermediate variables *)
  let perms : int list list =
    permutations inputs
  in

  let opslist : B.binop list list =
    B.gen_binops 3
  in

  let found_f : F.formula option =
    perms
    |> List.map (F.build_formula_list opslist)   (* formula list list *)
    |> List.flatten                              (* flatten to formula list *)
    |> List.find_map F.check_formula             (* formla option *)
  in

  match found_f with
  | Some f -> print_endline(F.to_str f)
  | None -> print_endline "None"

let () =
  let inputs = Sys.argv
               |> fun x -> Array.sub x 1 4
               |> Array.to_list
               |> List.map int_of_string in
  run_all inputs
