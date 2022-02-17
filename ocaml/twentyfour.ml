open Printf;;

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

module F = Formula
module B = Binop

(* run the whole search task, return the first result *)
let search_first (inputs:int list) : F.formula option =
  (* intermediate variables *)
  let opslist : B.binop list list =
    B.gen_binops 3
  in

  inputs
  |> permutations                              (* int list list *)
  |> List.map (F.build_formula_list opslist)   (* formula list list *)
  |> List.flatten                              (* flatten to formula list *)
  |> List.find_map F.check                     (* formula option *)

(* run the whole search task, return all results *)
let search_all (inputs:int list) : F.formula option list =
  (* intermediate variables *)
  let opslist : B.binop list list =
    B.gen_binops 3
  in

  inputs
  |> permutations                              (* int list list *)
  |> List.map (F.build_formula_list opslist)   (* formula list list *)
  |> List.flatten                              (* flatten to formula list *)
  |> List.map F.check                          (* formula option *)
;;

(* print the formula tree both in form of simplified s-expr like and
   tidied s-expr like statements *)
let print_formula_opt fopt =
  match fopt with
  | Some f -> printf "%s <==> %s\n" (F.to_str f) (F.simple_to_str f)
  | _ -> ()

let () =
  Array.sub Sys.argv 1 4
  |> Array.to_list
  |> List.map int_of_string
  |> search_all
  |> List.iter print_formula_opt
