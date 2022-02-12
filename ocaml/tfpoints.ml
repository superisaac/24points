open Printf;;

type binop = Add | Sub | Mul | Div;;

type formula_tree =
  | Num of int
  | Arith of formula_tree * binop * formula_tree;;

let rec permutations lst =
  let rec insert x l = match l with
    | [] -> [[x]]
    | a::m -> (x::l) :: (List.map (fun y -> a::y) (insert x m))
  in
    match lst with
      | a::m -> List.flatten (List.map (insert a) (permutations m))
      | _ -> [lst]

let b_to_str op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/";;

let rec f_to_str f =
  match f with
  | Num x -> sprintf "%d" x
  | Arith (left, bop, right) -> sprintf "(%s %s %s)" (b_to_str bop) (f_to_str left) (f_to_str right)

let fapply op a  b =
  match (a, b) with
  | (Some x, Some y) -> Some (op x y)
  | _ -> None

let fdiv a  b =
  match (a, b) with
  | (Some _, Some 0) -> None
  | (Some x, Some y) -> Some (x + y)
  | _ -> None

let rec f_calc f =
  match f with
  | Num x -> Some x
  | Arith (left, Add, right) -> fapply (+) (f_calc left) (f_calc right)
  | Arith (left, Sub, right) -> fapply (-) (f_calc left) (f_calc right)
  | Arith (left, Mul, right) -> fapply (fun x y -> x * y) (f_calc left) (f_calc right)
  | Arith (left, Div, right) -> fdiv (f_calc left) (f_calc right)

let check f =
  let result = f_calc f in
  match result with
  | None -> None
  | Some 24 -> Some f
  | Some _ -> None

let build_formula numbers ops =
  let a = Num (List.nth numbers 0) in
  let b = Num (List.nth numbers 1) in
  let c = Num (List.nth numbers 2) in
  let d = Num (List.nth numbers 3) in
  let op0 = List.nth ops 0 in
  let op1 = List.nth ops 1 in
  let op2 = List.nth ops 2 in
  [Arith (Arith (Arith (a, op0, b), op1, c), op2, d);
   Arith (Arith (a, op0, b), op1, Arith (c, op2, d));
   Arith (a, op0, Arith (b, op1, Arith (c, op2, d)))];;

let build_formula_ops_list numbers opslist =
  let alist = List.map (fun ops -> build_formula numbers ops) opslist in
  List.concat alist

let prepend_op lastitems =
  let kadd = List.map (fun t -> Add :: t) lastitems in
  let ksub = List.map (fun t -> Sub :: t) lastitems in
  let kmul = List.map (fun t -> Mul :: t) lastitems in
  let kdiv = List.map (fun t -> Div :: t) lastitems in
  List.concat [kadd; ksub; kmul; kdiv]

let rec join_binops n =
  match n with
  | 1 -> [[Add]; [Sub]; [Mul]; [Div]]
  | x -> prepend_op (join_binops (x-1))

let run_all inputs =
  let perms = permutations inputs in
  let opslist = join_binops 3 in
  let ftree1 = List.map (fun numbers -> build_formula_ops_list numbers opslist) perms in
  let trees = List.flatten ftree1 in
  let found_f = List.find_map check trees in
  match found_f with
  | Some f -> print_endline(f_to_str f)
  | None -> print_endline "None"

let () =
  let a = Sys.argv.(1) in
  let b = Sys.argv.(2) in
  let c = Sys.argv.(3) in
  let d = Sys.argv.(4) in
  let str_inputs = [a; b; c; d] in
  let inputs = List.map int_of_string str_inputs in
  run_all inputs
