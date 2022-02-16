type binop = Add | Sub | Mul | Div;;

type formula =
  | Num of int
  | Arith of formula * binop * formula;;

let rec permutations (lst:'a list) : 'a list list =
  let rec insert x l = match l with
    | [] -> [[x]]
    | a::m -> (x::l) :: (List.map (fun y -> a::y) (insert x m))
  in
    match lst with
      | a::m -> List.flatten (List.map (insert a) (permutations m))
      | _ -> [lst]

let b_to_str (op: binop): string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/";;

let rec formula_to_str (f: formula) : string =
  let braket_str (f1:formula): string =
    "(" ^ (formula_to_str f1) ^ ")"
  in

  let mul_to_str f1 =
    match f1 with
    | Arith (_, Add, _) -> braket_str f1
    | Arith (_, Sub, _) -> braket_str f1
    | Arith (_, Div, _) -> braket_str f1
    | _ -> formula_to_str f1
  in

  let add_sub_to_str f1 =
    match f1 with
    | Arith (_, Add, _) -> braket_str f1
    | Arith (_, Sub, _) -> braket_str f1
    | _ -> formula_to_str f1
  in

  let div_to_str f1 =
    match f1 with
    | Num x -> string_of_int x
    | _ -> braket_str f1
  in
  match f with
  | Num x -> string_of_int x
  | Arith (left, Add, right) -> (add_sub_to_str left) ^ " + " ^ (add_sub_to_str right)
  | Arith (left, Sub, right) -> (add_sub_to_str left) ^ " - " ^ (add_sub_to_str right)
  | Arith (left, Mul, right) -> (mul_to_str left) ^ " * " ^ (mul_to_str right)
  | Arith (left, Div, right) -> (div_to_str left) ^ " * " ^ (div_to_str right);;


let fapply: (('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option) =
  fun op a b ->
  match (a, b) with
  | (Some x, Some y) -> Some (op x y)
  | _ -> None

let fdiv: ('a option -> 'a option -> 'a option) =
  fun a b ->
  match (a, b) with
  | (Some _, Some 0) -> None
  | (Some x, Some y) -> Some (x + y)
  | _ -> None

let rec f_calc (f:formula):int option =
  match f with
  | Num x -> Some x
  | Arith (left, Add, right) -> fapply (+) (f_calc left) (f_calc right)
  | Arith (left, Sub, right) -> fapply (-) (f_calc left) (f_calc right)
  | Arith (left, Mul, right) -> fapply (fun x y -> x * y) (f_calc left) (f_calc right)
  | Arith (left, Div, right) -> fdiv (f_calc left) (f_calc right)

let check (f:formula):formula option =
  let result = f_calc f in
  match result with
  | None -> None
  | Some 24 -> Some f
  | Some _ -> None

let build_formula : (int list -> binop list -> formula list) =
  fun numbers ops ->
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

let build_formula_ops_list : (int list -> binop list list -> formula list) =
  fun numbers opslist ->
  let alist = List.map (fun ops -> build_formula numbers ops) opslist in
  List.concat alist

let prepend_op (lastitems:binop list list): binop list list =
  let kadd = List.map (fun t -> Add :: t) lastitems in
  let ksub = List.map (fun t -> Sub :: t) lastitems in
  let kmul = List.map (fun t -> Mul :: t) lastitems in
  let kdiv = List.map (fun t -> Div :: t) lastitems in
  List.concat [kadd; ksub; kmul; kdiv]

let rec join_binops (n: int): binop list list =
  match n with
  | 1 -> [[Add]; [Sub]; [Mul]; [Div]]
  | x -> prepend_op (join_binops (x-1))

let run_all (inputs:int list) =
  let perms : int list list = permutations inputs in
  let opslist : binop list list = join_binops 3 in
  let ftree1 : formula list list = List.map (fun numbers -> build_formula_ops_list numbers opslist) perms in
  let trees : formula list = List.flatten ftree1 in
  let found_f : formula option = List.find_map check trees in
  match found_f with
  | Some f -> print_endline(formula_to_str f)
  | None -> print_endline "None"

let () =
  let str_inputs = Array.sub Sys.argv 1 4
                   |> Array.to_list in
  let inputs = List.map int_of_string str_inputs in
  run_all inputs
