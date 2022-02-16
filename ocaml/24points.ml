(* define allowed opeators *)
type binop = Add | Sub | Mul | Div;;

(* formula tree *)
type formula =
  | Num of int   (* leaf *)
  | Arith of formula * binop * formula;;  (* binary arithmatic formula *)

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


let binop_to_str (op: binop): string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/";;

(* formula to str, remove braket as best as possible *)
let rec formula_to_str (f: formula) : string =
  (* wrap the formula string with a pair of braket *)
  let braket_str (f1:formula): string =
    "(" ^ (formula_to_str f1) ^ ")"
  in

  (* top level formula f is Multiply only another mul child can unwrap
     the braket *)
  let mul_to_str f1 =
    match f1 with
    | Arith (_, Add, _) -> braket_str f1
    | Arith (_, Sub, _) -> braket_str f1
    | Arith (_, Div, _) -> braket_str f1
    | _ -> formula_to_str f1
  in

  (* top level formula f is Add|Sub which are low priority
     operators. so the child nodes may unwrap the braket around. *)
  let add_sub_to_str f1 =
    match f1 with
    | Arith (_, Add, _) -> braket_str f1
    | Arith (_, Sub, _) -> braket_str f1
    | _ -> formula_to_str f1
  in

  (* top level formula f is Div, only leafe node is braket
     unwrappable *)
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


(* apply a two params function to optional params and generate optional result *)
let apply_opt: (('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option) =
  fun op a b ->
  match (a, b) with
  | (Some x, Some y) -> Some (op x y)
  | _ -> None

(* div function to prevent zero division *)
let div_opt: ('a option -> 'a option -> 'a option) =
  fun a b ->
  match (a, b) with
  | (Some _, Some 0) -> None
  | (Some x, Some y) -> Some (x / y)
  | _ -> None

(* calculate the formula to get an optional int result *)
let rec calc_fomula (f:formula):int option =
  match f with
  | Num x -> Some x
  | Arith (left, Add, right) -> apply_opt (+) (calc_fomula left) (calc_fomula right)
  | Arith (left, Sub, right) -> apply_opt (-) (calc_fomula left) (calc_fomula right)
  | Arith (left, Mul, right) -> apply_opt (fun x y -> x * y) (calc_fomula left) (calc_fomula right)
  | Arith (left, Div, right) -> div_opt (calc_fomula left) (calc_fomula right)

(* check whether the result of calculation is 24 or not *)
let check_formula (f:formula):formula option =
  let result = calc_fomula f in
  match result with
  | None -> None
  | Some 24 -> Some f
  | Some _ -> None

(* for each sample of number list and binop list there are 3 types of
   formula trees. *)
let build_formula_types : (int list -> binop list -> formula list) =
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

(* build all possible formula list *)
let build_formula_list : (binop list list -> int list -> formula list) =
  (* numbers is placed to the last params to make currify easy *)
  fun opslist numbers ->
  let alist =
    List.map (build_formula_types numbers) opslist
  in
  List.concat alist

(* prepend all binops ahead of items *)
let prepend_binops (lastitems: binop list list) : binop list list =
  let kadd = List.map (fun t -> Add :: t) lastitems in
  let ksub = List.map (fun t -> Sub :: t) lastitems in
  let kmul = List.map (fun t -> Mul :: t) lastitems in
  let kdiv = List.map (fun t -> Div :: t) lastitems in
  List.concat [kadd; ksub; kmul; kdiv]

(* generate a 4 x n binop list list *)
let rec gen_binops (n: int): binop list list =
  match n with
  | 1 -> [[Add]; [Sub]; [Mul]; [Div]]
  | x -> gen_binops (x-1) |> prepend_binops

(* run the whole search task *)
let run_all (inputs:int list) =
  (* intermediate variables *)
  let perms : int list list =
    permutations inputs
  in

  let opslist : binop list list =
    gen_binops 3
  in

  let found_f : formula option =
    perms
    |> List.map (build_formula_list opslist)   (* formula list list *)
    |> List.flatten                            (* flatten to formula list *)
    |> List.find_map check_formula             (* formla option *)
  in

  match found_f with
  | Some f -> print_endline(formula_to_str f)
  | None -> print_endline "None"

let () =
  let str_inputs = Sys.argv
                   |> fun x -> Array.sub x 1 4
                   |> Array.to_list in
  let inputs = List.map int_of_string str_inputs in
  run_all inputs
