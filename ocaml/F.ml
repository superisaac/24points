(* Formula tree *)

open Printf;;

(* utility functions *)
(* div function to prevent zero division *)
let div_opt: (int option -> int option -> int option) =
  fun a b ->
  match (a, b) with
  | (Some _, Some 0) -> None
  | (Some x, Some y) ->
     if x mod y == 0
     then Some (x / y)
     else None
  | _ -> None

(* apply a two params function to optional params and generate optional result *)
let apply_opt: (('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option) =
  fun op a b ->
  match (a, b) with
  | (Some x, Some y) -> Some (op x y)
  | _ -> None

(* formula tree *)
type formula =
  | Num of int   (* leaf *)
  | Arith of formula * B.binop * formula;;  (* binary arithmatic formula *)

let rec simple_to_str (f: formula) : string =
  match f with
  | Num x -> string_of_int x
  | Arith (left, op, right) -> sprintf "(%s %s %s)" (simple_to_str left) (B.to_str op) (simple_to_str right)

(* formula to str, remove braket as best as possible *)
let rec to_str (f: formula) : string =
  (* wrap the formula string with a pair of braket *)
  let braket_str (f1: formula): string =
    "(" ^ (to_str f1) ^ ")"
  in

  (* top level formula f is Multiply only another mul child can unwrap
     the braket *)
  let mul_to_str f1 =
    match f1 with
    | Arith (_, Add, _) -> braket_str f1
    | Arith (_, Sub, _) -> braket_str f1
    | Arith (_, Div, _) -> braket_str f1
    | _ -> to_str f1
  in

  (* top level formula f is Add|Sub which are low priority
     operators. so the child nodes may unwrap the braket around. *)
  let add_sub_to_str f1 =
    match f1 with
    | Arith (_, Add, _) -> braket_str f1
    | Arith (_, Sub, _) -> braket_str f1
    | _ -> to_str f1
  in

  (* top level formula f is Div, only leafe node is braket
     unwrappable *)
  let div_to_str f1 =
    match f1 with
    | Num x -> string_of_int x
    | _ -> braket_str f1
  in

  let concat child_fn left op right =
    (child_fn left) ^ " " ^ (B.to_str op) ^ " " ^ (child_fn right)
  in

  match f with
  | Num x -> string_of_int x
  | Arith (left, Add, right) -> concat add_sub_to_str left Add right
  | Arith (left, Sub, right) -> concat add_sub_to_str left Sub right
  | Arith (left, Mul, right) -> concat mul_to_str left Mul right
  | Arith (left, Div, right) -> concat div_to_str left Div right

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
let build_formula_types : (int list -> B.binop list -> formula list) =
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
   Arith (a, op0, Arith (b, op1, Arith (c, op2, d)))]


(* build all possible formula list *)
let build_formula_list : (B.binop list list -> int list -> formula list) =
  (* numbers is placed to the last params to make currify easy *)
  fun opslist numbers ->
  let alist =
    List.map (build_formula_types numbers) opslist
  in
  List.concat alist
