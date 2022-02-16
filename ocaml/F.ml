(* Formula tree *)

open Printf;;

(* utility functions *)

let num_24 = Q.of_int 24
let num_zero = Q.zero

(* div function to prevent zero division *)
let div_opt: (Q.t option -> Q.t option -> Q.t option) =
  fun a b ->
  match (a, b) with
  | (Some x, Some y) -> if Q.equal y num_zero
                        then None
                        else Some (Q.(/) x y)
  | _ -> None

(* apply a two params function to optional params and generate optional result *)
let apply_opt: ((Q.t -> Q.t -> Q.t) -> Q.t option -> Q.t option -> Q.t option) =
  fun op a b ->
  match (a, b) with
  | (Some x, Some y) -> Some (op x y)
  | _ -> None

(* formula tree *)
type formula =
  | Num of int   (* leaf *)
  | Arith of formula * B.binop * formula  (* binary arithmatic formula *)

(* turn formula to a infix lisp like string representation *)
let rec simple_to_str (f: formula) : string =
  match f with
  | Num x -> string_of_int x
  | Arith (left, op, right) -> sprintf "(%s %s %s)" (simple_to_str left) (B.to_str op) (simple_to_str right)

(* formula to str, remove bracket as best as possible *)
let rec to_str (f: formula) : string =
  (* wrap the formula string with a pair of bracket to keep its
     arithmatic priority. *)
  let bracket_str (f1: formula): string =
    "(" ^ (to_str f1) ^ ")"
  in

  (* up level formula f is Multiply, only another multiply child can
     unwrap the bracket *)
  let mul_to_str (f1: formula): string =
    match f1 with
    | Num _ -> to_str f1
    | Arith (_, Mul, _) -> to_str f1
    | _ -> bracket_str f1
  in

  (* up level formula f is Sub and this formula is the right of sub operator. so the child nodes may unwrap the bracket around. *)
  let sub_right_to_str f1 =
    match f1 with
    | Arith (_, Add, _) -> bracket_str f1
    | Arith (_, Sub, _) -> bracket_str f1
    | _ -> to_str f1
  in

  (* up level formula f is Div, only leafe node is bracket
     unwrappable *)
  let div_to_str f1 =
    match f1 with
    | Num _ -> to_str f1
    | _ -> bracket_str f1
  in

  (* apply fn to both left and right child and concat them with op *)
  let concat : (formula -> string) -> formula -> B.binop -> formula -> string =
    fun fn left op right ->
    String.concat " " [fn left; B.to_str op; fn right]
  in

  (* join a list of string with spaces *)
  let join_str (slst: string list): string =
    String.concat " " slst
  in

  match f with
  | Num x -> string_of_int x
  | Arith (left, Add, right) -> concat to_str left Add right
  | Arith (left, Sub, right) -> join_str [to_str left;
                                          B.to_str Sub;
                                          sub_right_to_str right]
  | Arith (left, Mul, right) -> concat mul_to_str left Mul right
  | Arith (left, Div, right) -> concat div_to_str left Div right

(* calculate the formula to get an optional int result *)
let rec calc (f:formula):Q.t option =
  match f with
  | Num x -> Some (Q.of_int x)
  | Arith (left, Add, right) -> apply_opt Q.(+) (calc left) (calc right)
  | Arith (left, Sub, right) -> apply_opt Q.(-) (calc left) (calc right)
  | Arith (left, Mul, right) -> apply_opt Q.( * ) (calc left) (calc right)
  | Arith (left, Div, right) -> div_opt (calc left) (calc right)

(* check whether the result of calculation is 24 or not *)
let check (f: formula) : formula option =
  match calc f with
  | Some s -> if Q.equal s num_24
              then Some f (* here we got the answer, then lift it *)
              else None
  | _ -> None

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
  (* numbers is placed behind opslist to make currify easy *)
  fun opslist numbers ->
  let alist : formula list list =
    List.map (build_formula_types numbers) opslist
  in
  List.concat alist
