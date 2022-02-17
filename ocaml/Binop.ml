(* module B describes binop and related functions *)
(* define allowed opeators *)
type binop = Add | Sub | Mul | Div;;

let to_str (op: binop): string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/";;

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
