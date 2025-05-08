open Core
open Pretty_printing
open Formula

type t = cost * side * Formula.t [@@deriving eq, show { with_path = false }]

let mk side formula = Formula.add_estimation (side, formula)
let formula (_, _, formula) = formula

let pp_debug fmt (cost, side, formula) =
  Format.fprintf fmt "(%d, %a, %a)" cost Formula.pp_side side Formula.pp_formula
    formula

let pp fmt (cost, side, formula) =
  let open Formula in
  match side with
  | L -> Format.fprintf fmt "(%d) %a |-" cost pp_formula formula
  | R -> Format.fprintf fmt "|- (%d) %a" cost pp_formula formula

let to_string = format_to_string pp
