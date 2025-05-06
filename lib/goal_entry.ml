open Pretty_printing
open Formula

type t = cost * side * Formula.t [@@deriving eq, show { with_path = false }]

let mk side formula = Formula.add_estimation (side, formula)
let formula (_, _, formula) = formula
let pp _fmt = failwith "todo"
let to_string = format_to_string pp
