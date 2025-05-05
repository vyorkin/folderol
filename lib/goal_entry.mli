open Formula

type t = cost * side * Formula.t [@@deriving eq, show]

val formula : t -> Formula.t
