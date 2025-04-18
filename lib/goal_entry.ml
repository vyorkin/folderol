open Formula

type t = cost * side * Formula.t [@@deriving eq, show { with_path = false }]
