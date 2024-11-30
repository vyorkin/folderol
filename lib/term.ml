module List = Core.List

type t =
  | Var of string
  | Param of string * string list
  | Bound of int
  | Function of string * t list
[@@deriving eq, show { with_path = false }]

let rec replace (old_term, new_term) term =
  if term = old_term then new_term
  else
    match term with
    | Function (name, args) ->
        Function (name, List.map args ~f:(replace (old_term, new_term)))
    | _ -> term
