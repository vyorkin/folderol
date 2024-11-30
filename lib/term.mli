type t =
  | Var of string  (** Meta-variable. *)
  | Param of string * string list
      (** Parameters like [b[?a1, ..., ?an]].
      Each parameter has a name and a list of variables that represent provisos of quantifier rules. *)
  | Bound of int  (** Bound variable. *)
  | Function of string * t list  (** Function application. *)
[@@deriving eq, show]

val replace : t * t -> t -> t
