open Formula

type t = cost * side * Formula.t [@@deriving eq, show]

val mk : side -> Formula.t -> t
val formula : t -> Formula.t
val pp_debug : Format.formatter -> t -> unit

val pp : Format.formatter -> t -> unit
(** Prints a goal entry using the given [fmt] formatter. *)

val to_string : t -> string
(** Prints a goal entry. *)
