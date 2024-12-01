type t =
  | Var of string  (** Meta-variable. *)
  | Param of string * string list
      (** Parameters like [b[?a1, ..., ?an]].
      Each parameter has a name and a list of variables that represent provisos of quantifier rules. *)
  | Bound of int  (** Bound variable. *)
  | Function of string * t list  (** Function application. *)
[@@deriving eq, show]

val replace : t * t -> t -> t
(** Traverses the given term recursively
    and replaces all occurrences of [old_term] with [new_term]. 

    @param old_term The term to be replaced.
    @param new_term The term to replace [old_term] with.
    @param term The term to traverse and modify.

    If [old_term] is not found in [term],
    the original term is returned unmodified. *)
