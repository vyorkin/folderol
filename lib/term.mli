type t =
  | Var of string  (** Meta-variable. *)
  | Param of string * string list
      (** Parameters like [b[?a1, ..., ?an]]. Each parameter has a name and a
          list of variables that represent provisos of quantifier rules. *)
  | Bound of int  (** Bound variable. *)
  | Function of string * t list  (** Function application. *)
[@@deriving eq, show]

val pp_term : Format.formatter -> t -> unit
(** Prints the term [t] using the format output function [fmt].

    Example usage:

    {[
      pp_term fmt (Var "x");
      pp_term fmt (Param ("f", [ "x"; "y" ]));
      pp_term fmt (Bound 3);
      pp_term fmt (Function ("add", [ Var "x"; Var "y" ]))
    ]}

    @param fmt The formatter to which the formatted term is output.
    @param t The term to be formatted. *)

val to_string : t -> string
(** Prints a term.

    @param term The term to convert to a string.
    @return A string representing the term. *)

val replace : t * t -> t -> t
(** Traverses the given term recursively and replaces all occurrences of
    [old_term] with [new_term].

    @param old_term The term to be replaced.
    @param new_term The term to replace [old_term] with.
    @param term The term to traverse and modify.

    If [old_term] is not found in [term], the original term is returned
    unmodified. *)

val variable_names : string list -> t -> string list
(** Collects distinct variable names in a term. Named [vars_in_term] in the
    original paper. *)
