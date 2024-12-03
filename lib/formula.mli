type connective = Conj | Disj | Impl | Iff | Not [@@deriving eq, show]
type quantifier = Forall | Exists [@@deriving eq, show]

type t =
  | Pred of string * Term.t list
      (** Atomic formulas (predicates): [P(t1, ..., tn)].
      Represented by the name and the list of arguments (terms). *)
  | Conn of connective * t list
      (** Connectives like [A & B], [A | B], [A -> B], [A <-> B], [!B] and so on. 
      Represented by the connective and the list of its subformulas. *)
  | Quant of quantifier * string * t
      (** Quantified formulae like [\exists x.A] and [\forall x.A].
      Represented by the type of quantifier,
      the name of bound variable (used only for printing) and the body. *)
[@@deriving eq, show]

val pp_formula : Format.formatter -> t -> unit
(** 
  Prints a formula using the given [fmt] formatter.
  
  @param fmt The formatter used to print the formula.
  @param formula The formula to format.
*)

val formula_to_string : t -> string
(** 
  Prints a formula.

  @param formula The formula to convert to a string.
  @return A string representing the formula.
*)

val abstract : Term.t -> t -> t
(** Replaces occurences of [Term.t] by a bound variable. *)

val subst_bound_var : Term.t -> t -> t
(** Replaces occurences of a bound variable by [Term.t]. *)
