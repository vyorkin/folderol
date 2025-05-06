type t = Goal.t list
(** Proof state. *)

val empty : unit -> t
(** Creates empty goal table. *)

val mk : Formula.t list * Formula.t list -> t
(** Given two lists of formulas (Γ and Δ) creates a goal table. *)

val insert_goals : t -> Goal.t list * Formula.t list -> Formula.t list * t
(** Tries to solve each goal from the [goal list * Formula.t]. After solving the
    goal it instantiates all the other goals with the resulting env (unifier),
    since its variables may appear in other goals. *)

val pp : Format.formatter -> t -> unit
(** Prints a goal table using the given [fmt] formatter. *)

val to_string : t -> string
(** Prints a goal table. *)
