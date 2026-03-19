type t = Goal.t list
(** Proof state. *)

val empty : unit -> t
(** Creates empty goal table. *)

val is_empty : t -> bool
(** Returns [true] if the goal table has no remaining goals. *)

val mk : Formula.t list * Formula.t list -> t
(** Given two lists of formulas (Γ and Δ) creates a goal table. *)

val insert_goals : t -> Goal.t list * Formula.t list -> Formula.t list * t
(** Tries to solve each goal from the [goal list * Formula.t]. After solving the
    goal it instantiates all the other goals with the resulting env (unifier),
    since its variables may appear in other goals. Uses the first available
    unifier. *)

val insert_goals_all :
  t -> Goal.t list * Formula.t list -> (Formula.t list * t) list
(** Like [insert_goals] but returns all possible outcomes by trying every
    available unifier. Used for backtracking proof search. *)

val pp : Format.formatter -> t -> unit
(** Prints a goal table using the given [fmt] formatter. *)

val to_string : t -> string
(** Prints a goal table. *)
