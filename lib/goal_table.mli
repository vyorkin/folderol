type t = Goal.t list
(** Proof state *)

val insert_goals : t -> Goal.t list * Formula.t list -> Formula.t list * t
(** Tries to solve each goal from the [goal list * Formula.t]. After solving the
    goal it instantiates all the other goals with the resulting env (unifier),
    since its variables may appear in other goals. *)
