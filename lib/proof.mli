val default_depth_limit : int
(** Default maximum number of steps before search stops. *)

val init : Goal_table.t -> unit
val clear : unit -> unit
val step : unit -> (Goal_table.t, string) result
val steps : int -> (Goal_table.t, string) result

val apply_rule : Rule.t -> Goal_table.t -> (Goal_table.t, string) result
(** Apply a specific rule to the current goal table. Finds the first goal entry
    that reduces with the given rule and applies it. *)

val run : ?limit:int -> unit -> (Goal_table.t, string) result
(** Run proof search with backtracking up to [limit] steps (default:
    [default_depth_limit]). When a branch fails, backtracks to try alternative
    unifier choices. Returns the final goal table. An empty table means the
    proof succeeded. *)

val with_goal_table : (Goal_table.t -> 'a) -> 'a
(** Apply a function to the current goal table. *)

val get_proof_trees : unit -> Proof_tree.t list
(** Returns proof trees built during the last completed proof. *)

type proof_step = { goal : Goal.t; rule : Rule.t; solved : Formula.t list }
(** A single proof step recording what rule was applied and what was solved. *)

val get_proof_trace : unit -> proof_step list
(** Returns the proof trace (list of steps in order of application). *)

val print_step : Rule.t -> Goal_entry.t -> Formula.t list -> unit
(** Print a single proof step. *)

val print_goal_table : unit -> unit

val get_lemma_cache_hits : unit -> int
(** Returns the number of lemma cache hits during the last proof search. *)

val print_proof_trace : unit -> unit
(** Print the full proof trace showing each step with its rule and result. *)
