open Unification

(** Γ |- ∆ *)
type side = L | R [@@deriving eq, show]

type cost = int
type goal_entry = cost * side * Formula.t
type goal = goal_entry list

type goal_table = goal list
(** Proof state *)

val inst_term : Env.t -> Term.t -> Term.t
(** Recursively applies substitutions to a term based on the given environment
    [env]. The environment maps meta-variable names to their substituted terms.
    This function performs delayed substitution, ensuring efficiency by avoiding
    repeated rewriting of large terms during unification.

    - If the term is a function ([Term.Function]), its arguments are recursively
      instantiated.
    - If the term is a parameter ([Term.Param]), its variable names are replaced
      with their corresponding terms from the environment, collecting all
      variable names in the process.
    - If the term is a variable ([Term.Var]), it is replaced by its
      corresponding term from the environment, or left unchanged if no
      substitution exists.
    - Any other term is returned unchanged. *)

val inst_formula : Env.t -> Formula.t -> Formula.t
(** Recursively applies substitutions to a formula based on the given
    environment [env]. This function ensures that substitutions are propagated
    to all terms and subformulas within the formula.

    - If the formula is a predicate ([Formula.Pred]), its terms are recursively
      instantiated.
    - If the formula is a logical connective ([Formula.Conn]), all its
      subformulas are recursively instantiated.
    - If the formula is a quantifier ([Formula.Quant]), the quantifier's body is
      recursively instantiated while leaving the bound variable unchanged. *)

val inst_goal : Env.t -> goal -> goal
(** Applies substitutions to a single goal, where a goal is represented as a
    tuple [(cost, side, formula)]. The formula within the goal is recursively
    instantiated based on the environment [env]. *)

val inst_goals : Env.t -> goal list -> goal list
(** Applies substitutions to a list of goals. Each goal in the list is processed
    using [inst_goal env], ensuring all formulas in all goals are instantiated
    with the substitutions recorded in the environment [env]. *)

val split_goal : goal -> Formula.t list * Formula.t list
(** Splits a goal represented as a list of triples [cost * side * Formula.t]
    into two lists of atomic formulas:
    - a list of left-side (Γ) formulas ([A1, ..., Am])
    - a list of right-side (∆) formulas ([B1, ..., Bn])

    Reverses the input list before splitting to ensure the output lists are in
    the correct order. This design favors newer formulas over older ones to
    reduce looping during unification, as Folderol prioritizes unifying atomic
    formulas over complex ones.

    @param goal A list of triples representing the goal.
    @return
      A tuple [(ls, rs)] where:
      - [ls] contains all atomic formulas from the left side ([L]).
      - [rs] contains all atomic formulas from the right side ([R]). *)

val solve_goal : goal -> (Formula.t * unifier) list
(** Attempts to solve the given [goal] by iterating over atomic formulas from
    its left-hand side (∆) and right-hand side (Γ).

    The process involves:
    - Splitting the goal into left and right formulas.
    - Filtering for atomic formulas on both sides.
    - For each atomic formula on the left, finding a unifiable counterpart on
      the right.

    If a pair is unifiable, it returns a list containing the atomic formula and
    its unifier. If no unifiable pairs are found, it tries the next left
    formula. If all pairs fail, the function returns an empty list.

    The function prioritizes finding the first solution, returning as soon as a
    unifier is identified. *)

val insert_goals :
  goal_table -> goal list * Formula.t list -> Formula.t list * goal_table
(** Tries to solve each goal from the [goal list * Formula.t]. After solving the
    goal it instantiates all the other goals with the resulting env (unifier),
    since its variables may appear in other goals. *)

val cost : side * Formula.t -> cost
(** Cost of ∀L and ∃R is 3. The cost of other rules is the number of premises.
*)

val add_estimation : side * Formula.t -> cost * side * Formula.t
(** Estimates formula cost and attaches it. *)

val insert_goal_entry :
  less:(goal_entry * goal_entry -> bool) -> goal_entry * goal -> goal
(** The entries in a goal are ordered by cost - the first entry is the cheapest.
    This function helps to maintain goal_entry's in order.

    @param less
      Comparison function: [<] to place the new entry first among entries of
      equal cost, [<=] to place it at the end. *)

val insert_goal_entry_early : goal_entry * goal -> goal
(** Inserts a new [goal_entry] by placing it first among entries of equal cost.
*)

val insert_goal_entry_late : goal_entry * goal -> goal
(** Inserts a new [goal_entry] by placing it at the end of entries of equal
    cost. *)

val new_goal : goal -> (side * Formula.t) list -> goal
val new_goals : goal -> (side * Formula.t) list list -> goal list

val reduce_goal : goal -> side * Formula.t -> goal
(** Reduces goal. *)
