open Unification

type sided_formula = Formula.side * Formula.t
(** Aka "sequent component". Represents a single formula in either the
    antecedent (left) or succedent (right) of a sequent Γ ⊢ Δ. *)

type t = Goal_entry.t list
(** A goal is a list of [Goal_entry.t], where each entry is essentially a
    triple: [cost * side * formula].

    The list is kept sorted by [cost]. To maintain this order, we use the
    functions: [insert_goal_entry], [insert_goal_entry_early], and
    [insert_goal_entry_late]. *)

val mk : Goal_entry.t list -> t
(** Creates a new goal. *)

val insert_goal_entry :
  less:(Goal_entry.t * Goal_entry.t -> bool) -> Goal_entry.t * t -> t
(** The entries in a goal are ordered by cost - the first entry is the cheapest.
    This function helps to maintain goal entry's in order.

    @param less
      Comparison function: [<] to place the new entry first among entries of
      equal cost, [<=] to place it at the end. *)

val insert_goal_entry_early : Goal_entry.t * t -> t
(** Inserts a new [goal_entry] by placing it first among entries of equal cost.
*)

val insert_goal_entry_late : Goal_entry.t * t -> t
(** Inserts a new [goal_entry] by placing it at the end of entries of equal
    cost. *)

val mk_subgoal : t -> sided_formula list -> t
(** A rule is applied to the head of this list. The tail holds the remaining
    formulas, which must be included in each subgoal. Each subgoal is made from
    the tail by adding new sequent components: pairs of type [side * formula].

    Calling [Goal.mk_subgoal goal sided_formulas] copies the pairs (sequent
    components) into goal, which is the tail of the goal. It calls
    [add_estimation] to attach a cost to each formula/sequent component. *)

val mk_subgoals : t -> sided_formula list list -> t list
(** Since a rule may produce more than one subgoal, this [Goal.mk_subgoals]
    function creates a list of subgoals from a goal and a list of new
    [side * formula] pairs. *)

val fold_left : f:('accum -> Formula.t -> 'accum) -> init:'accum -> t -> 'accum
(** Folds over all formulas. Named [accum_goal] in the original paper. *)

val split : t -> Formula.t list * Formula.t list
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

val solve : t -> (Formula.t * unifier) list
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

val variable_names : init:string list -> t -> string list
(** Collects distinct variable names in a formula. Named [vars_in_goal] in the
    original paper. *)

val reduce : t -> Goal_entry.t -> (Rule.t * t list, string) result
(** Handles all the rules. Given a formula and its side, it uses the immediate
    subformulas to build subgoals.

    @return A list of subgoals and the rule that was applied. *)

val pp_goal_entries : Format.formatter -> t -> unit

val pp : Format.formatter -> t -> unit
(** Prints a goal using the given [fmt] formatter. *)

val to_string : t -> string
(** Prints a goal. *)
