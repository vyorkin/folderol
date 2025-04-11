open Unification

type t = Goal_entry.t list

val insert_goal_entry :
  less:(Goal_entry.t * Goal_entry.t -> bool) -> Goal_entry.t * t -> t
(** The entries in a goal are ordered by cost - the first entry is the cheapest.
    This function helps to maintain goal_entry's in order.

    @param less
      Comparison function: [<] to place the new entry first among entries of
      equal cost, [<=] to place it at the end. *)

val insert_goal_entry_early : Goal_entry.t * t -> t
(** Inserts a new [goal_entry] by placing it first among entries of equal cost.
*)

val insert_goal_entry_late : Goal_entry.t * t -> t
(** Inserts a new [goal_entry] by placing it at the end of entries of equal
    cost. *)

val new_goal : t -> (Formula.side * Formula.t) list -> t
val new_goals : t -> (Formula.side * Formula.t) list list -> t list

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

val reduce : t -> Formula.side * Formula.t -> t
(** Reduces goal. *)
