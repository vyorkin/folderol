val instantiate_term : Env.t -> Term.t -> Term.t
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

val instantiate_formula : Env.t -> Formula.t -> Formula.t
(** Recursively applies substitutions to a formula based on the given
    environment [env]. This function ensures that substitutions are propagated
    to all terms and subformulas within the formula.

    - If the formula is a predicate ([Formula.Pred]), its terms are recursively
      instantiated.
    - If the formula is a logical connective ([Formula.Conn]), all its
      subformulas are recursively instantiated.
    - If the formula is a quantifier ([Formula.Quant]), the quantifier's body is
      recursively instantiated while leaving the bound variable unchanged. *)

val instantiate_goal : Env.t -> Goal.t -> Goal.t
(** Applies substitutions to a single goal, where a goal is represented as a
    tuple [(cost, side, formula)]. The formula within the goal is recursively
    instantiated based on the environment [env]. *)

val instantiate_goals : Env.t -> Goal.t list -> Goal.t list
(** Applies substitutions to a list of goals. Each goal in the list is processed
    using [instantiate env], ensuring all formulas in all goals are instantiated
    with the substitutions in the environment [env]. *)
