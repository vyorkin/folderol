type unifier = Env.t

val chase_var : Env.t -> Term.t -> Term.t
(** Resolves the given term (if it is a variable) by repeatedly replacing it
    with its assignment in the environment [env].

    For example, if [env] contains mappings ?a -> ?b and ?b -> ?c, then
    [chase_var env (Term.Var "a")] will return [Term.Var "c"].

    @param env The environment mapping variables to terms.
    @param term The term to resolve.
    @return The resolved term, or the input term if no resolution is possible.
*)

val occurs_in : Env.t -> string -> Term.t -> bool
(** Performs the "Occurs check".
    @return [true] if [var] occurs in the given term. *)

val unify : Env.t -> Formula.t * Formula.t -> (Env.t, string) result
(** Attempts to unify two atomic formulas [f1] and [f2] under the environment
    [env].

    This function only handles atomic formulas, which are predicates of the form
    [P(t1, ..., tn)] and [Q(u1, ..., un)].

    - If the predicates [p1] and [p2] of [f1] and [f2] are the same, it
      delegates to [unify_terms] to unify their terms [ts] and [us].
    - If the predicates [p1] and [p2] differ, it returns an error indicating the
      mismatch.
    - If either [f1] or [f2] is not an atomic formula, it returns an error
      indicating that only predicates are supported.

    @param env The current environment for unification.
    @param f1 The first atomic formula to unify.
    @param f2 The second atomic formula to unify.
    @return
      The updated env if unification succeeds, or an error message if it fails.

    @raise Error
      if the predicates differ or if non-atomic formulas are provided.

    Example:
    {[
      let env = [] in
      let f1 = Formula.Pred ("P", [Term.Var "x"; Term.Const 1]) in
      let f2 = Formula.Pred ("P", [Term.Const 2; Term.Const 1]) in
      match unify env (f1, f2) with
      | Ok new_env -> (* Unification succeeded, work with new_env *)
      | Error msg -> (* Handle unification failure *)
    ]} *)
