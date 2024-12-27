(** Module representing the environment for unification.

    The environment is used to avoid repeatedly substituting meta-variables
    throughout the equations every time a meta-variable is assigned. Instead,
    it maintains a mapping of meta-variable -> term assignments and resolves
    terms on-the-fly.

    Keeping environment allows us to ensure that cycles are 
    prevented using occurs check which disallows assignments like [(?b, f(?a)), (?a, g(?b))]. *)
module Env : sig
  type t
  (** Type representing the environment,
      which maps meta-variables to terms. *)

  val empty : t
  (** Empty environment with no variable assignments. *)

  val add : t -> string * Term.t -> t
  (** Adds an assignment [var] -> [term] to the environment [env].

      @param env The existing environment.
      @param var The variable being assigned.
      @param term The term assigned to [var].
      @return The updated environment.
      @raise [Map.Key_already_present] if [var] is already assigned in [env]. *)

  val find : t -> string -> Term.t option
  (** Looks up the assignment for [var] in [env].

      @param env The environment to search.
      @param var The variable whose assignment is sought.
      @return [Some term] if [var] is assigned to [term], otherwise [None]. *)
end

val unify : Env.t -> Formula.t * Formula.t -> (Env.t, string) result
(** 
  Attempts to unify two atomic formulas [f1] and [f2] under the environment [env]. 

  This function only handles atomic formulas, which are predicates of the form 
  [P(t1, ..., tn)] and [Q(u1, ..., un)].

  - If the predicates [p1] and [p2] of [f1] and [f2] are the same, it delegates 
    to [unify_terms] to unify their terms [ts] and [us].
  - If the predicates [p1] and [p2] differ, it returns an error indicating 
    the mismatch.
  - If either [f1] or [f2] is not an atomic formula, it returns an error 
    indicating that only predicates are supported.

  @param env The current environment for unification.
  @param f1 The first atomic formula to unify.
  @param f2 The second atomic formula to unify.
  @return The updated env if unification succeeds, or an error message if it fails.

  @raise Error if the predicates differ or if non-atomic formulas are provided.

  Example:
  {[
    let env = [] in
    let f1 = Formula.Pred ("P", [Term.Var "x"; Term.Const 1]) in
    let f2 = Formula.Pred ("P", [Term.Const 2; Term.Const 1]) in
    match unify env (f1, f2) with
    | Ok new_env -> (* Unification succeeded, work with new_env *)
    | Error msg -> (* Handle unification failure *)
  ]}
*)
