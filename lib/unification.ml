type unifier = Env.t

let rec chase_var env = function
  | Term.Var var ->
      let term = Env.find env var in
      Option.fold ~none:(Term.Var var) ~some:(chase_var env) term
  | term -> term

let rec occurs_in env var = function
  | Term.Var var' ->
      if var' = var then true
      else
        let term = Env.find env var in
        Option.fold ~none:false ~some:(occurs_in env var) term
  | Term.Param (_, vars) ->
      let terms = List.map (fun s -> Term.Var s) vars in
      occurs_in_any_of env var terms
  | Term.Function (_, terms) -> occurs_in_any_of env var terms
  | _ -> false

(** Returns [true] if [var] occurs in any of the given [terms]. *)
and occurs_in_any_of env var terms =
  let term = List.find_opt (fun term -> occurs_in env var term) terms in
  Option.is_some term

(** Handles only atomic formulas (predicates),
    like [P(t1, ..., tn)] and [Q(u1, ..., un)]. *)
let rec unify env (f1, f2) =
  match (f1, f2) with
  | Formula.Pred (p1, ts), Formula.Pred (p2, us) ->
      if p1 = p2 then unify_terms env (ts, us)
      else
        Error
          (Printf.sprintf "Can not unify different predicates %s and %s" p1 p2)
  | _, _ ->
      Error
        (Printf.sprintf
           "Only atomic formulas (predicates) are supported. Given: %s and %s"
           (Formula.to_string f1) (Formula.to_string f2))

(** Unifies two lists of terms: [ts] and [us].
    @return The modified [env]. *)
and unify_terms env (ts, us) =
  let open Core.Result.Let_syntax in
  match (ts, us) with
  | [], [] -> Ok env
  | t :: ts, u :: us ->
      let%bind env' = unify_term env (t, u) in
      unify_terms env' (ts, us)
  | _, _ -> Error "Lists of terms have different length"

(** Unifies two arbitrary terms [t] and [u].
    @return The modified [env]. *)
and unify_term env (t, u) =
  match (t, u) with
  | Term.Var v, t -> unify_var env (chase_var env (Term.Var v), chase_var env t)
  | t, Term.Var v -> unify_var env (chase_var env t, chase_var env (Term.Var v))
  | Term.Param (a, _), Param (b, _) ->
      if a = b then Ok env
      else
        Error
          (Printf.sprintf "Can not unify different parameters %s and %s" a b)
  | Term.Function (f, ts), Term.Function (g, us) ->
      if f = g then unify_terms env (ts, us)
      else
        Error (Printf.sprintf "Can not unify different functions %s and %s" f g)
  | t, u ->
      Error
        (Printf.sprintf "Can not unify terms %s and %s" (Term.to_string t)
           (Term.to_string u))

(** Unifies a variable term [var] with any other [term].
    @return The modified [env]. *)
and unify_var env (var, term) =
  match (var, term) with
  | Var _, t when var = t -> Ok env
  | Var v, t ->
      if occurs_in env v t then
        Error (Printf.sprintf "Occurs check failed for variable %s" v)
      else Ok (Env.add env (v, t))
  | t, u -> unify_term env (t, u)
