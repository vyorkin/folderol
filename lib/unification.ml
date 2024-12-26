module Env = struct
  open Base

  type t = (string, Term.t, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let add env (var, term) = Map.add_exn env ~key:var ~data:term
  let find env var = Map.find env var
end

(** Resolves the given term (if it is a variable)
    by repeatedly replacing it with its assignment in the environment [env].

    For example, if [env] contains mappings ?a -> ?b and ?b -> ?c,
    then [chase_var env (Term.Var "a")] will return [Term.Var "c"].

    @param env The environment mapping variables to terms.
    @param term The term to resolve.
    @return The resolved term, or the input term if no resolution is possible. *)
let rec chase_var env = function
  | Term.Var var ->
      let term = Env.find env var in
      Option.fold ~none:(Term.Var var) ~some:(chase_var env) term
  | term -> term

(** Returns [true] if [var] occurs in the given term. *)
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
      if p1 = p2 then unify_terms env (ts, us) else Error "blya"
  | _, _ ->
      Error
        (Printf.sprintf
           "Only atomic formulas (predicates) are supported. Given: %s and %s"
           (Formula.to_string f1) (Formula.to_string f2))

(** Unifies two lists of terms: [ts] and [us].
    @return The modified [env]. *)
and unify_terms env (ts, us) =
  let open Base.Result.Let_syntax in
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
        Error (Printf.sprintf "Occurs check failed for variable: %s" v)
      else Ok (Env.add env (v, t))
  | t, u -> unify_term env (t, u)

let%test "chase_var: nonexistent variable" =
  let env = Env.empty in
  let var = Term.Var "x" in
  chase_var env var = var

let%test "chase_var: basic variable resolution" =
  let env = Env.add Env.empty ("x", Term.Function ("f", [])) in
  let term = chase_var env (Term.Var "x") in
  term = Term.Function ("f", [])

let%test "chase_var: chained variable resolution" =
  let env = Env.add Env.empty ("x", Term.Function ("f", [])) in
  let env = Env.add env ("y", Term.Var "x") in
  let term = chase_var env (Term.Var "y") in
  term = Term.Function ("f", [])

let%test "chase_var: non-variable input" =
  let env = Env.empty in
  let non_var_term = Term.Function ("g", [ Term.Var "z" ]) in
  chase_var env non_var_term = non_var_term

let%test "occurs_in: variable does not occur" =
  let env = Env.empty in
  let term = Term.Function ("f", [ Term.Param ("p", [ "y" ]); Term.Var "z" ]) in
  occurs_in env "x" term = false

let%test "occurs_in: variable occurs directly" =
  let env = Env.empty in
  let term = Term.Function ("f", [ Term.Var "x"; Term.Var "y" ]) in
  occurs_in env "x" term

let%test "occurs_in: variable occurs in Param" =
  let env = Env.empty in
  let term = Term.Param ("p", [ "x"; "z" ]) in
  occurs_in env "x" term

let%test "occurs_in: variable does not occur in Param" =
  let env = Env.empty in
  let term = Term.Param ("p", [ "y"; "z" ]) in
  occurs_in env "x" term = false

let%test "occurs_in: variable occurs in nested Function" =
  let env = Env.empty in
  let term =
    Term.Function ("f", [ Term.Function ("g", [ Term.Var "x" ]); Term.Var "y" ])
  in
  occurs_in env "x" term

let%test "occurs_in: variable does not occur in nested Function" =
  let env = Env.empty in
  let term =
    Term.Function ("f", [ Term.Function ("g", [ Term.Var "z" ]); Term.Var "y" ])
  in
  occurs_in env "x" term = false

let%test "occurs_in: variable does not occur in empty Function" =
  let env = Env.empty in
  let term = Term.Function ("f", []) in
  occurs_in env "x" term = false

let%test "unify: successful predicate unification" =
  (* P(x, p(a)) = P(f(), p(a))
     expected: x -> f() *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Var "x"; Term.Param ("p", [ "a" ]) ]) in
  let f2 =
    Formula.Pred ("P", [ Term.Function ("f", []); Term.Param ("p", [ "a" ]) ])
  in
  match unify env (f1, f2) with
  | Ok env' ->
      let resolved = chase_var env' (Term.Var "x") in
      resolved = Term.Function ("f", [])
  | Error _ -> false

let%test "unify: mismatched predicate names" =
  (* P(x) != Q(f())
     expected: Error *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Var "x" ]) in
  let f2 = Formula.Pred ("Q", [ Term.Function ("f", []) ]) in
  match unify env (f1, f2) with Ok _ -> false | Error _ -> true

let%test "unify: occurs check failure" =
  (* P(x) = P(f(x))
     expected: Error "Occurs check failed" *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Var "x" ]) in
  let f2 = Formula.Pred ("P", [ Term.Function ("f", [ Term.Var "x" ]) ]) in
  match unify env (f1, f2) with
  | Ok _ -> false
  | Error msg -> String.starts_with msg ~prefix:"Occurs check failed"

let%test "unify: mismatched term lists length" =
  (* P(x, y) != P(f())
     expected: Error *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Var "x"; Term.Var "y" ]) in
  let f2 = Formula.Pred ("P", [ Term.Function ("f", []) ]) in
  match unify env (f1, f2) with Ok _ -> false | Error _ -> true

let%test "unify: nested function unification" =
  (* P(f(x)) = P(f(g()))
     expected: x -> g() *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Function ("f", [ Term.Var "x" ]) ]) in
  let f2 =
    Formula.Pred ("P", [ Term.Function ("f", [ Term.Function ("g", []) ]) ])
  in
  match unify env (f1, f2) with
  | Ok env' ->
      let resolved = chase_var env' (Term.Var "x") in
      resolved = Term.Function ("g", [])
  | Error _ -> false
