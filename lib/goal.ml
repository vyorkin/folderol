open Pretty_printing
module List = Core.List

type sided_formula = Formula.side * Formula.t
type t = Goal_entry.t list

let mk entries = entries
let unify env (f1, f2) = Unification.unify env (f1, f2) |> Result.to_option

(* insert 1 into [1, 3, 0, 1]:
          ^       ^
          x       y

   [<, 1]: insert_early
       ^
       y

    match 1, 1 :: 3 :: 0 :: 1 ->
    if 1 <  1 then 1 :: insert (<) (1, 3 :: 0 :: 1) else 1 :: 1 :: 3 :: 0 :: 1
       ^    ^      ^                ^                    ^    ^
       y <  x      y                x                    x    y
                                                        \--------------------/

   [<=, 1]: insert_late
        ^
        y

    match 1, 1 :: 3 :: 0 :: 1 ->
    if 1 <= 1 then 1 :: insert (<=) (1, 3 :: 0 :: 1) else 1 :: 1 :: 3 :: 0 :: 1
       ^    ^      ^                 ^                    ^    ^
       y <= x      y                 x                    x    y
                  \-------------------------------/
   *)

(*
let rec insert_goal_entry ~less = function
  | x, [] -> [ x ]
  | x, y :: ys ->
      if less (y, x) then y :: insert_goal_entry ~less (x, ys) else x :: y :: ys
*)

let rec insert_goal_entry ~less (x, goal) =
  match goal with
  | [] -> [ x ]
  | y :: ys when less (y, x) -> y :: insert_goal_entry ~less (x, ys)
  | _ -> x :: goal

let goal_entry_less ((cost0, _, _), (cost1, _, _)) = cost0 < cost1
let goal_entry_less_or_eq ((cost0, _, _), (cost1, _, _)) = cost0 <= cost1
let insert_goal_entry_early = insert_goal_entry ~less:goal_entry_less
let insert_goal_entry_late = insert_goal_entry ~less:goal_entry_less_or_eq

let mk_subgoal goal sided_formulas =
  sided_formulas
  |> List.map ~f:Formula.add_estimation
  |> List.fold_right ~init:goal ~f:(fun formula acc ->
         insert_goal_entry_early (formula, acc))

(** Creates a list of subgoals from a goal (list of tuples
    [cost * side * formula]) and a list of new pairs [side * formula]. *)
let mk_subgoals goal = List.map ~f:(mk_subgoal goal)

(* SML version from the folderol paper *)

(* fun accum_goal f ([], bs) = bs *)
(*   | accum_goal f ((_,_,A)::G, bs) = accum_goal f (G, f(A,bs)); *)

(* [accum_goal] in the original paper *)
let fold_left ~f ~init entries =
  entries |> List.map ~f:Goal_entry.formula |> List.fold_left ~f ~init

let split goal =
  let open Formula in
  let rec split' (ls, rs) = function
    | [] -> (ls, rs)
    | (_, L, x) :: goal -> split' (x :: ls, rs) goal
    | (_, R, y) :: goal -> split' (ls, y :: rs) goal
  in
  goal |> List.rev |> split' ([], [])

let solve goal =
  let filter_pred = List.filter ~f:Formula.is_pred in
  let lfs, rfs = split goal in
  let lps, rps = (filter_pred lfs, filter_pred rfs) in
  let rec solve' = function
    | [], _ -> []
    | lp :: lps', rps ->
        let rec find_unifier = function
          | [] -> solve' (lps', rps)
          | rp :: rps' -> (
              match unify Env.empty (lp, rp) with
              | Some u -> [ (lp, u) ] (* return formula and the unifier *)
              | None -> find_unifier rps' (* keep searching in ∆ *))
        in
        find_unifier rps
  in
  solve' (lps, rps)

(** For rules [\forall R] and [\exists L] it generates a fresh parameter and
    attaches all the variables in the goal.

    For rules [\forall L] and [\exists R] it generates a fresh variable. The
    subgoal contains the original entry inserted "late" and the new entry
    inserted "early".

    @raise Error
      If no reductions are possible, indicating that all formulas are atomic. *)

let variable_names ~init =
  fold_left ~f:(fun acc f -> Formula.variable_names ~init:acc f) ~init

let reduce goal entry =
  (* We assume all formulas in [goal] or [entry] are [abstract]'ed.
     No formula contains free variables: 
     all variables are bound and all parameters are unbound. *)
  let push_subgoals = mk_subgoals goal in
  let vars_in formula =
    let init = Formula.variable_names ~init:[] formula in
    variable_names ~init goal
  in
  (* When applying the ∀R or ∃L rule we should replace the
     bound variables (like x, y, z) with fresh parameters (like a, b, c) *)
  let subst_bound_var_with_param formula =
    Formula.subst_bound_var (Term.Param (Symbol.mk (), vars_in formula)) formula
  in
  let subst_bound_var_with_var formula =
    Formula.subst_bound_var (Term.Var (Symbol.mk ())) formula
  in
  let reduce_goal ((_, side, formula) as entry) =
    let open Formula in
    (* This function adds (push) subtheorems (subgoals) whose truth is required to 
       prove the main theorem using sequent calculus rules from Gentzen's LK.
       Note that in comments below:
       [ f0, f1, ..., fn |- ] ~ [ f0 |-, f1 |- , ..., fn |- ]
       and
       [ f0 |-, |- f1 ] ~ [ f0 |- f1 ] *)
    match (side, formula) with
    (* (¬R): |- ¬f ==> [ f |- ] *)
    | R, Conn (Not, [ f ]) -> Ok (Rule.NotR, push_subgoals [ [ (L, f) ] ])
    (* (¬L): ¬f |- ==> [ |- f ] *)
    | L, Conn (Not, [ f ]) -> Ok (Rule.NotL, push_subgoals [ [ (R, f) ] ])
    (* (∧R): |- f0 ∧ f1 ==> [ |- f0 ], [ |- f1 ] *)
    | R, Conn (Conj, [ f0; f1 ]) ->
        Ok (Rule.ConjR, push_subgoals [ [ (R, f0) ]; [ (R, f1) ] ])
    (* (∧L): f0 ∧ f1 |- ==> [ f0, f1 |- ] *)
    | L, Conn (Conj, [ f0; f1 ]) ->
        Ok (Rule.ConjL, push_subgoals [ [ (L, f0); (L, f1) ] ])
    (* (∨R): |- f0 ∨ f1 ==> [ |- f0, f1 ] *)
    | R, Conn (Disj, [ f0; f1 ]) ->
        Ok (Rule.DisjR, push_subgoals [ [ (R, f0); (R, f1) ] ])
    (* (∨L): f0 ∨ f1 |- ==> [ f0 |- ], [ f1 |- ] *)
    | L, Conn (Disj, [ f0; f1 ]) ->
        Ok (Rule.DisjL, push_subgoals [ [ (L, f0) ]; [ (L, f1) ] ])
    (* (→R): |- f0 → f1 ==> [ f0 |- f1 ] *)
    | R, Conn (Impl, [ f0; f1 ]) ->
        Ok (Rule.ImplR, push_subgoals [ [ (L, f0); (R, f1) ] ])
    (* (→L): f0 → f1 |- ==> [ |- f0 ], [ f1 |- ] *)
    | L, Conn (Impl, [ f0; f1 ]) ->
        Ok (Rule.ImplL, push_subgoals [ [ (R, f0) ]; [ (L, f1) ] ])
    (* (↔R): |- f0 ↔ f1 ==> [ f0 |- f1 ], [ f1 |- f0 ] *)
    | R, Conn (Iff, [ f0; f1 ]) ->
        Ok
          ( Rule.IffR,
            push_subgoals [ [ (L, f0); (R, f1) ]; [ (R, f0); (L, f1) ] ] )
    (* (↔L): f0 ↔ f1 |- ==> [ f0, f1 |- ], [ |- f0, f1 ] *)
    | L, Conn (Iff, [ f0; f1 ]) ->
        Ok
          ( Rule.IffL,
            push_subgoals [ [ (L, f0); (L, f1) ]; [ (R, f0); (R, f1) ] ] )
    (* (∀R) *)
    | R, Quant (Forall, _, f) ->
        Ok
          (Rule.ForallR, push_subgoals [ [ (R, subst_bound_var_with_param f) ] ])
    (* (∀L) *)
    | L, Quant (Forall, _, f) ->
        Ok
          ( Rule.ForallL,
            [
              (* A copy of ∀x.A is retained in the subgoal so that 
               the rule can be applied again with different terms. *)
              insert_goal_entry_early
                ( add_estimation (L, subst_bound_var_with_var f),
                  insert_goal_entry_late (entry, goal) );
            ] )
    (* (∃R) *)
    | R, Quant (Exists, _, f) ->
        Ok
          ( Rule.ExistsR,
            [
              (* The rule ∃R similarly retains the quantified formula in its subgoal. *)
              insert_goal_entry_early
                ( add_estimation (R, subst_bound_var_with_var f),
                  insert_goal_entry_late (entry, goal) );
            ] )
    (* (∃L) *)
    | L, Quant (Exists, _, f) ->
        Ok
          (Rule.ExistsL, push_subgoals [ [ (L, subst_bound_var_with_param f) ] ])
    | _ ->
        Error
          (Printf.sprintf "Reduce failed: %s %s" (Formula.show_side side)
             (Formula.to_string formula))
  in
  reduce_goal entry

let pp_goal_entries fmt goal =
  let open Format in
  fprintf fmt "@[<v>";
  List.iter goal ~f:(fprintf fmt "%a@," Goal_entry.pp);
  fprintf fmt "@]"

let pp fmt goal =
  let open Format in
  let gamma, delta = split goal in
  fprintf fmt "@[<h>";
  let pp_goal_formulas fmt formulas =
    if not (List.is_empty formulas) then
      pp_print_list ~pp_sep:pp_comma Formula.pp_formula fmt formulas
  in
  pp_goal_formulas fmt gamma;
  if not (List.is_empty gamma) then fprintf fmt " ";
  fprintf fmt "|-";
  if not (List.is_empty delta) then fprintf fmt " ";
  pp_goal_formulas fmt delta;
  fprintf fmt "@]"

let to_string = format_to_string pp
