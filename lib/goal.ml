module List = Core.List

type sided_formula = Formula.side * Formula.t
type t = Goal_entry.t list

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

let mk goal sided_formulas =
  sided_formulas
  |> List.map ~f:Formula.add_estimation
  |> List.fold_left ~init:goal ~f:(fun acc formula ->
         insert_goal_entry_early (formula, acc))

let mk_list goal = List.map ~f:(mk goal)

(* SML version from the folderol paper *)

(* fun accum_goal f ([], bs) = bs *)
(*   | accum_goal f ((_,_,A)::G, bs) = accum_goal f (G, f(A,bs)); *)

(* [accum_goal] in the original paper *)
let rec fold_formulas ~f ~init =
  List.fold_left ~f:(fun acc (_, _, formula) -> f acc formula) ~init

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
      If not reductions are possible, indicating that all formulas are atomic.
*)

let reduce goal _sideed_formula =
  let _mk_goals = mk_list goal in
  (* let vars_in  *)
  failwith "blya"

let variable_names ~init =
  fold_formulas ~f:(fun acc f -> Formula.variable_names ~init:acc f) ~init
