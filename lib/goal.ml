module List = Core.List

type t = Goal_entry.t list

let unify env (f1, f2) = Unification.unify env (f1, f2) |> Result.to_option

(* insert 1 into [1, 3, 0, 1]:
          ^       ^
          y       x

   [<, 1]: insert_late
    match 1, 1 :: 3 :: 0 :: 1 ->
    if 1 <  1 then 1 :: insert (<) (1, 3 :: 0 :: 1) else 1 :: 1 :: 3 :: 0 :: 1
                   ^                ^                    ^    ^
                   y                x                    x    y
                                                        \--------------------/

   [<=, 1]: insert_early
    match 1, 1 :: 3 :: 0 :: 1 ->
    if 1 <= 1 then 1 :: insert (<) (1, 3 :: 0 :: 1) else 1 :: 1 :: 3 :: 0 :: 1
                   ^                ^                    ^    ^
                   y                x                    x    y
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

let new_goal goal formulas =
  formulas
  |> List.map ~f:Formula.add_estimation
  |> List.fold_left ~init:goal ~f:(fun acc formula ->
         insert_goal_entry_early (formula, acc))

let new_goals goal = List.map ~f:(new_goal goal)

let rec accumulate f (goal, formulas) =
  List.fold_left goal ~init:formulas ~f:(fun acc (_, _, formula) ->
      f acc formula)

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

let reduce goal formula = failwith "hooy"
(*   let goals = new_goals g in *)
