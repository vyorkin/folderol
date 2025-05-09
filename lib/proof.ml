open Core
open Core.Result.Let_syntax

let goal_table = ref (Goal_table.empty ())
let init table = goal_table := table
let clear () = init (Goal_table.empty ())

let print_step rule goal_entry formulas =
  let goal_entry_str = Goal_entry.to_string goal_entry in
  let rule_str = Rule.to_string rule in
  match formulas with
  | [] -> print_endline (Printf.sprintf "[%s]\n%s" rule_str goal_entry_str)
  | _ ->
      print_endline
        (Printf.sprintf "[%s]\n%s:\n%s" rule_str goal_entry_str
           (String.concat ~sep:", " (List.map ~f:Formula.to_string formulas)))

let run_step = function
  | [] -> Ok []
  | [] :: _ -> Error "Empty goal"
  | (goal_entry :: goal) :: table ->
      let%bind rule, subgoals = Goal.reduce goal goal_entry in
      let formulas, table' = Goal_table.insert_goals table (subgoals, []) in
      print_step rule goal_entry formulas;
      Ok table'

let rec run_steps table n =
  match (table, n) with
  | [], _ -> Ok []
  | _, 0 -> Ok table
  | _, i ->
      let%bind table' = run_step table in
      run_steps table' (i - 1)

let step () =
  let%bind table = run_step !goal_table in
  goal_table := table;
  Ok table

let steps n =
  let%bind table = run_steps !goal_table n in
  goal_table := table;
  Ok table

let print_goal_table () = print_endline (Goal_table.to_string !goal_table)
