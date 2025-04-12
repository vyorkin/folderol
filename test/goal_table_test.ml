open FolderolLib

(* testables *)

let env_testable = Alcotest.testable Env.pp Env.equal
let term_testable = Alcotest.testable Term.pp Term.equal
let formula_testable = Alcotest.testable Formula.pp Formula.equal
let side_testable = Alcotest.testable Formula.pp_side Formula.equal_side
let goal_entry_testable = Alcotest.(triple int side_testable formula_testable)
let goal_testable = Alcotest.(list goal_entry_testable)
let goal_table_testable = Alcotest.(list goal_testable)

(* insert_goals *)

let test_insert_goals_solvable () =
  let open Term in
  let open Formula in
  let initial_goal_table = [] in
  let goals =
    [
      [
        (1, L, Pred ("P", [ Var "x" ]));
        (* |- *)
        (2, R, Pred ("P", [ Bound 42 ]));
      ];
    ]
  in
  let final_fs, final_goal_table =
    Goal_table.insert_goals initial_goal_table (goals, [])
  in
  let expected_fs = [ Pred ("P", [ Bound 42 ]) ] in
  let expected_goal_table = [] in
  Alcotest.(check (list formula_testable))
    "insert_goals solvable: final_fs" expected_fs final_fs;
  Alcotest.(check goal_table_testable)
    "insert_goals solvable: final_goal_table" expected_goal_table
    final_goal_table

let test_insert_goals_unsolvable () =
  let open Term in
  let open Formula in
  let initial_goal_table = [] in
  let goals =
    [
      [
        (1, L, Pred ("P", [ Var "x" ]));
        (* |- *)
        (2, R, Pred ("Q", [ Bound 42 ]));
        (* ^^^ different predicates *)
      ];
    ]
  in
  let final_fs, final_goal_table =
    Goal_table.insert_goals initial_goal_table (goals, [])
  in
  let expected_fs = [] in
  let expected_goal_table = goals in
  Alcotest.(check (list formula_testable))
    "insert_goals unsolvable: final_fs" expected_fs final_fs;
  Alcotest.(check goal_table_testable)
    "insert_goals unsolvable: final_goal_table" expected_goal_table
    final_goal_table

let test_insert_goals_mixed () =
  let open Term in
  let open Formula in
  let initial_goal_table = [] in
  let goals =
    [
      [
        (1, L, Pred ("P", [ Var "x" ]));
        (* |- *)
        (2, R, Pred ("P", [ Bound 42 ]));
        (* solvable *)
      ];
      [
        (1, L, Pred ("Q", [ Var "y" ]));
        (* |- *)
        (2, R, Pred ("R", [ Bound 43 ]));
        (* unsolvable *)
      ];
    ]
  in
  let final_fs, final_goal_table =
    Goal_table.insert_goals initial_goal_table (goals, [])
  in
  let expected_fs = [ Pred ("P", [ Bound 42 ]) ] in
  let expected_goal_table =
    [
      [
        (1, L, Pred ("Q", [ Var "y" ]));
        (* |- *)
        (2, R, Pred ("R", [ Bound 43 ]));
      ];
    ]
  in
  Alcotest.(check (list formula_testable))
    "insert_goals mixed: final_fs" expected_fs final_fs;
  Alcotest.(check goal_table_testable)
    "insert_goals mixed: final_goal_table" expected_goal_table final_goal_table

let test_insert_goals_nested () =
  let open Term in
  let open Formula in
  let initial_goal_table = [] in
  let goals =
    [
      [
        (1, L, Pred ("P", [ Function ("f", [ Var "x" ]) ]));
        (* |- *)
        (2, R, Pred ("P", [ Function ("f", [ Bound 42 ]) ]));
      ];
    ]
  in
  let final_fs, final_goal_table =
    Goal_table.insert_goals initial_goal_table (goals, [])
  in
  let expected_fs = [ Pred ("P", [ Function ("f", [ Bound 42 ]) ]) ] in
  let expected_goal_table = [] in
  Alcotest.(check (list formula_testable))
    "insert_goals nested: final_fs" expected_fs final_fs;
  Alcotest.(check goal_table_testable)
    "insert_goals nested: final_goal_table" expected_goal_table final_goal_table
