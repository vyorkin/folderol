open FolderolLib

(* testables *)

let env_testable = Alcotest.testable Env.pp Env.equal
let term_testable = Alcotest.testable Term.pp Term.equal
let formula_testable = Alcotest.testable Formula.pp Formula.equal
let side_testable = Alcotest.testable Formula.pp_side Formula.equal_side
let goal_entry_testable = Alcotest.(triple int side_testable formula_testable)
let goal_testable = Alcotest.(list goal_entry_testable)
let goal_table_testable = Alcotest.(list goal_testable)

(* accumulate *)

(* TODO: add tests for the accumulate function *)

(* split *)

let test_split_empty_goal () =
  let goal = [] in
  let ls, rs = Goal.split goal in
  Alcotest.(check (list formula_testable))
    "split: handles empty goal (left)" [] ls;
  Alcotest.(check (list formula_testable))
    "split: handles empty goal (right)" [] rs

let test_split_mixed_goal_entries () =
  let open Formula in
  let open Term in
  let goal =
    [
      (1, L, Pred ("P", [ Var "x" ]));
      (2, R, Pred ("Q", [ Var "y" ]));
      (3, L, Pred ("R", [ Var "z" ]));
      (4, R, Pred ("S", [ Var "w" ]));
      (5, L, Pred ("T", [ Var "u" ]));
    ]
  in
  let ls, rs = Goal.split goal in
  Alcotest.(check (list formula_testable))
    "split: handles mixed goal entries (left)"
    [
      Pred ("P", [ Term.Var "x" ]);
      Pred ("R", [ Term.Var "z" ]);
      Pred ("T", [ Term.Var "u" ]);
    ]
    ls;
  Alcotest.(check (list formula_testable))
    "split: handles mixed goal entries (right)"
    [ Pred ("Q", [ Term.Var "y" ]); Pred ("S", [ Term.Var "w" ]) ]
    rs

(* solve *)

let test_solve_basic_unification () =
  let open Term in
  let open Formula in
  let goal =
    [
      (1, L, Pred ("P", [ Var "x" ])); (* |- *) (2, R, Pred ("P", [ Bound 42 ]));
    ]
  in
  let result = Goal.solve goal in
  let expected_unifier = Env.mk [ ("x", Bound 42) ] in
  let expected = [ (Formula.Pred ("P", [ Var "x" ]), expected_unifier) ] in
  Alcotest.(check (list (pair formula_testable env_testable)))
    "solve: unifies simple predicates" expected result

let test_solve_no_unification () =
  let open Term in
  let open Formula in
  let goal =
    [
      (1, L, Pred ("P", [ Var "x" ]));
      (* |- *)
      (2, R, Pred ("Q", [ Bound 42 ]));
      (* ^^^ different predicate *)
    ]
  in
  let result = Goal.solve goal in
  Alcotest.(check (list (pair formula_testable env_testable)))
    "solve: returns empty list if no unification is possible" [] result

let test_solve_multiple_unification () =
  let open Term in
  let open Formula in
  let goal =
    [
      (1, L, Pred ("P", [ Var "x" ]));
      (* |- *)
      (2, R, Pred ("P", [ Bound 42 ]));
      (3, R, Pred ("P", [ Bound 43 ]));
    ]
  in
  let result = Goal.solve goal in
  let expected_unifier = Env.mk [ ("x", Bound 42) ] in
  let expected = [ (Pred ("P", [ Var "x" ]), expected_unifier) ] in
  Alcotest.(check (list (pair formula_testable env_testable)))
    "solve: handles multiple unification possibilities" expected result

let test_solve_nested_terms () =
  let open Formula in
  let open Term in
  let goal =
    [
      (1, L, Pred ("P", [ Function ("f", [ Var "x" ]) ]));
      (* |- *)
      (2, R, Pred ("P", [ Function ("f", [ Bound 42 ]) ]));
    ]
  in
  let result = Goal.solve goal in
  let expected_unifier = Env.mk [ ("x", Bound 42) ] in
  let expected =
    [ (Pred ("P", [ Function ("f", [ Var "x" ]) ]), expected_unifier) ]
  in
  Alcotest.(check (list (pair formula_testable env_testable)))
    "solve: works with nested terms" expected result
