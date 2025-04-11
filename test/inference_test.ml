open FolderolLib
open Inference

(* testables *)

let env_testable = Alcotest.testable Env.pp Env.equal
let term_testable = Alcotest.testable Term.pp Term.equal
let formula_testable = Alcotest.testable Formula.pp Formula.equal
let side_testable = Alcotest.testable pp_side equal_side
let goal_entry_testable = Alcotest.(triple int side_testable formula_testable)
let goal_testable = Alcotest.(list goal_entry_testable)
let goal_table_testable = Alcotest.(list goal_testable)

(* inst_term *)

let test_inst_term_with_no_substitution () =
  let env = Env.empty in
  let term = Term.Function ("f", [ Term.Var "x" ]) in
  Alcotest.(check term_testable)
    "inst_term: no substitution" term (inst_term env term)

let test_inst_term_with_single_substitution () =
  let env = Env.add Env.empty ("x", Term.Function ("g", [])) in
  let term = Term.Var "x" in
  let expected = Term.Function ("g", []) in
  Alcotest.(check term_testable)
    "inst_term: single substitution" expected (inst_term env term)

let test_inst_term_nested_substitution () =
  let env = Env.add Env.empty ("x", Term.Function ("g", [ Term.Var "y" ])) in
  let env = Env.add env ("y", Term.Function ("h", [])) in
  let term = Term.Var "x" in
  let expected = Term.Function ("g", [ Term.Function ("h", []) ]) in
  Alcotest.(check term_testable)
    "inst_term: nested substitution" expected (inst_term env term)

(* inst_formula *)

let test_inst_formula_with_no_substitution () =
  let env = Env.empty in
  let formula = Formula.Pred ("P", [ Term.Var "x"; Term.Function ("f", []) ]) in
  Alcotest.(check formula_testable)
    "inst_formula: no substitution" formula (inst_formula env formula)

let test_inst_formula_with_substitution () =
  let env = Env.add Env.empty ("x", Term.Function ("g", [])) in
  let formula = Formula.Pred ("P", [ Term.Var "x"; Term.Function ("f", []) ]) in
  let expected =
    Formula.Pred ("P", [ Term.Function ("g", []); Term.Function ("f", []) ])
  in
  Alcotest.(check formula_testable)
    "inst_formula: with substitution" expected (inst_formula env formula)

let test_inst_formula_with_nested_substitution () =
  let env = Env.add Env.empty ("x", Term.Function ("g", [ Term.Var "y" ])) in
  let env = Env.add env ("y", Term.Function ("h", [])) in
  let formula = Formula.Pred ("P", [ Term.Var "x" ]) in
  let expected =
    Formula.Pred ("P", [ Term.Function ("g", [ Term.Function ("h", []) ]) ])
  in
  Alcotest.(check formula_testable)
    "inst_formula: nested substitution" expected (inst_formula env formula)

(* inst_goal *)

let test_inst_goal_with_no_substitution () =
  let env = Env.empty in
  let input_goal = [ (0, L, Formula.Pred ("P", [ Term.Var "x" ])) ] in
  let expected_goal = input_goal in
  Alcotest.(check goal_testable)
    "inst_goal: no substitution" expected_goal (inst_goal env input_goal)

let test_inst_goal_single_substitution () =
  let env = Env.add Env.empty ("x", Term.Function ("f", [])) in
  let input_goal = [ (1, R, Formula.Pred ("P", [ Term.Var "x" ])) ] in
  let expected_goal =
    [ (1, R, Formula.Pred ("P", [ Term.Function ("f", []) ])) ]
  in
  Alcotest.(check goal_testable)
    "inst_goal: single substitution" expected_goal (inst_goal env input_goal)

let test_inst_goal_nested_substitution () =
  let env = Env.add Env.empty ("x", Term.Function ("g", [ Term.Var "y" ])) in
  let input_goal = [ (2, L, Formula.Pred ("Q", [ Term.Var "x" ])) ] in
  let expected_goal =
    [ (2, L, Formula.Pred ("Q", [ Term.Function ("g", [ Term.Var "y" ]) ])) ]
  in
  Alcotest.(check goal_testable)
    "inst_goal: nested substitution" expected_goal (inst_goal env input_goal)

let test_inst_goal_with_multiple_substitutions () =
  let env = Env.add Env.empty ("x", Term.Function ("g", [ Term.Var "y" ])) in
  let env = Env.add env ("y", Term.Function ("h", [])) in
  let goal = [ (0, L, Formula.Pred ("P", [ Term.Var "x" ])) ] in
  let expected =
    [
      ( 0,
        L,
        Formula.Pred ("P", [ Term.Function ("g", [ Term.Function ("h", []) ]) ])
      );
    ]
  in
  Alcotest.(check goal_testable)
    "inst_goal: multiple substitutions" expected (inst_goal env goal)

(* inst_goals *)

let test_inst_goals_with_no_substitution () =
  let env = Env.empty in
  let input_goals = [ [ (0, L, Formula.Pred ("P", [ Term.Var "x" ])) ] ] in
  let expected_goals = input_goals in
  Alcotest.(check goal_table_testable)
    "inst_goals: no substitution" expected_goals
    (inst_goals env input_goals)

let test_inst_goals_single_substitution () =
  let env = Env.add Env.empty ("x", Term.Function ("f", [])) in
  let input_goals =
    [
      [ (0, L, Formula.Pred ("P", [ Term.Var "x" ])) ];
      [ (1, R, Formula.Pred ("Q", [ Term.Var "x" ])) ];
    ]
  in
  let expected_goals =
    [
      [ (0, L, Formula.Pred ("P", [ Term.Function ("f", []) ])) ];
      [ (1, R, Formula.Pred ("Q", [ Term.Function ("f", []) ])) ];
    ]
  in
  Alcotest.(check goal_table_testable)
    "inst_goals: single substitution" expected_goals
    (inst_goals env input_goals)

let test_inst_goals_nested_substitution () =
  let env = Env.add Env.empty ("x", Term.Function ("g", [ Term.Var "y" ])) in
  let env = Env.add env ("y", Term.Function ("h", [])) in
  let input_goals =
    [
      [ (0, L, Formula.Pred ("P", [ Term.Var "x" ])) ];
      [ (1, R, Formula.Pred ("Q", [ Term.Var "x" ])) ];
    ]
  in
  let expected_goals =
    [
      [
        ( 0,
          L,
          Formula.Pred
            ("P", [ Term.Function ("g", [ Term.Function ("h", []) ]) ]) );
      ];
      [
        ( 1,
          R,
          Formula.Pred
            ("Q", [ Term.Function ("g", [ Term.Function ("h", []) ]) ]) );
      ];
    ]
  in
  Alcotest.(check goal_table_testable)
    "inst_goals: nested substitution" expected_goals
    (inst_goals env input_goals)

(* split_goal *)

let test_split_goal_empty_goal () =
  let goal = [] in
  let ls, rs = split_goal goal in
  Alcotest.(check (list formula_testable))
    "split_goal: handles empty goal (left)" [] ls;
  Alcotest.(check (list formula_testable))
    "split_goal: handles empty goal (right)" [] rs

let test_split_goal_mixed_goal_entries () =
  let goal =
    [
      (1, L, Formula.Pred ("p", [ Term.Var "x" ]));
      (2, R, Formula.Pred ("q", [ Term.Var "y" ]));
      (3, L, Formula.Pred ("r", [ Term.Var "z" ]));
      (4, R, Formula.Pred ("s", [ Term.Var "w" ]));
      (5, L, Formula.Pred ("t", [ Term.Var "u" ]));
    ]
  in
  let ls, rs = split_goal goal in
  Alcotest.(check (list formula_testable))
    "split_goal: handles mixed goal entries (left)"
    [
      Formula.Pred ("p", [ Term.Var "x" ]);
      Formula.Pred ("r", [ Term.Var "z" ]);
      Formula.Pred ("t", [ Term.Var "u" ]);
    ]
    ls;
  Alcotest.(check (list formula_testable))
    "split_goal: handles mixed goal entries (right)"
    [
      Formula.Pred ("q", [ Term.Var "y" ]); Formula.Pred ("s", [ Term.Var "w" ]);
    ]
    rs

(* solve_goal *)

let test_solve_goal_basic_unification () =
  let goal =
    [
      (1, L, Formula.Pred ("p", [ Term.Var "x" ]));
      (2, R, Formula.Pred ("p", [ Term.Bound 42 ]));
    ]
  in
  let result = solve_goal goal in
  let expected_unifier = Env.add Env.empty ("x", Term.Bound 42) in
  let expected = [ (Formula.Pred ("p", [ Term.Var "x" ]), expected_unifier) ] in
  Alcotest.(check (list (pair formula_testable env_testable)))
    "solve_goal: unifies simple predicates" expected result

let test_solve_goal_no_unification () =
  let goal =
    [
      (1, L, Formula.Pred ("p", [ Term.Var "x" ]));
      (2, R, Formula.Pred ("q", [ Term.Bound 42 ]));
      (* ^^^ different predicate *)
    ]
  in
  let result = solve_goal goal in
  Alcotest.(check (list (pair formula_testable env_testable)))
    "solve_goal: returns empty list if no unification is possible" [] result

let test_solve_goal_multiple_unification () =
  let goal =
    [
      (1, L, Formula.Pred ("p", [ Term.Var "x" ]));
      (2, R, Formula.Pred ("p", [ Term.Bound 42 ]));
      (3, R, Formula.Pred ("p", [ Term.Bound 43 ]));
    ]
  in
  let result = solve_goal goal in
  let expected_unifier = Env.add Env.empty ("x", Term.Bound 42) in
  let expected = [ (Formula.Pred ("p", [ Term.Var "x" ]), expected_unifier) ] in
  Alcotest.(check (list (pair formula_testable env_testable)))
    "solve_goal: handles multiple unification possibilities" expected result

let test_solve_goal_nested_terms () =
  let goal =
    [
      (1, L, Formula.Pred ("p", [ Term.Function ("f", [ Term.Var "x" ]) ]));
      (2, R, Formula.Pred ("p", [ Term.Function ("f", [ Term.Bound 42 ]) ]));
    ]
  in
  let result = solve_goal goal in
  let expected_unifier = Env.add Env.empty ("x", Term.Bound 42) in
  let expected =
    [
      ( Formula.Pred ("p", [ Term.Function ("f", [ Term.Var "x" ]) ]),
        expected_unifier );
    ]
  in
  Alcotest.(check (list (pair formula_testable env_testable)))
    "solve_goal: works with nested terms" expected result

(* insert_goals *)

let test_insert_goals_solvable () =
  let initial_goal_table = [] in
  let goals =
    [
      [
        (1, L, Formula.Pred ("p", [ Term.Var "x" ]));
        (2, R, Formula.Pred ("p", [ Term.Bound 42 ]));
      ];
    ]
  in
  let final_fs, final_goal_table =
    insert_goals initial_goal_table (goals, [])
  in
  let expected_fs = [ Formula.Pred ("p", [ Term.Bound 42 ]) ] in
  let expected_goal_table = [] in
  Alcotest.(check (list formula_testable))
    "insert_goals solvable: final_fs" expected_fs final_fs;
  Alcotest.(check goal_table_testable)
    "insert_goals solvable: final_goal_table" expected_goal_table
    final_goal_table

let test_insert_goals_unsolvable () =
  let initial_goal_table = [] in
  let goals =
    [
      [
        (1, L, Formula.Pred ("p", [ Term.Var "x" ]));
        (2, R, Formula.Pred ("q", [ Term.Bound 42 ]));
        (* ^^^ different predicates *)
      ];
    ]
  in
  let final_fs, final_goal_table =
    insert_goals initial_goal_table (goals, [])
  in
  let expected_fs = [] in
  let expected_goal_table = goals in
  Alcotest.(check (list formula_testable))
    "insert_goals unsolvable: final_fs" expected_fs final_fs;
  Alcotest.(check goal_table_testable)
    "insert_goals unsolvable: final_goal_table" expected_goal_table
    final_goal_table

let test_insert_goals_mixed () =
  let initial_goal_table = [] in
  let goals =
    [
      [
        (1, L, Formula.Pred ("p", [ Term.Var "x" ]));
        (2, R, Formula.Pred ("p", [ Term.Bound 42 ]));
        (* solvable *)
      ];
      [
        (1, L, Formula.Pred ("q", [ Term.Var "y" ]));
        (2, R, Formula.Pred ("r", [ Term.Bound 43 ]));
        (* unsolvable *)
      ];
    ]
  in
  let final_fs, final_goal_table =
    insert_goals initial_goal_table (goals, [])
  in
  let expected_fs = [ Formula.Pred ("p", [ Term.Bound 42 ]) ] in
  let expected_goal_table =
    [
      [
        (1, L, Formula.Pred ("q", [ Term.Var "y" ]));
        (2, R, Formula.Pred ("r", [ Term.Bound 43 ]));
      ];
    ]
  in
  Alcotest.(check (list formula_testable))
    "insert_goals mixed: final_fs" expected_fs final_fs;
  Alcotest.(check goal_table_testable)
    "insert_goals mixed: final_goal_table" expected_goal_table final_goal_table

let test_insert_goals_nested () =
  let initial_goal_table = [] in
  let goals =
    [
      [
        (1, L, Formula.Pred ("p", [ Term.Function ("f", [ Term.Var "x" ]) ]));
        (2, R, Formula.Pred ("p", [ Term.Function ("f", [ Term.Bound 42 ]) ]));
      ];
    ]
  in
  let final_fs, final_goal_table =
    insert_goals initial_goal_table (goals, [])
  in
  let expected_fs =
    [ Formula.Pred ("p", [ Term.Function ("f", [ Term.Bound 42 ]) ]) ]
  in
  let expected_goal_table = [] in
  Alcotest.(check (list formula_testable))
    "insert_goals nested: final_fs" expected_fs final_fs;
  Alcotest.(check goal_table_testable)
    "insert_goals nested: final_goal_table" expected_goal_table final_goal_table

(* reduce_goal *)

(* let reduce_goal pair goal = failwith "whatever nahooy" *)
