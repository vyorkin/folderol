open FolderolLib

(* testables *)

let env_testable = Alcotest.testable Env.pp Env.equal
let term_testable = Alcotest.testable Term.pp Term.equal
let formula_testable = Alcotest.testable Formula.pp Formula.equal
let side_testable = Alcotest.testable Formula.pp_side Formula.equal_side
let goal_entry_testable = Alcotest.(triple int side_testable formula_testable)
let goal_testable = Alcotest.(list goal_entry_testable)
let goal_table_testable = Alcotest.(list goal_testable)

(* fold_formulas *)

let test_fold_formulas_in_a_single_goal_entry_with_a_single_empty_predicate () =
  let open Formula in
  let actual = Goal.fold_formulas (fun acc _ -> acc) ([], [ Pred ("P", []) ]) in
  let expected = [ Pred ("P", []) ] in
  Alcotest.(check (list formula_testable))
    "fold_formulas: single goal entry with a single empty predicate" expected
    actual

let test_fold_formulas_in_a_single_goal_entry () =
  let open Formula in
  let goal = [ (1, L, Pred ("Q", [ Var "x" ])) ] in
  let actual = Goal.fold_formulas (fun acc f -> f :: acc) (goal, []) in
  Alcotest.(check (list formula_testable))
    "fold_formulas: single goal entry"
    [ Pred ("Q", [ Var "x" ]) ]
    actual

let test_fold_formulas_in_multiple_goal_entries () =
  let open Formula in
  let goal =
    [
      (2, R, Conn (Conj, [ Pred ("A", []); Pred ("B", []) ]));
      (1, L, Quant (Forall, "x", Pred ("P", [ Var "x" ])));
    ]
  in
  let actual = Goal.fold_formulas (fun acc f -> acc @ [ f ]) (goal, []) in
  let expected =
    [
      Conn (Conj, [ Pred ("A", []); Pred ("B", []) ]);
      Quant (Forall, "x", Pred ("P", [ Var "x" ]));
    ]
  in
  Alcotest.(check (list formula_testable))
    "fold_formulas: multiple goal entries" expected actual

let test_fold_formulas_with_initial_state () =
  let open Formula in
  let goal =
    [
      (3, R, Conn (Impl, [ Pred ("C", []); Pred ("D", []) ]));
      (2, L, Pred ("B", [ Function ("f", [ Var "y" ]) ]));
    ]
  in
  let initial = [ Pred ("A", [ Var "z" ]) ] in
  let actual = Goal.fold_formulas (fun acc f -> f :: acc) (goal, initial) in
  let expected =
    [
      Pred ("B", [ Function ("f", [ Var "y" ]) ]);
      Conn (Impl, [ Pred ("C", []); Pred ("D", []) ]);
      Pred ("A", [ Var "z" ]);
    ]
  in
  Alcotest.(check (list formula_testable))
    "fold_formulas: with initial state" expected actual

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

(* mk *)

let test_mk_empty_formulas () =
  let open Formula in
  let goal = [ (1, L, Pred ("P", [ Term.Var "x" ])) ] in
  let actual = Goal.mk goal [] in
  Alcotest.(check goal_testable)
    "mk: empty formulas leave goal unchanged" goal actual

let test_mk_single_formula () =
  (* mk [(P(x)|-)] [(Q∧|-)] -> [(Q∧|-), (P(x)|-)] *)
  let open Formula in
  let initial_predicate = Pred ("P", [ Term.Var "x" ]) in
  let initial_goal = [ (1, L, initial_predicate) ] in
  let new_conn_conj = Conn (Conj, [ Pred ("Q", []) ]) in
  let formulas = [ (L, new_conn_conj) ] in
  let actual = Goal.mk initial_goal formulas in
  let expected =
    [
      (1, L, new_conn_conj);
      (* cost=1 for (L, Conj) - 1 subgoal *)
      (1, L, initial_predicate);
      (* cost=1 for (L, Pred)  *)
    ]
  in
  Alcotest.(check goal_testable) "mk: single formula" expected actual

let test_mk_multiple_formulas () =
  let open Formula in
  let open Term in
  let p_x = Pred ("P", [ Var "x" ]) in
  let initial_goal = [ (1, L, p_x) ] in
  let conn_impl = Conn (Impl, [ Pred ("A", []); Pred ("B", []) ]) in
  let quant_forall = Quant (Forall, "y", Pred ("Q", [ Var "y" ])) in
  let formulas = [ (R, conn_impl); (L, quant_forall) ] in
  let actual = Goal.mk initial_goal formulas in
  let expected =
    [
      (1, R, conn_impl);
      (* cost=1 for (R, Impl) *)
      (1, L, p_x);
      (* cost=1 for (L, Pred)  *)
      (3, L, quant_forall);
      (* cost=3 for (L, Forall) *)
    ]
  in
  Alcotest.(check goal_testable) "mk: multiple formulas" expected actual

(* mk_list *)

let test_mk_list_empty_input () =
  let actual = Goal.mk_list [] [] in
  Alcotest.(check (list goal_testable)) "mk_list: empty input" [] actual

let test_mk_list_multiple_goal_sets () =
  let open Formula in
  let p_b = Pred ("B", []) in
  let initial_goal = [ (1, L, p_b) ] in
  let conn_disj = Conn (Disj, [ Pred ("X", []); Pred ("Y", []) ]) in
  let quant_exists = Quant (Exists, "z", Pred ("R", [ Term.Var "z" ])) in
  let conn_not = Conn (Not, [ Pred ("S", []) ]) in
  let formula_sets =
    [ [ (L, conn_disj) ]; [ (R, quant_exists); (L, conn_not) ] ]
  in
  let actual_goals = Goal.mk_list initial_goal formula_sets in
  let expected_goals =
    [
      [
        (1, L, p_b);
        (* cost=1 for (L, Pred)  *)
        (2, L, conn_disj);
        (* cost=2 for (L, Disj) - 2 subgoals *)
      ];
      [
        (1, L, conn_not);
        (* cost=1 for (L, Not) - 1 subgoal *)
        (1, L, p_b);
        (* cost=1 for (L, Pred)  *)
        (3, R, quant_exists);
        (* cost=3 for (R, Exists) *)
      ];
    ]
  in
  Alcotest.(check (list goal_testable))
    "mk_list: mutiple goals" expected_goals actual_goals

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
  let expected = [ (Pred ("P", [ Var "x" ]), expected_unifier) ] in
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
