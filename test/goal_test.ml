open FolderolLib

(* testables *)

let env_testable = Alcotest.testable Env.pp Env.equal
let term_testable = Alcotest.testable Term.pp Term.equal
let formula_testable = Alcotest.testable Formula.pp Formula.equal
let side_testable = Alcotest.testable Formula.pp_side Formula.equal_side
let rule_testable = Alcotest.testable Rule.pp Rule.equal
let goal_entry_testable = Alcotest.(triple int side_testable formula_testable)
let goal_testable = Alcotest.(list goal_entry_testable)
let goal_table_testable = Alcotest.(list goal_testable)

(* fold_left *)

let test_fold_left_empty_goal () =
  let open Formula in
  let formulas = [ Pred ("P", []) ] in
  let goal = [] in
  let actual = Goal.fold_left ~f:(fun acc f -> f :: acc) ~init:formulas goal in
  let expected = [ Pred ("P", []) ] in
  Alcotest.(check (list formula_testable))
    "fold_left: empty goal and a single initial formula" expected actual

let test_fold_left_goal_with_a_single_entry () =
  let open Formula in
  let goal = [ (1, L, Pred ("Q", [ Var "x" ])) ] in
  let formulas = [] in
  let actual = Goal.fold_left ~f:(fun acc f -> f :: acc) ~init:formulas goal in
  let expected = [ Pred ("Q", [ Var "x" ]) ] in
  Alcotest.(check (list formula_testable))
    "fold_left: goal with a single entry and empty formulas" expected actual

let test_fold_left_goal_with_multiple_entries () =
  let open Formula in
  let goal =
    [
      (2, R, Conn (Conj, [ Pred ("A", []); Pred ("B", []) ]));
      (1, L, Quant (Forall, "x", Pred ("P", [ Var "x" ])));
    ]
  in
  let formulas = [] in
  let actual = Goal.fold_left ~f:(fun acc f -> f :: acc) ~init:formulas goal in
  let expected =
    [
      Quant (Forall, "x", Pred ("P", [ Var "x" ]));
      Conn (Conj, [ Pred ("A", []); Pred ("B", []) ]);
    ]
  in
  Alcotest.(check (list formula_testable))
    "fold_left: goal with multiple entries and empty formulas" expected actual

let test_fold_left_goal_with_multiple_entries_and_initial_formulas () =
  let open Formula in
  let goal =
    [
      (3, R, Conn (Impl, [ Pred ("C", []); Pred ("D", []) ]));
      (2, L, Pred ("B", [ Function ("f", [ Var "y" ]) ]));
    ]
  in
  let formulas = [ Pred ("A", [ Var "z" ]) ] in
  let actual = Goal.fold_left ~f:(fun acc f -> f :: acc) ~init:formulas goal in
  let expected =
    [
      Pred ("B", [ Function ("f", [ Var "y" ]) ]);
      Conn (Impl, [ Pred ("C", []); Pred ("D", []) ]);
      Pred ("A", [ Var "z" ]);
    ]
  in
  Alcotest.(check (list formula_testable))
    "fold_left: initial formulas and a goal with multiple entries" expected
    actual

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

(* mk_subgoal *)

let test_mk_subgoal_empty_formulas () =
  let open Formula in
  let goal = [ (1, L, Pred ("P", [ Term.Var "x" ])) ] in
  let actual = Goal.mk_subgoal goal [] in
  Alcotest.(check goal_testable)
    "mk_subgoal: empty formulas leave goal unchanged" goal actual

let test_mk_subgoal_single_formula () =
  (* mk [(P(x)|-)] [(Q∧|-)] -> [(Q∧|-), (P(x)|-)] *)
  let open Formula in
  let initial_predicate = Pred ("P", [ Term.Var "x" ]) in
  let initial_goal = [ (1, L, initial_predicate) ] in
  let new_conn_conj = Conn (Conj, [ Pred ("Q", []) ]) in
  let formulas = [ (L, new_conn_conj) ] in
  let actual = Goal.mk_subgoal initial_goal formulas in
  let expected =
    [
      (1, L, new_conn_conj);
      (* cost=1 for (L, Conj) - 1 subgoal *)
      (1, L, initial_predicate);
      (* cost=1 for (L, Pred)  *)
    ]
  in
  Alcotest.(check goal_testable) "mk_subgoal: single formula" expected actual

let test_mk_subgoal_multiple_formulas () =
  let open Formula in
  let open Term in
  let p_x = Pred ("P", [ Var "x" ]) in
  let initial_goal = [ (1, L, p_x) ] in
  let conn_impl = Conn (Impl, [ Pred ("A", []); Pred ("B", []) ]) in
  let quant_forall = Quant (Forall, "y", Pred ("Q", [ Var "y" ])) in
  let formulas = [ (R, conn_impl); (L, quant_forall) ] in
  let actual = Goal.mk_subgoal initial_goal formulas in
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
  Alcotest.(check goal_testable) "mk_subgoal: multiple formulas" expected actual

(* mk_subgoals *)

let test_mk_subgoals_empty_input () =
  let actual = Goal.mk_subgoals [] [] in
  Alcotest.(check (list goal_testable)) "mk_subgoals: empty input" [] actual

let test_mk_subgoals_multiple_goal_sets () =
  let open Formula in
  let p_b = Pred ("B", []) in
  let initial_goal = [ (1, L, p_b) ] in
  let conn_disj = Conn (Disj, [ Pred ("X", []); Pred ("Y", []) ]) in
  let quant_exists = Quant (Exists, "z", Pred ("R", [ Term.Var "z" ])) in
  let conn_not = Conn (Not, [ Pred ("S", []) ]) in
  let formula_sets =
    [ [ (L, conn_disj) ]; [ (R, quant_exists); (L, conn_not) ] ]
  in
  let actual_goals = Goal.mk_subgoals initial_goal formula_sets in
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
    "mk_subgoals: mutiple goals" expected_goals actual_goals

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

(* variable_names *)

let test_variable_names_empty_goal () =
  let goal = [] in
  let actual = Goal.variable_names ~init:[] goal in
  let expected = [] in
  Alcotest.(check (list string))
    "variable_names: empty goal returns empty list" expected actual

let test_variable_names_simple_goal () =
  let open Formula in
  let open Term in
  let goal =
    [ (1, L, Pred ("P", [ Var "x" ])); (2, R, Pred ("Q", [ Var "y" ])) ]
  in
  let actual = Goal.variable_names ~init:[] goal in
  let expected = [ "y"; "x" ] in
  Alcotest.(check (list string))
    "variable_names: simple goal with two variables" expected actual

let test_variable_names_with_init_and_duplicates () =
  let open Formula in
  let open Term in
  let goal =
    [
      (1, L, Pred ("P", [ Var "x"; Function ("f", [ Var "z" ]) ]));
      (1, R, Quant (Forall, "y", Pred ("Q", [ Var "y"; Var "x" ])));
    ]
  in
  let init = [ "a"; "z" ] in
  let actual = Goal.variable_names ~init goal in
  let expected = [ "y"; "x"; "a"; "z" ] in
  Alcotest.(check (list string))
    "variable_names: with init list and duplicate variables" expected actual

(* reduce *)

let reduce_result =
  Alcotest.(result (pair rule_testable (list goal_testable)) string)

(* (¬R): |- ¬P ==> [ P |- ] *)
let test_reduce_not_right () =
  let open Formula in
  let goal = [] in
  let entry = (1, R, Conn (Not, [ Pred ("P", []) ])) in
  let result = Goal.reduce goal entry in
  let expected = Ok (Rule.NotR, [ [ (4, L, Pred ("P", [])) ] ]) in
  Alcotest.(check reduce_result) "reduce: ¬R rule" expected result

(* (¬L): ¬P |- ==> [ |- P ] *)
let test_reduce_not_left () =
  let open Formula in
  let goal = [] in
  let entry = (1, L, Conn (Not, [ Pred ("P", []) ])) in
  let result = Goal.reduce goal entry in
  let expected = Ok (Rule.NotL, [ [ (4, R, Pred ("P", [])) ] ]) in
  Alcotest.(check reduce_result) "reduce: ¬L rule" expected result

(* (∧R): |- P ∧ Q ==> [ |- P ], [ |- Q ] *)
let test_reduce_conj_right () =
  let open Formula in
  let goal = [] in
  let entry = (1, R, Conn (Conj, [ Pred ("P", []); Pred ("Q", []) ])) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok (Rule.ConjR, [ [ (4, R, Pred ("P", [])) ]; [ (4, R, Pred ("Q", [])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: ∧R rule" expected result

(* (∧L): P ∧ Q |- ==> [ P, Q |- ] *)
let test_reduce_conj_left () =
  let open Formula in
  let goal = [] in
  let entry = (1, L, Conn (Conj, [ Pred ("P", []); Pred ("Q", []) ])) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok (Rule.ConjL, [ [ (4, L, Pred ("P", [])); (4, L, Pred ("Q", [])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: ∧L rule" expected result

let test_reduce_disj_right () =
  let open Formula in
  let goal = [] in
  let entry = (1, R, Conn (Disj, [ Pred ("P", []); Pred ("Q", []) ])) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok (Rule.DisjR, [ [ (4, R, Pred ("P", [])); (4, R, Pred ("Q", [])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: ∨R rule" expected result

let test_reduce_disj_left () =
  let open Formula in
  let goal = [] in
  let entry = (1, L, Conn (Disj, [ Pred ("P", []); Pred ("Q", []) ])) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok (Rule.DisjL, [ [ (4, L, Pred ("P", [])) ]; [ (4, L, Pred ("Q", [])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: ∨L rule" expected result

let test_reduce_impl_right () =
  let open Formula in
  let goal = [] in
  let entry = (1, R, Conn (Impl, [ Pred ("P", []); Pred ("Q", []) ])) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok (Rule.ImplR, [ [ (4, L, Pred ("P", [])); (4, R, Pred ("Q", [])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: →R rule" expected result

let test_reduce_impl_left () =
  let open Formula in
  let goal = [] in
  let entry = (1, L, Conn (Impl, [ Pred ("P", []); Pred ("Q", []) ])) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok (Rule.ImplL, [ [ (4, R, Pred ("P", [])) ]; [ (4, L, Pred ("Q", [])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: →L rule" expected result

let test_reduce_iff_right () =
  let open Formula in
  let goal = [] in
  let entry = (1, R, Conn (Iff, [ Pred ("P", []); Pred ("Q", []) ])) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok
      ( Rule.IffR,
        [
          [ (4, L, Pred ("P", [])); (4, R, Pred ("Q", [])) ];
          [ (4, R, Pred ("P", [])); (4, L, Pred ("Q", [])) ];
        ] )
  in
  Alcotest.(check reduce_result) "reduce: ↔R rule" expected result

let test_reduce_iff_left () =
  let open Formula in
  let goal = [] in
  let entry = (1, L, Conn (Iff, [ Pred ("P", []); Pred ("Q", []) ])) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok
      ( Rule.IffL,
        [
          [ (4, L, Pred ("P", [])); (4, L, Pred ("Q", [])) ];
          [ (4, R, Pred ("P", [])); (4, R, Pred ("Q", [])) ];
        ] )
  in
  Alcotest.(check reduce_result) "reduce: ↔L rule" expected result

(* (∀R) *)
let test_reduce_forall_right () =
  let open Formula in
  let open Term in
  Symbol.reset ();
  let goal = [] in
  let entry = (1, R, Quant (Forall, "x", Pred ("P", [ Bound 0 ]))) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok (Rule.ForallR, [ [ (4, R, Pred ("P", [ Param ("a", []) ])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: ∀R rule" expected result

(* (∀L):
   ∀x.P(x) |- ==> [ ∀x.P(x), ∀x.P[t/x] |- ]
   or
   ∀x.P(0) |- ==> [ ∀x.P(x), ∀x.P(t) |- ],
   where t := Var "a" *)
let test_reduce_forall_left () =
  let open Formula in
  let open Term in
  Symbol.reset ();
  let goal = [] in
  let entry = (1, L, Quant (Forall, "x", Pred ("P", [ Bound 0 ]))) in
  let result = Goal.reduce goal entry in
  (* A copy of ∀x.P is retained in the subgoal so that
     the rule can be applied again with different terms. *)
  let expected =
    Ok (Rule.ForallL, [ [ entry; (4, L, Pred ("P", [ Var "a" ])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: ∀L rule" expected result

(* (∃R):
   |- ∃x.P(x) ==> [ ∃x.P(x), |- ∃x.P(t/x) ]
   or
   |- ∃x.P(0) ==> [ ∃x.P(x), |- ∃x.P(t) ],
   where t := Var "a" *)
let test_reduce_exists_right () =
  let open Formula in
  let open Term in
  Symbol.reset ();
  let goal = [] in
  let entry = (1, R, Quant (Exists, "x", Pred ("P", [ Bound 0 ]))) in
  let result = Goal.reduce goal entry in
  (* The rule ∃R similarly retains the quantified formula in its subgoal. *)
  let expected =
    Ok (Rule.ExistsR, [ [ entry; (4, R, Pred ("P", [ Var "a" ])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: ∃R rule" expected result

(* (∃L) *)
let test_reduce_exists_left () =
  let open Formula in
  let open Term in
  Symbol.reset ();
  let goal = [] in
  let entry = (1, L, Quant (Exists, "x", Pred ("P", [ Bound 0 ]))) in
  let result = Goal.reduce goal entry in
  let expected =
    Ok (Rule.ExistsL, [ [ (4, L, Pred ("P", [ Param ("a", []) ])) ] ])
  in
  Alcotest.(check reduce_result) "reduce: ∃L rule" expected result

(* TODO: *)
(*   let goal = [ (1, R, Pred ("P", [])) ] in *)
(*   let entry = (1, L, Pred ("P", [])) in *)

(* TODO: *)
(*   let goal = [ (1, R, Pred ("Q", [])) ] in *)
(*   let entry = (1, L, Pred ("P", [])) in *)

(* to_string *)

let test_to_string_empty_goal () =
  let goal = [] in
  let actual = Goal.to_string goal in
  let expected = "|-" in
  Alcotest.(check string) "to_string: empty goal" expected actual

let test_to_string_mixed_goal () =
  let open Formula in
  let open Term in
  let goal =
    [
      (4, L, Pred ("P", [ Var "x" ]));
      (4, R, Pred ("Q", [ Var "y" ]));
      (4, L, Pred ("R", [ Var "z" ]));
    ]
  in
  let actual = Goal.to_string goal in
  let expected = "P(x), R(z) |- Q(y)" in
  Alcotest.(check string) "to_string: mixed goal" expected actual

let test_to_string_complex_goal () =
  let open Formula in
  let open Term in
  let goal =
    [
      (1, L, Conn (Conj, [ Pred ("A", []); Pred ("B", []) ]));
      (1, R, Conn (Impl, [ Pred ("C", []); Pred ("D", []) ]));
      (3, L, Quant (Forall, "x", Pred ("P", [ Var "x" ])));
    ]
  in
  let actual = Goal.to_string goal in
  let expected = "A ∧ B, ∀x.P(x) |- C → D" in
  Alcotest.(check string) "to_string: complex goal" expected actual
