open FolderolLib
open Instantiation

(* testables *)

let env_testable = Alcotest.testable Env.pp Env.equal
let term_testable = Alcotest.testable Term.pp Term.equal
let formula_testable = Alcotest.testable Formula.pp Formula.equal
let side_testable = Alcotest.testable Formula.pp_side Formula.equal_side
let goal_entry_testable = Alcotest.(triple int side_testable formula_testable)
let goal_testable = Alcotest.(list goal_entry_testable)
let goal_table_testable = Alcotest.(list goal_testable)

(* instantiate_term *)

let test_instantiate_term_with_no_substitution () =
  let open Term in
  let term = Function ("f", [ Var "x" ]) in
  Alcotest.(check term_testable)
    "instantiate_term: no substitution" term
    (instantiate_term Env.empty term)

let test_instantiate_term_with_single_substitution () =
  let env = Env.mk [ ("x", Term.Function ("g", [])) ] in
  let term = Term.Var "x" in
  let expected = Term.Function ("g", []) in
  Alcotest.(check term_testable)
    "instantiate_term: single substitution" expected
    (instantiate_term env term)

let test_instantiate_term_nested_substitution () =
  let open Term in
  let env =
    Env.mk [ ("x", Function ("g", [ Var "y" ])); ("y", Function ("h", [])) ]
  in
  let term = Var "x" in
  let expected = Function ("g", [ Function ("h", []) ]) in
  Alcotest.(check term_testable)
    "instantiate_term: nested substitution" expected
    (instantiate_term env term)

(* instantiate_formula *)

let test_instantiate_formula_with_no_substitution () =
  let env = Env.empty in
  let formula = Formula.Pred ("P", [ Term.Var "x"; Term.Function ("f", []) ]) in
  Alcotest.(check formula_testable)
    "instantiate_formula: no substitution" formula
    (instantiate_formula env formula)

let test_instantiate_formula_with_substitution () =
  let open Term in
  let open Formula in
  let env = Env.mk [ ("x", Function ("g", [])) ] in
  let formula = Pred ("P", [ Var "x"; Function ("f", []) ]) in
  let expected = Pred ("P", [ Function ("g", []); Function ("f", []) ]) in
  Alcotest.(check formula_testable)
    "instantiate_formula: with substitution" expected
    (instantiate_formula env formula)

let test_instantiate_formula_with_nested_substitution () =
  let open Term in
  let open Formula in
  let env =
    Env.mk [ ("x", Function ("g", [ Var "y" ])); ("y", Function ("h", [])) ]
  in
  let formula = Pred ("P", [ Var "x" ]) in
  let expected = Pred ("P", [ Function ("g", [ Function ("h", []) ]) ]) in
  Alcotest.(check formula_testable)
    "instantiate_formula: nested substitution" expected
    (instantiate_formula env formula)

(* instantiate_goal *)

let test_instantiate_goal_with_no_substitution () =
  let open Formula in
  let input_goal = [ (0, L, Pred ("P", [ Term.Var "x" ])) ] in
  let expected_goal = input_goal in
  Alcotest.(check goal_testable)
    "instantiate_goal: no substitution" expected_goal
    (instantiate_goal Env.empty input_goal)

let test_instantiate_goal_single_substitution () =
  let open Term in
  let open Formula in
  let env = Env.mk [ ("x", Function ("f", [])) ] in
  let input_goal = [ (1, R, Pred ("P", [ Var "x" ])) ] in
  let expected_goal = [ (1, R, Pred ("P", [ Function ("f", []) ])) ] in
  Alcotest.(check goal_testable)
    "instantiate_goal: single substitution" expected_goal
    (instantiate_goal env input_goal)

let test_instantiate_goal_nested_substitution () =
  let open Term in
  let open Formula in
  let env = Env.mk [ ("x", Function ("g", [ Var "y" ])) ] in
  let input_goal = [ (2, L, Pred ("Q", [ Var "x" ])) ] in
  let expected_goal = [ (2, L, Pred ("Q", [ Function ("g", [ Var "y" ]) ])) ] in
  Alcotest.(check goal_testable)
    "instantiate_goal: nested substitution" expected_goal
    (instantiate_goal env input_goal)

let test_instantiate_goal_with_multiple_substitutions () =
  let open Term in
  let open Formula in
  let env =
    Env.mk [ ("x", Function ("g", [ Var "y" ])); ("y", Function ("h", [])) ]
  in
  let goal = [ (0, L, Pred ("P", [ Var "x" ])) ] in
  let expected =
    [ (0, L, Pred ("P", [ Function ("g", [ Function ("h", []) ]) ])) ]
  in
  Alcotest.(check goal_testable)
    "instantiate_goal: multiple substitutions" expected
    (instantiate_goal env goal)

(* instantiate_goals *)

let test_instantiate_goals_with_no_substitution () =
  let open Formula in
  let input_goals = [ [ (0, L, Pred ("P", [ Term.Var "x" ])) ] ] in
  let expected_goals = input_goals in
  Alcotest.(check goal_table_testable)
    "instantiate_goals: no substitution" expected_goals
    (instantiate_goals Env.empty input_goals)

let test_instantiate_goals_single_substitution () =
  let open Term in
  let open Formula in
  let env = Env.mk [ ("x", Function ("f", [])) ] in
  let input_goals =
    [
      [ (0, L, Pred ("P", [ Var "x" ])) ];
      (* |- *)
      [ (1, R, Pred ("Q", [ Var "x" ])) ];
    ]
  in
  let expected_goals =
    [
      [ (0, L, Pred ("P", [ Function ("f", []) ])) ];
      (* |- *)
      [ (1, R, Pred ("Q", [ Function ("f", []) ])) ];
    ]
  in
  Alcotest.(check goal_table_testable)
    "instantiate_goals: single substitution" expected_goals
    (instantiate_goals env input_goals)

let test_instantiate_goals_nested_substitution () =
  let open Term in
  let open Formula in
  let env =
    Env.mk [ ("x", Function ("g", [ Var "y" ])); ("y", Function ("h", [])) ]
  in
  let input_goals =
    [
      [ (0, L, Pred ("P", [ Var "x" ])) ];
      (* |- *)
      [ (1, R, Pred ("Q", [ Var "x" ])) ];
    ]
  in
  let expected_goals =
    [
      [ (0, L, Pred ("P", [ Function ("g", [ Function ("h", []) ]) ])) ];
      (* |- *)
      [ (1, R, Pred ("Q", [ Function ("g", [ Function ("h", []) ]) ])) ];
    ]
  in
  Alcotest.(check goal_table_testable)
    "instantiate_goals: nested substitution" expected_goals
    (instantiate_goals env input_goals)
