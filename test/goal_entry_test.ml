open FolderolLib

(* testables *)

let env_testable = Alcotest.testable Env.pp Env.equal
let term_testable = Alcotest.testable Term.pp Term.equal
let formula_testable = Alcotest.testable Formula.pp Formula.equal
let side_testable = Alcotest.testable Formula.pp_side Formula.equal_side
let goal_entry_testable = Alcotest.(triple int side_testable formula_testable)
let goal_testable = Alcotest.(list goal_entry_testable)
let goal_table_testable = Alcotest.(list goal_testable)

(* to_string *)

let test_to_string_left_side () =
  let open Formula in
  let entry = (1, L, Pred ("P", [ Term.Var "x" ])) in
  let actual = Goal_entry.to_string entry in
  let expected = "(cost=1) P(x) |-" in
  Alcotest.(check string) "to_string: left side entry" expected actual

let test_to_string_right_side () =
  let open Formula in
  let entry = (1, R, Pred ("Q", [ Term.Function ("f", [ Term.Var "y" ]) ])) in
  let actual = Goal_entry.to_string entry in
  let expected = "|- (cost=1) Q(f(y))" in
  Alcotest.(check string) "to_string: right side entry" expected actual

let test_to_string_complex_formula () =
  let open Formula in
  let entry = (1, L, Conn (Conj, [ Pred ("A", []); Pred ("B", []) ])) in
  let actual = Goal_entry.to_string entry in
  let expected = "(cost=1) A ∧ B |-" in
  Alcotest.(check string) "to_string: complex formula" expected actual
