open FolderolLib

let formula_testable = Alcotest.testable Formula.pp Formula.equal

let parse_formula input =
  let lexbuf = Lexing.from_string input in
  try Parser.main Lexer.read lexbuf with
  | Lexer.LexingError msg -> failwith ("Lexing error: " ^ msg)
  | Parsing.Parse_error -> failwith "Parsing error"

let test_atomic_formula () =
  let open Formula in
  let input = "P(x, y)" in
  let expected = Pred ("P", [ Var "x"; Var "y" ]) in
  let parsed = parse_formula input in
  Alcotest.(check formula_testable) "Parse atomic formula" expected parsed

let test_predicate_without_parameters () =
  let open Formula in
  let input = "P" in
  let expected = Pred ("P", []) in
  let parsed = parse_formula input in
  Alcotest.(check formula_testable)
    "Parse predicate without parameters" expected parsed

let test_complex_formula_with_empty_predicates () =
  let open Formula in
  let input = "P ∧ (Q → R(x))" in
  let expected =
    Conn
      ( Conj,
        [
          Pred ("P", []);
          Conn (Impl, [ Pred ("Q", []); Pred ("R", [ Var "x" ]) ]);
        ] )
  in
  let parsed = parse_formula input in
  Alcotest.(check formula_testable)
    "Parse complex formula with empty predicates" expected parsed

let test_conjunction () =
  let open Formula in
  let input = "P(x) ∧ Q(y)" in
  let expected =
    Conn (Conj, [ Pred ("P", [ Var "x" ]); Pred ("Q", [ Var "y" ]) ])
  in
  let parsed = parse_formula input in
  Alcotest.(check formula_testable) "Parse conjunction" expected parsed

let test_implication () =
  let open Formula in
  let input = "P(x) → Q(y)" in
  let expected =
    Conn (Impl, [ Pred ("P", [ Var "x" ]); Pred ("Q", [ Var "y" ]) ])
  in
  let parsed = parse_formula input in
  Alcotest.(check formula_testable) "Parse implication" expected parsed

let test_forall_quantifier () =
  let open Formula in
  let input = "∀x.P(x)" in
  let expected = Quant (Forall, "x", Pred ("P", [ Var "x" ])) in
  let parsed = parse_formula input in
  Alcotest.(check formula_testable) "Parse universal quantifier" expected parsed

let test_exists_quantifier () =
  let open Formula in
  let input = "∃x.Q(x)" in
  let expected = Quant (Exists, "x", Pred ("Q", [ Var "x" ])) in
  let parsed = parse_formula input in
  Alcotest.(check formula_testable)
    "Parse existential quantifier" expected parsed

let test_parentheses () =
  let open Formula in
  let input = "(P(x) ∧ Q(y)) → R(z)" in
  let expected =
    Conn
      ( Impl,
        [
          Conn (Conj, [ Pred ("P", [ Var "x" ]); Pred ("Q", [ Var "y" ]) ]);
          Pred ("R", [ Var "z" ]);
        ] )
  in
  let parsed = parse_formula input in
  Alcotest.(check formula_testable)
    "Parse formula with parentheses" expected parsed
