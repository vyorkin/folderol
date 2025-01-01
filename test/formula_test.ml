open FolderolLib

let formula_testable = Formula.(Alcotest.testable pp equal)

let test_abstract_simple_formula () =
  let term = Term.Var "x" in
  let formula =
    Formula.Pred
      ( "P",
        [ Term.Var "x"; Term.Function ("f", [ Term.Var "y"; Term.Var "x" ]) ] )
  in
  let expected =
    Formula.Pred
      ( "P",
        [ Term.Bound 0; Term.Function ("f", [ Term.Var "y"; Term.Bound 0 ]) ] )
  in
  Alcotest.(check formula_testable)
    "abstract: simple" expected
    (Formula.abstract term formula)

let test_abstract_nested_formula () =
  let term = Term.Var "x" in
  let formula =
    Formula.Conn
      ( Conj,
        [
          Pred ("P", [ Term.Var "x" ]);
          Quant (Forall, "y", Pred ("Q", [ Term.Var "x" ]));
        ] )
  in
  let expected =
    Formula.Conn
      ( Conj,
        [
          Pred ("P", [ Term.Bound 0 ]);
          Quant (Forall, "y", Pred ("Q", [ Term.Bound 1 ]));
        ] )
  in
  Alcotest.(check formula_testable)
    "abstract: nested formula" expected
    (Formula.abstract term formula)

let test_subst_bound_var_simple_formula () =
  let term = Term.Var "z" in
  let formula =
    Formula.Pred
      ( "P",
        [ Term.Bound 0; Term.Function ("f", [ Term.Var "y"; Term.Bound 0 ]) ] )
  in
  let expected =
    Formula.Pred
      ( "P",
        [ Term.Var "z"; Term.Function ("f", [ Term.Var "y"; Term.Var "z" ]) ] )
  in
  Alcotest.(check formula_testable)
    "subst_bound_var: simple" expected
    (Formula.subst_bound_var term formula)

let test_subst_bound_var_nested_formula () =
  let term = Term.Var "z" in
  let formula =
    Formula.Conn
      ( Disj,
        [
          Pred ("P", [ Term.Bound 0 ]);
          Quant (Exists, "y", Pred ("Q", [ Term.Bound 1 ]));
        ] )
  in
  let expected =
    Formula.Conn
      ( Disj,
        [
          Pred ("P", [ Term.Var "z" ]);
          Quant (Exists, "y", Pred ("Q", [ Term.Var "z" ]));
        ] )
  in
  Alcotest.(check formula_testable)
    "subst_bound_var: nested formula" expected
    (Formula.subst_bound_var term formula)

let test_pp_conjunction_formula () =
  let actual =
    Formula.to_string
      (Formula.Conn (Conj, [ Pred ("P", [ Var "x" ]); Pred ("Q", [ Var "y" ]) ]))
  in
  let expected = "P(x) ∧ Q(y)" in
  Alcotest.(check string) "pp: conjunction formula" expected actual

let test_pp_quantified_formula () =
  let actual =
    Formula.to_string (Quant (Forall, "x", Pred ("P", [ Var "x" ])))
  in
  let expected = "∀x.P(x)" in
  Alcotest.(check string) "pp: quantified formula" expected actual

let test_pp_implication () =
  let actual =
    Formula.to_string
      (Conn
         ( Impl,
           [
             Pred ("P", [ Var "x" ]);
             Quant
               ( Exists,
                 "y",
                 Conn
                   (Disj, [ Pred ("Q", [ Var "y" ]); Pred ("R", [ Var "z" ]) ])
               );
           ] ))
  in
  let expected = "P(x) → ∃y.(Q(y) ∨ R(z))" in
  Alcotest.(check string) "pp: implication formula" expected actual
