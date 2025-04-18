open FolderolLib
open Term
open Formula

let term_testable = Alcotest.testable Term.pp Term.equal
let formula_testable = Alcotest.testable pp equal

(* abstract *)

let test_abstract_simple_formula () =
  let term = Var "x" in
  let formula = Pred ("P", [ Var "x"; Function ("f", [ Var "y"; Var "x" ]) ]) in
  let expected =
    Pred ("P", [ Bound 0; Function ("f", [ Var "y"; Bound 0 ]) ])
  in
  Alcotest.(check formula_testable)
    "abstract: simple" expected (abstract term formula)

let test_abstract_nested_formula () =
  let term = Var "x" in
  let formula =
    Conn
      ( Conj,
        [
          Pred ("P", [ Var "x" ]); Quant (Forall, "y", Pred ("Q", [ Var "x" ]));
        ] )
  in
  let expected =
    Conn
      ( Conj,
        [
          Pred ("P", [ Bound 0 ]); Quant (Forall, "y", Pred ("Q", [ Bound 1 ]));
        ] )
  in
  Alcotest.(check formula_testable)
    "abstract: nested formula" expected (abstract term formula)

(* subst *)

let test_subst_bound_var_simple_formula () =
  let term = Var "z" in
  let formula = Pred ("P", [ Bound 0; Function ("f", [ Var "y"; Bound 0 ]) ]) in
  let expected =
    Pred ("P", [ Var "z"; Function ("f", [ Var "y"; Var "z" ]) ])
  in
  Alcotest.(check formula_testable)
    "subst_bound_var: simple" expected
    (subst_bound_var term formula)

let test_subst_bound_var_nested_formula () =
  let term = Var "z" in
  let formula =
    Conn
      ( Disj,
        [
          Pred ("P", [ Bound 0 ]); Quant (Exists, "y", Pred ("Q", [ Bound 1 ]));
        ] )
  in
  let expected =
    Conn
      ( Disj,
        [
          Pred ("P", [ Var "z" ]); Quant (Exists, "y", Pred ("Q", [ Var "z" ]));
        ] )
  in
  Alcotest.(check formula_testable)
    "subst_bound_var: nested formula" expected
    (subst_bound_var term formula)

(* fold_terms *)

let collect_terms formula =
  let f acc term = term :: acc in
  fold_terms f (formula, []) |> List.rev

let test_fold_terms_empty () =
  let formula =
    Conn (Disj, [ Pred ("X", []); Quant (Exists, "y", Pred ("Y", [])) ])
  in
  let actual = collect_terms formula in
  let expected = [] in
  Alcotest.(check (list term_testable))
    "fold_terms: empty terms" expected actual

let test_fold_terms_in_a_single_predicate () =
  let formula = Pred ("P", [ Var "x"; Function ("f", [ Var "y" ]) ]) in
  let actual = collect_terms formula in
  let expected = [ Var "x"; Function ("f", [ Var "y" ]) ] in
  Alcotest.(check (list term_testable))
    "fold_terms: single predicate" expected actual

let test_fold_terms_in_nested_connectives () =
  let formula =
    Conn
      ( Conj,
        [
          Conn
            ( Disj,
              [
                Pred ("P", [ Function ("g", [ Var "a" ]) ]);
                Pred ("Q", [ Var "b" ]);
              ] );
          Quant (Exists, "x", Pred ("R", [ Var "c" ]));
        ] )
  in
  let actual = collect_terms formula in
  let expected = [ Function ("g", [ Var "a" ]); Var "b"; Var "c" ] in
  Alcotest.(check (list term_testable))
    "fold_terms: nested connectives" expected actual

let test_fold_terms_in_a_deep_quantifier () =
  let formula =
    Quant
      ( Forall,
        "y",
        Conn
          ( Impl,
            [
              Pred ("S", [ Var "d" ]);
              Quant (Exists, "z", Pred ("T", [ Function ("h", [ Var "e" ]) ]));
            ] ) )
  in
  let actual = collect_terms formula in
  let expected = [ Var "d"; Function ("h", [ Var "e" ]) ] in
  Alcotest.(check (list term_testable))
    "fold_terms: deep quantifier" expected actual

let test_fold_terms_in_a_mixed_structure () =
  let formula =
    Conn
      ( Iff,
        [
          Quant
            ( Forall,
              "x",
              Conn
                ( Conj,
                  [
                    Pred ("U", [ Var "f"; Function ("k", []) ]); Pred ("V", []);
                  ] ) );
          Pred ("W", [ Var "g" ]);
        ] )
  in
  let actual = collect_terms formula in
  let expected = [ Var "f"; Function ("k", []); Var "g" ] in
  Alcotest.(check (list term_testable))
    "fold_terms: mixed structure" expected actual

(* pp *)

let test_pp_conjunction_formula () =
  let actual =
    to_string
      (Conn (Conj, [ Pred ("P", [ Var "x" ]); Pred ("Q", [ Var "y" ]) ]))
  in
  let expected = "P(x) ∧ Q(y)" in
  Alcotest.(check string) "pp: conjunction formula" expected actual

let test_pp_quantified_formula () =
  let actual = to_string (Quant (Forall, "x", Pred ("P", [ Var "x" ]))) in
  let expected = "∀x.P(x)" in
  Alcotest.(check string) "pp: quantified formula" expected actual

let test_pp_implication () =
  let actual =
    to_string
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
