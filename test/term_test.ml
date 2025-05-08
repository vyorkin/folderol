open FolderolLib
open Term

let term_testable = Alcotest.testable pp equal

(* variable_names *)

let test_variable_names_empty () =
  let actual = variable_names [] (Var "x") in
  Alcotest.(check (list string)) "empty initial" [ "x" ] actual

let test_variable_names_existing () =
  let actual = variable_names [ "y" ] (Var "y") in
  Alcotest.(check (list string)) "existing variable" [ "y" ] actual

let test_variable_names_nested () =
  let term =
    Function ("f", [ Var "a"; Function ("g", [ Var "b"; Var "a" ]) ])
  in
  let actual = variable_names [] term in
  Alcotest.(check (list string)) "nested functions" [ "b"; "a" ] actual

let test_variable_names_mixed () =
  let term =
    Function ("h", [ Bound 0; Var "y"; Function ("i", [ Var "z"; Var "x" ]) ])
  in
  let actual = variable_names [ "z" ] term in
  Alcotest.(check (list string)) "mixed terms" [ "x"; "y"; "z" ] actual

let test_variable_names_const () =
  let actual = variable_names [ "a" ] (Function ("c", [])) in
  Alcotest.(check (list string)) "constant term" [ "a" ] actual

let test_variable_names_dups () =
  let term = Function ("f", [ Var "x"; Var "x"; Var "y"; Var "x" ]) in
  let actual = variable_names [] term in
  Alcotest.(check (list string)) "duplicates" [ "y"; "x" ] actual

(* replace *)

let test_replace_in_simple_term () =
  let term = Var "x" in
  let result = replace (Var "x", Bound 42) term in
  Alcotest.(check term_testable) "replace: in a simple term" result (Bound 42)

let test_replace_in_nested_function () =
  let term = Function ("f", [ Var "x"; Bound 1; Var "x" ]) in
  let result = replace (Var "x", Bound 42) term in
  let expected = Function ("f", [ Bound 42; Bound 1; Bound 42 ]) in
  Alcotest.(check term_testable) "replace: in a nested function" expected result

let test_replace_does_not_modify_unmatched_terms () =
  let term = Function ("f", [ Param ("y", [ "a"; "b" ]); Bound 1 ]) in
  let result = replace (Var "x", Bound 42) term in
  Alcotest.(check term_testable)
    "replace: does not modify unmatched terms" term result

let test_replace_in_deeply_nested_function () =
  let term =
    Function ("f", [ Function ("g", [ Var "x"; Bound 2 ]); Var "x" ])
  in
  let result = replace (Var "x", Bound 99) term in
  let expected =
    Function ("f", [ Function ("g", [ Bound 99; Bound 2 ]); Bound 99 ])
  in
  Alcotest.(check term_testable)
    "replace: in a deeply nested function" expected result

(* pp *)

let test_pp_variable () =
  let expected = "x" in
  let actual = to_string (Var "x") in
  Alcotest.(check string) "pp: variable" expected actual

let test_pp_parameter () =
  let expected = "b(?x, ?y, ?z)" in
  let actual = to_string (Param ("b", [ "x"; "y"; "z" ])) in
  Alcotest.(check string) "pp: parameter" expected actual

let test_pp_bound_variable () =
  let expected = "42" in
  let actual = to_string (Bound 42) in
  Alcotest.(check string) "pp: bound variable" expected actual

let test_pp_function_with_no_arguments () =
  let expected = "P()" in
  let actual = to_string (Function ("P", [])) in
  Alcotest.(check string) "pp: function with no arguments" expected actual

let test_pp_function_with_arguments () =
  let expected = "Q(x, P(a, b))" in
  let actual =
    to_string
      (Function ("Q", [ Var "x"; Function ("P", [ Var "a"; Var "b" ]) ]))
  in
  Alcotest.(check string) "pp: function with arguments" expected actual

let test_pp_nested_functions () =
  let expected = "W(Z(P(a), Q(b, c)), x)" in
  let actual =
    to_string
      (Function
         ( "W",
           [
             Function
               ( "Z",
                 [
                   Function ("P", [ Var "a" ]);
                   Function ("Q", [ Var "b"; Var "c" ]);
                 ] );
             Var "x";
           ] ))
  in
  Alcotest.(check string) "pp: nested functions" expected actual
