open FolderolLib

let term_testable = Term.(Alcotest.testable pp equal)

let test_replace_in_simple_term () =
  let term = Term.Var "x" in
  let result = Term.replace (Var "x", Bound 42) term in
  Alcotest.(check term_testable)
    "replace: in a simple term" result (Term.Bound 42)

let test_replace_in_nested_function () =
  let term = Term.Function ("f", [ Var "x"; Bound 1; Var "x" ]) in
  let result = Term.replace (Var "x", Bound 42) term in
  let expected = Term.Function ("f", [ Bound 42; Bound 1; Bound 42 ]) in
  Alcotest.(check term_testable) "replace: in a nested function" expected result

let test_replace_does_not_modify_unmatched_terms () =
  let term = Term.Function ("f", [ Param ("y", [ "a"; "b" ]); Bound 1 ]) in
  let result = Term.replace (Var "x", Bound 42) term in
  Alcotest.(check term_testable)
    "replace: does not modify unmatched terms" term result

let test_replace_in_deeply_nested_function () =
  let term =
    Term.Function ("f", [ Function ("g", [ Var "x"; Bound 2 ]); Var "x" ])
  in
  let result = Term.replace (Var "x", Bound 99) term in
  let expected =
    Term.Function ("f", [ Function ("g", [ Bound 99; Bound 2 ]); Bound 99 ])
  in
  Alcotest.(check term_testable)
    "replace: in a deeply nested function" expected result

let test_pp_variable () =
  let expected = "x" in
  let actual = Term.to_string (Var "x") in
  Alcotest.(check string) "pp: variable" expected actual

let test_pp_parameter () =
  let expected = "f(x, y, z)" in
  let actual = Term.to_string (Param ("f", [ "x"; "y"; "z" ])) in
  Alcotest.(check string) "pp: parameter" expected actual

let test_pp_bound_variable () =
  let expected = "42" in
  let actual = Term.to_string (Bound 42) in
  Alcotest.(check string) "pp: bound variable" expected actual

let test_pp_function_with_no_arguments () =
  let expected = "g()" in
  let actual = Term.to_string (Function ("g", [])) in
  Alcotest.(check string) "pp: function with no arguments" expected actual

let test_pp_function_with_arguments () =
  let expected = "h(x, f(a, b))" in
  let actual =
    Term.to_string
      (Function ("h", [ Var "x"; Function ("f", [ Var "a"; Var "b" ]) ]))
  in
  Alcotest.(check string) "pp: function with arguments" expected actual

let test_pp_nested_functions () =
  let expected = "k(h(f(a), g(b, c)), x)" in
  let actual =
    Term.to_string
      (Function
         ( "k",
           [
             Function
               ( "h",
                 [
                   Function ("f", [ Var "a" ]);
                   Function ("g", [ Var "b"; Var "c" ]);
                 ] );
             Var "x";
           ] ))
  in
  Alcotest.(check string) "pp: nested functions" expected actual
