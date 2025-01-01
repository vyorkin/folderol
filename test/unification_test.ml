open FolderolLib
open Unification

(* helpers *)

let pp_error ppf err = Format.pp_print_string ppf err
let error = Alcotest.testable pp_error ( = )

(* testables *)

let term_testable = Alcotest.testable Term.pp Term.equal
let env_testable = Alcotest.testable Env.pp Env.equal

(* chase_var *)

let test_chase_var_with_non_existent_variable () =
  let env = Env.empty in
  let var = Term.Var "x" in
  let resolved = chase_var env var in
  Alcotest.(check term_testable) "chase_var: non-existent variable" var resolved

let test_chase_var_basic_variable_resolution () =
  let env = Env.add Env.empty ("x", Term.Function ("f", [])) in
  let resolved = chase_var env (Term.Var "x") in
  let expected = Term.Function ("f", []) in
  Alcotest.(check term_testable)
    "chase_var: basic variable resolution" expected resolved

let test_chase_var_chained_variable_resolution () =
  let env = Env.add Env.empty ("x", Term.Function ("f", [])) in
  let env = Env.add env ("y", Term.Var "x") in
  let resolved = chase_var env (Term.Var "y") in
  let expected = Term.Function ("f", []) in
  Alcotest.(check term_testable)
    "chase_var: chained variable resolution" expected resolved

let test_chase_var_resolves_non_variable_term () =
  let env = Env.empty in
  let non_var_term = Term.Function ("g", [ Term.Var "z" ]) in
  let resolved = chase_var env non_var_term in
  Alcotest.(check term_testable)
    "chase_var: resolves non-variable term" non_var_term resolved

(* occurs_in *)

let test_occurs_in_when_variable_does_not_occur () =
  let env = Env.empty in
  let term = Term.Function ("f", [ Term.Param ("p", [ "y" ]); Term.Var "z" ]) in
  Alcotest.(check bool)
    "chase_var: when variable does not occur" false (occurs_in env "x" term)

let test_occurs_in_when_varialbe_occurs_directly () =
  let env = Env.empty in
  let term = Term.Function ("f", [ Term.Var "x"; Term.Var "y" ]) in
  Alcotest.(check bool)
    "chase_var: variable occurs directly" true (occurs_in env "x" term)

let test_occurs_in_when_variable_occurs_in_param () =
  let env = Env.empty in
  let term = Term.Param ("p", [ "x"; "z" ]) in
  Alcotest.(check bool)
    "chase_var: variable occurs in parameter" true (occurs_in env "x" term)

let test_occurs_in_when_varialbe_does_not_occur_in_param () =
  let env = Env.empty in
  let term = Term.Param ("p", [ "y"; "z" ]) in
  Alcotest.(check bool)
    "chase_var: variable does not occur in parameter" false
    (occurs_in env "x" term)

let test_occurs_in_when_variable_occurs_in_nested_function () =
  let env = Env.empty in
  let term =
    Term.Function ("f", [ Term.Function ("g", [ Term.Var "x" ]); Term.Var "y" ])
  in
  Alcotest.(check bool)
    "chase_var: variable occurs in nested function" true
    (occurs_in env "x" term)

let test_occurs_in_when_varialbe_does_not_occur_in_nested_function () =
  let env = Env.empty in
  let term =
    Term.Function ("f", [ Term.Function ("g", [ Term.Var "z" ]); Term.Var "y" ])
  in
  Alcotest.(check bool)
    "occurs_in: variable does not occur in nested function" false
    (occurs_in env "x" term)

let test_occurs_in_when_varialbe_does_not_occur_in_empty_function () =
  let env = Env.empty in
  let term = Term.Function ("f", []) in
  Alcotest.(check bool)
    "occurs_in: variable does not occur in empty Function" false
    (occurs_in env "x" term)

(* unify *)

let test_unify_unifies_predicates () =
  (* equation: P(x, p(a)) = P(f(), p(a))
     expected: x -> f() *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Var "x"; Term.Param ("p", [ "a" ]) ]) in
  let f2 =
    Formula.Pred ("P", [ Term.Function ("f", []); Term.Param ("p", [ "a" ]) ])
  in
  let result_env = unify env (f1, f2) in
  let expected_env = Env.add Env.empty ("x", Term.Function ("f", [])) in
  Alcotest.(check (result env_testable error))
    "unify: unifies predicates" (Ok expected_env) result_env

let test_unify_fails_with_mismatched_predicate_names () =
  (* equation: P(x) = Q(f())
     expected: Error "Can not unify different predicates P and Q" *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Var "x" ]) in
  let f2 = Formula.Pred ("Q", [ Term.Function ("f", []) ]) in
  let result_env = unify env (f1, f2) in
  let expected_error = "Can not unify different predicates P and Q" in
  Alcotest.(check (result env_testable error))
    "unify: mismatched predicate names" (Error expected_error) result_env

let test_unify_occurs_check_failure () =
  (* equation: P(x) = P(f(x))
     expected: Error "Occurs check failed for variable x" *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Var "x" ]) in
  let f2 = Formula.Pred ("P", [ Term.Function ("f", [ Term.Var "x" ]) ]) in
  let result_env = unify env (f1, f2) in
  let expected_error = "Occurs check failed for variable x" in
  Alcotest.(check (result env_testable error))
    "unify: occurs check failure" (Error expected_error) result_env

let test_unify_fails_with_mismatched_term_lists_length () =
  (* equation: P(x, y) = P(f())
     expected: Error "Lists of terms have different length" *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Var "x"; Term.Var "y" ]) in
  let f2 = Formula.Pred ("P", [ Term.Function ("f", []) ]) in
  let result_env = unify env (f1, f2) in
  let expected_error = "Lists of terms have different length" in
  Alcotest.(check (result env_testable error))
    "unify: mismatched term lists length" (Error expected_error) result_env

let test_unify_unifies_nested_functions () =
  (* equation: P(f(x)) = P(f(g()))
     expected: x -> g() *)
  let env = Env.empty in
  let f1 = Formula.Pred ("P", [ Term.Function ("f", [ Term.Var "x" ]) ]) in
  let f2 =
    Formula.Pred ("P", [ Term.Function ("f", [ Term.Function ("g", []) ]) ])
  in
  let result_env = unify env (f1, f2) in
  let expected_env = Env.add Env.empty ("x", Term.Function ("g", [])) in
  Alcotest.(check (result env_testable error))
    "unify: unifies nested functions" (Ok expected_env) result_env

let test_unify_unifies_nested_functions_with_variables () =
  (* equation: P(f(x, g(y)), z) = P(f(a, g(b)), h(c))
     expected: x -> a, y -> b, z -> h(c) *)
  let env = Env.empty in
  let f1 =
    Formula.Pred
      ( "P",
        [
          Term.Function
            ("f", [ Term.Var "x"; Term.Function ("g", [ Term.Var "y" ]) ]);
          Term.Var "z";
        ] )
  in
  let f2 =
    Formula.Pred
      ( "P",
        [
          Term.Function
            ( "f",
              [
                Term.Param ("a", []);
                Term.Function ("g", [ Term.Param ("b", []) ]);
              ] );
          Term.Function ("h", [ Term.Param ("c", []) ]);
        ] )
  in
  let result_env = unify env (f1, f2) in
  let expected_env =
    let e = Env.empty in
    let e = Env.add e ("x", Term.Param ("a", [])) in
    let e = Env.add e ("y", Term.Param ("b", [])) in
    let e = Env.add e ("z", Term.Function ("h", [ Term.Param ("c", []) ])) in
    e
  in
  Alcotest.(check (result env_testable error))
    "unify: nested function unification with variables" (Ok expected_env)
    result_env

let test_unify_fails_due_to_different_nested_functions () =
  (* equation: P(f(x), g(y)) = P(h(a), g(b))
     expected: Error "Can not unify different functions f and h" *)
  let env = Env.empty in
  let f1 =
    Formula.Pred
      ( "P",
        [
          Term.Function ("f", [ Term.Var "x" ]);
          Term.Function ("g", [ Term.Var "y" ]);
        ] )
  in
  let f2 =
    Formula.Pred
      ( "P",
        [
          Term.Function ("h", [ Term.Param ("a", []) ]);
          Term.Function ("g", [ Term.Param ("b", []) ]);
        ] )
  in
  let result_env = unify env (f1, f2) in
  let expected_error = "Can not unify different functions f and h" in
  Alcotest.(check (result env_testable error))
    "unify: failure due to different nested functions" (Error expected_error)
    result_env

let test_unify_unifies_deeply_nested_structures () =
  (* equation: P(f(g(h(x))), y) = P(f(g(h(a))), b)
     expected: x -> a, y -> b *)
  let env = Env.empty in
  let f1 =
    Formula.Pred
      ( "P",
        [
          Term.Function
            ( "f",
              [ Term.Function ("g", [ Term.Function ("h", [ Term.Var "x" ]) ]) ]
            );
          Term.Var "y";
        ] )
  in
  let f2 =
    Formula.Pred
      ( "P",
        [
          Term.Function
            ( "f",
              [
                Term.Function
                  ("g", [ Term.Function ("h", [ Term.Param ("a", []) ]) ]);
              ] );
          Term.Param ("b", []);
        ] )
  in
  let result_env = unify env (f1, f2) in
  let expected_env =
    let e = Env.empty in
    let e = Env.add e ("x", Term.Param ("a", [])) in
    let e = Env.add e ("y", Term.Param ("b", [])) in
    e
  in
  Alcotest.(check (result env_testable error))
    "unify: deeply nested structure" (Ok expected_env) result_env

let test_unify_unifies_deep_structures_with_multiple_variables () =
  (* equation: P(f(x, g(y, z)), h(w)) = P(f(a, g(b, c)), h(d))
     expected: x -> a, y -> b, z -> c, w -> d *)
  let env = Env.empty in
  let f1 =
    Formula.Pred
      ( "P",
        [
          Term.Function
            ( "f",
              [
                Term.Var "x"; Term.Function ("g", [ Term.Var "y"; Term.Var "z" ]);
              ] );
          Term.Function ("h", [ Term.Var "w" ]);
        ] )
  in
  let f2 =
    Formula.Pred
      ( "P",
        [
          Term.Function
            ( "f",
              [
                Term.Param ("a", []);
                Term.Function
                  ("g", [ Term.Param ("b", []); Term.Param ("c", []) ]);
              ] );
          Term.Function ("h", [ Term.Param ("d", []) ]);
        ] )
  in
  let result_env = unify env (f1, f2) in
  let expected_env =
    let e = Env.empty in
    let e = Env.add e ("x", Term.Param ("a", [])) in
    let e = Env.add e ("y", Term.Param ("b", [])) in
    let e = Env.add e ("z", Term.Param ("c", [])) in
    let e = Env.add e ("w", Term.Param ("d", [])) in
    e
  in
  Alcotest.(check (result env_testable error))
    "unify: multiple variables in deep structure" (Ok expected_env) result_env
