let () =
  let open Alcotest in
  run "Folderol"
    [
      ( "Term",
        [
          test_case "Replace in a simple term" `Quick
            Term_test.test_replace_in_simple_term;
          test_case "Replace in a nested function" `Quick
            Term_test.test_replace_in_nested_function;
          test_case "Replace does not modify unmatched terms" `Quick
            Term_test.test_replace_does_not_modify_unmatched_terms;
          test_case "Replace in a deeply nested function" `Quick
            Term_test.test_replace_in_deeply_nested_function;
          test_case "Pretty-print variable" `Quick Term_test.test_pp_variable;
          test_case "Pretty-print parameter" `Quick Term_test.test_pp_parameter;
          test_case "Pretty-print bound variable" `Quick
            Term_test.test_pp_bound_variable;
          test_case "Pretty-print function with no arguments" `Quick
            Term_test.test_pp_function_with_no_arguments;
          test_case "Pretty-print function with arguments" `Quick
            Term_test.test_pp_function_with_arguments;
          test_case "Pretty-print nested functions" `Quick
            Term_test.test_pp_nested_functions;
        ] );
      ( "Formula",
        [
          test_case "Abstract simple formula" `Quick
            Formula_test.test_abstract_simple_formula;
          test_case "Abstract nested formula" `Quick
            Formula_test.test_abstract_nested_formula;
          test_case "Substitute bound var in simple formula" `Quick
            Formula_test.test_subst_bound_var_simple_formula;
          test_case "Substitute bound var in nested formula" `Quick
            Formula_test.test_subst_bound_var_nested_formula;
          test_case "Pretty-print conjunction formula" `Quick
            Formula_test.test_pp_conjunction_formula;
          test_case "Pretty-print quantified formula" `Quick
            Formula_test.test_pp_quantified_formula;
          test_case "Pretty-print implication formula" `Quick
            Formula_test.test_pp_implication;
        ] );
      ( "Parser",
        [
          test_case "Parse atomic formula" `Quick
            Parser_test.test_atomic_formula;
          test_case "Parse conjunction formula" `Quick
            Parser_test.test_conjunction;
          test_case "Parse implication formula" `Quick
            Parser_test.test_implication;
          test_case "Parse formula with forall quantifier" `Quick
            Parser_test.test_forall_quantifier;
          test_case "Parse formula with exists quantifier" `Quick
            Parser_test.test_exists_quantifier;
          test_case "Parse formula with parentheses" `Quick
            Parser_test.test_parentheses;
        ] );
      ( "Unification: Meta-variable resolution",
        [
          test_case "With non-existent variable" `Quick
            Unification_test.test_chase_var_with_non_existent_variable;
          test_case "Basic variable resolution" `Quick
            Unification_test.test_chase_var_basic_variable_resolution;
          test_case "Chained variable resolution" `Quick
            Unification_test.test_chase_var_chained_variable_resolution;
          test_case "Resolves non-variable terms" `Quick
            Unification_test.test_chase_var_resolves_non_variable_term;
        ] );
      ( "Unification: Occurs check",
        [
          test_case "When variable does not occur" `Quick
            Unification_test.test_occurs_in_when_variable_does_not_occur;
          test_case "When variable occurs directly" `Quick
            Unification_test.test_occurs_in_when_varialbe_occurs_directly;
          test_case "When variable occurs in parameter" `Quick
            Unification_test.test_occurs_in_when_variable_occurs_in_param;
          test_case "When variable does not occur in parameter" `Quick
            Unification_test
            .test_occurs_in_when_varialbe_does_not_occur_in_param;
          test_case "When variable occurs in a nested function" `Quick
            Unification_test
            .test_occurs_in_when_variable_occurs_in_nested_function;
          test_case "When variable does not occur in a nested function" `Quick
            Unification_test
            .test_occurs_in_when_varialbe_does_not_occur_in_nested_function;
          test_case "When variable does not occur in an empty function" `Quick
            Unification_test
            .test_occurs_in_when_varialbe_does_not_occur_in_empty_function;
        ] );
      ( "Unification",
        [
          test_case "Unifies predicates" `Quick
            Unification_test.test_unify_unifies_predicates;
          test_case "Fails with mismatched predicate names" `Quick
            Unification_test.test_unify_fails_with_mismatched_predicate_names;
          test_case "Occurs check failure" `Quick
            Unification_test.test_unify_occurs_check_failure;
          test_case "Fails with mismatched term lists length" `Quick
            Unification_test.test_unify_fails_with_mismatched_term_lists_length;
          test_case "Unifies nested functions" `Quick
            Unification_test.test_unify_unifies_nested_functions;
          test_case "Unifies nested functions with variables" `Quick
            Unification_test.test_unify_unifies_nested_functions_with_variables;
          test_case "Fails due to different nested functions" `Quick
            Unification_test.test_unify_fails_due_to_different_nested_functions;
          test_case "Unifies deeply nested structures" `Quick
            Unification_test.test_unify_unifies_deeply_nested_structures;
          test_case "Unifies deep structures with multiple variables" `Quick
            Unification_test
            .test_unify_unifies_deep_structures_with_multiple_variables;
        ] );
      ( "Instantiation: Term",
        [
          test_case "no substitution" `Quick
            Inference_test.test_inst_term_with_no_substitution;
          test_case "single substitution" `Quick
            Inference_test.test_inst_term_with_single_substitution;
          test_case "nested substitution" `Quick
            Inference_test.test_inst_term_nested_substitution;
        ] );
      ( "Instantiation: Formula",
        [
          test_case "no substitution" `Quick
            Inference_test.test_inst_formula_with_no_substitution;
          test_case "with substitution" `Quick
            Inference_test.test_inst_formula_with_substitution;
          test_case "nested substitution" `Quick
            Inference_test.test_inst_formula_with_nested_substitution;
        ] );
      ( "Instantiation: Single Goal",
        [
          test_case "no substitution" `Quick
            Inference_test.test_inst_goal_with_no_substitution;
          test_case "applies substitution" `Quick
            Inference_test.test_inst_goal_single_substitution;
          test_case "nested substitution" `Quick
            Inference_test.test_inst_goal_nested_substitution;
          test_case "multiple substitutions" `Quick
            Inference_test.test_inst_goal_with_multiple_substitutions;
        ] );
      ( "Instantiation: Goal List",
        [
          test_case "no substitution" `Quick
            Inference_test.test_inst_goals_with_no_substitution;
          test_case "with substitution" `Quick
            Inference_test.test_inst_goals_single_substitution;
          test_case "nested substitution" `Quick
            Inference_test.test_inst_goals_nested_substitution;
        ] );
      ( "Inference: Split Goal",
        [
          test_case "empty goal" `Quick
            Inference_test.test_split_goal_empty_goal;
          test_case "mixed goal entries" `Quick
            Inference_test.test_split_goal_mixed_goal_entries;
        ] );
      ( "Inference: Solve Goal",
        [
          test_case "simple predicates" `Quick
            Inference_test.test_solve_goal_basic_unification;
          test_case "no unification" `Quick
            Inference_test.test_solve_goal_no_unification;
          test_case "multiple unification" `Quick
            Inference_test.test_solve_goal_multiple_unification;
          test_case "nested terms" `Quick
            Inference_test.test_solve_goal_nested_terms;
        ] );
      ( "Inference: Insert Goals",
        [
          test_case "solvable" `Quick Inference_test.test_insert_goals_solvable;
          test_case "unsolvable" `Quick
            Inference_test.test_insert_goals_unsolvable;
          test_case "mixed" `Quick Inference_test.test_insert_goals_mixed;
          test_case "nested" `Quick Inference_test.test_insert_goals_nested;
        ] );
    ]
