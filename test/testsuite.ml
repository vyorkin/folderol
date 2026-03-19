let () =
  let open Alcotest in
  run "Folderol"
    [
      ( "Symbol",
        [
          test_case "Generate simple symbol" `Quick Symbol_test.test_mk_symbol;
          test_case "Generate multiple symbols" `Quick
            Symbol_test.test_mk_multiple_symbols;
        ] );
      ( "Term: variable_names",
        [
          test_case "Initial list is empty" `Quick
            Term_test.test_variable_names_empty;
          test_case "Existing initial duplicate variable name" `Quick
            Term_test.test_variable_names_existing;
          test_case "Nested " `Quick Term_test.test_variable_names_nested;
          test_case "Mixed variables" `Quick Term_test.test_variable_names_mixed;
          test_case "Single initial var, no vars inside term" `Quick
            Term_test.test_variable_names_const;
          test_case "Multiple variable names" `Quick
            Term_test.test_variable_names_dups;
        ] );
      ( "Term: replace",
        [
          test_case "Replace in a simple term" `Quick
            Term_test.test_replace_in_simple_term;
          test_case "Replace in a nested function" `Quick
            Term_test.test_replace_in_nested_function;
          test_case "Replace does not modify unmatched terms" `Quick
            Term_test.test_replace_does_not_modify_unmatched_terms;
          test_case "Replace in a deeply nested function" `Quick
            Term_test.test_replace_in_deeply_nested_function;
        ] );
      ( "Term: pp",
        [
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
      ( "Formula: abstract",
        [
          test_case "Abstract simple formula" `Quick
            Formula_test.test_abstract_simple_formula;
          test_case "Abstract nested formula" `Quick
            Formula_test.test_abstract_nested_formula;
        ] );
      ( "Formula: substitute",
        [
          test_case "Substitute bound var in simple formula" `Quick
            Formula_test.test_subst_bound_var_simple_formula;
          test_case "Substitute bound var in nested formula" `Quick
            Formula_test.test_subst_bound_var_nested_formula;
        ] );
      ( "Formula: fold_left",
        [
          test_case "empty terms" `Quick Formula_test.test_fold_left_empty;
          test_case "single predicate" `Quick
            Formula_test.test_fold_left_in_a_single_predicate;
          test_case "nested connectives" `Quick
            Formula_test.test_fold_left_in_nested_connectives;
          test_case "deep quantifier" `Quick
            Formula_test.test_fold_left_in_a_deep_quantifier;
          test_case "mixed structure" `Quick
            Formula_test.test_fold_left_in_a_mixed_structure;
        ] );
      ( "Formula: variable_names",
        [
          test_case "empty" `Quick Formula_test.test_variable_names_empty;
          test_case "quantifier bound var" `Quick
            Formula_test.test_variable_names_quantifier_bound_var;
          test_case "nested with init" `Quick
            Formula_test.test_variable_names_nested_with_init;
          test_case "duplicate occurences" `Quick
            Formula_test.test_variable_names_duplicate_occurrences;
        ] );
      ( "Formula: pp",
        [
          test_case "pretty-print conjunction formula" `Quick
            Formula_test.test_pp_conjunction_formula;
          test_case "pretty-print quantified formula" `Quick
            Formula_test.test_pp_quantified_formula;
          test_case "pretty-print implication formula" `Quick
            Formula_test.test_pp_implication;
        ] );
      ( "Parser",
        [
          test_case "parse atomic formula" `Quick
            Parser_test.test_atomic_formula;
          test_case "parse predicate without parameters" `Quick
            Parser_test.test_predicate_without_parameters;
          test_case "parse complex formula with empty predicates" `Quick
            Parser_test.test_complex_formula_with_empty_predicates;
          test_case "parse conjunction formula" `Quick
            Parser_test.test_conjunction;
          test_case "parse implication formula" `Quick
            Parser_test.test_implication;
          test_case "parse formula with forall quantifier" `Quick
            Parser_test.test_forall_quantifier;
          test_case "parse formula with exists quantifier" `Quick
            Parser_test.test_exists_quantifier;
          test_case "parse formula with parentheses" `Quick
            Parser_test.test_parentheses;
        ] );
      ( "Unification: chase_var",
        [
          test_case "with non-existent variable" `Quick
            Unification_test.test_chase_var_with_non_existent_variable;
          test_case "basic variable resolution" `Quick
            Unification_test.test_chase_var_basic_variable_resolution;
          test_case "chained variable resolution" `Quick
            Unification_test.test_chase_var_chained_variable_resolution;
          test_case "resolves non-variable terms" `Quick
            Unification_test.test_chase_var_resolves_non_variable_term;
        ] );
      ( "Unification: occurs_in",
        [
          test_case "when variable does not occur" `Quick
            Unification_test.test_occurs_in_when_variable_does_not_occur;
          test_case "when variable occurs directly" `Quick
            Unification_test.test_occurs_in_when_variable_occurs_directly;
          test_case "when variable occurs in parameter" `Quick
            Unification_test.test_occurs_in_when_variable_occurs_in_param;
          test_case "when variable does not occur in parameter" `Quick
            Unification_test
            .test_occurs_in_when_variable_does_not_occur_in_param;
          test_case "when variable occurs in a nested function" `Quick
            Unification_test
            .test_occurs_in_when_variable_occurs_in_nested_function;
          test_case "when variable does not occur in a nested function" `Quick
            Unification_test
            .test_occurs_in_when_variable_does_not_occur_in_nested_function;
          test_case "when variable does not occur in an empty function" `Quick
            Unification_test
            .test_occurs_in_when_variable_does_not_occur_in_empty_function;
        ] );
      ( "Unification",
        [
          test_case "unifies predicates" `Quick
            Unification_test.test_unify_unifies_predicates;
          test_case "fails with mismatched predicate names" `Quick
            Unification_test.test_unify_fails_with_mismatched_predicate_names;
          test_case "occurs check failure" `Quick
            Unification_test.test_unify_occurs_check_failure;
          test_case "fails with mismatched term lists length" `Quick
            Unification_test.test_unify_fails_with_mismatched_term_lists_length;
          test_case "unifies nested functions" `Quick
            Unification_test.test_unify_unifies_nested_functions;
          test_case "unifies nested functions with variables" `Quick
            Unification_test.test_unify_unifies_nested_functions_with_variables;
          test_case "fails due to different nested functions" `Quick
            Unification_test.test_unify_fails_due_to_different_nested_functions;
          test_case "unifies deeply nested structures" `Quick
            Unification_test.test_unify_unifies_deeply_nested_structures;
          test_case "unifies deep structures with multiple variables" `Quick
            Unification_test
            .test_unify_unifies_deep_structures_with_multiple_variables;
        ] );
      ( "Instantiation: Term",
        [
          test_case "no substitution" `Quick
            Instantiation_test.test_instantiate_term_with_no_substitution;
          test_case "single substitution" `Quick
            Instantiation_test.test_instantiate_term_with_single_substitution;
          test_case "nested substitution" `Quick
            Instantiation_test.test_instantiate_term_nested_substitution;
        ] );
      ( "Instantiation: Formula",
        [
          test_case "no substitution" `Quick
            Instantiation_test.test_instantiate_formula_with_no_substitution;
          test_case "with substitution" `Quick
            Instantiation_test.test_instantiate_formula_with_substitution;
          test_case "nested substitution" `Quick
            Instantiation_test.test_instantiate_formula_with_nested_substitution;
        ] );
      ( "Instantiation: Single goal",
        [
          test_case "no substitution" `Quick
            Instantiation_test.test_instantiate_goal_with_no_substitution;
          test_case "applies substitution" `Quick
            Instantiation_test.test_instantiate_goal_single_substitution;
          test_case "nested substitution" `Quick
            Instantiation_test.test_instantiate_goal_nested_substitution;
          test_case "multiple substitutions" `Quick
            Instantiation_test.test_instantiate_goal_with_multiple_substitutions;
        ] );
      ( "Instantiation: Goal list",
        [
          test_case "no substitution" `Quick
            Instantiation_test.test_instantiate_goals_with_no_substitution;
          test_case "with substitution" `Quick
            Instantiation_test.test_instantiate_goals_single_substitution;
          test_case "nested substitution" `Quick
            Instantiation_test.test_instantiate_goals_nested_substitution;
        ] );
      ( "Goal: fold_left",
        [
          test_case "empty goal and single initial formula" `Quick
            Goal_test.test_fold_left_empty_goal;
          test_case "goal with a single entry and empty formulas" `Quick
            Goal_test.test_fold_left_goal_with_a_single_entry;
          test_case "goal with multiple entries and empty formulas" `Quick
            Goal_test.test_fold_left_goal_with_multiple_entries;
          test_case "initial formulas and a goal with multiple entries" `Quick
            Goal_test
            .test_fold_left_goal_with_multiple_entries_and_initial_formulas;
        ] );
      ( "Goal: variable_names",
        [
          test_case "empty goal" `Quick Goal_test.test_variable_names_empty_goal;
          test_case "simple goal with variables" `Quick
            Goal_test.test_variable_names_simple_goal;
          test_case "with init and duplicates" `Quick
            Goal_test.test_variable_names_with_init_and_duplicates;
        ] );
      ( "Goal: split",
        [
          test_case "empty goal" `Quick Goal_test.test_split_empty_goal;
          test_case "mixed goal entries" `Quick
            Goal_test.test_split_mixed_goal_entries;
        ] );
      ( "Goal: mk_subgoal",
        [
          test_case "empty formulas" `Quick
            Goal_test.test_mk_subgoal_empty_formulas;
          test_case "single formula" `Quick
            Goal_test.test_mk_subgoal_single_formula;
          test_case "multiple formulas" `Quick
            Goal_test.test_mk_subgoal_multiple_formulas;
        ] );
      ( "Goal: mk_subgoals",
        [
          test_case "empty input" `Quick Goal_test.test_mk_subgoals_empty_input;
          test_case "multiple goals" `Quick
            Goal_test.test_mk_subgoals_multiple_goal_sets;
        ] );
      ( "Goal: solve",
        [
          test_case "simple predicates" `Quick
            Goal_test.test_solve_basic_unification;
          test_case "no unification" `Quick Goal_test.test_solve_no_unification;
          test_case "multiple unification" `Quick
            Goal_test.test_solve_multiple_unification;
          test_case "nested terms" `Quick Goal_test.test_solve_nested_terms;
        ] );
      ( "Goal: reduce",
        [
          test_case "¬R rule" `Quick Goal_test.test_reduce_not_right;
          test_case "¬L rule" `Quick Goal_test.test_reduce_not_left;
          test_case "∧R rule" `Quick Goal_test.test_reduce_conj_right;
          test_case "∧L rule" `Quick Goal_test.test_reduce_conj_left;
          test_case "∨R rule" `Quick Goal_test.test_reduce_disj_right;
          test_case "∨L rule" `Quick Goal_test.test_reduce_disj_left;
          test_case "→R rule" `Quick Goal_test.test_reduce_impl_right;
          test_case "→L rule" `Quick Goal_test.test_reduce_impl_left;
          test_case "↔R rule" `Quick Goal_test.test_reduce_iff_right;
          test_case "↔L rule" `Quick Goal_test.test_reduce_iff_left;
          test_case "∀R rule" `Quick Goal_test.test_reduce_forall_right;
          test_case "∀L rule" `Quick Goal_test.test_reduce_forall_left;
          test_case "∃R rule" `Quick Goal_test.test_reduce_exists_right;
          test_case "∃L rule" `Quick Goal_test.test_reduce_exists_left;
        ] );
      ( "Goal: pp",
        [
          test_case "empty goal" `Quick Goal_test.test_to_string_empty_goal;
          test_case "mixed goal" `Quick Goal_test.test_to_string_mixed_goal;
          test_case "complex goal" `Quick Goal_test.test_to_string_complex_goal;
        ] );
      ( "Goal_entry: pp",
        [
          test_case "left side" `Quick Goal_entry_test.test_to_string_left_side;
          test_case "right side" `Quick
            Goal_entry_test.test_to_string_right_side;
          test_case "complex formula" `Quick
            Goal_entry_test.test_to_string_complex_formula;
        ] );
      ( "Goal_table: insert_goals",
        [
          test_case "solvable" `Quick Goal_table_test.test_insert_goals_solvable;
          test_case "unsolvable" `Quick
            Goal_table_test.test_insert_goals_unsolvable;
          test_case "mixed" `Quick Goal_table_test.test_insert_goals_mixed;
          test_case "nested" `Quick Goal_table_test.test_insert_goals_nested;
        ] );
      ( "Goal_table: pp",
        [
          test_case "empty table" `Quick
            Goal_table_test.test_to_string_empty_table;
          test_case "single goal" `Quick
            Goal_table_test.test_to_string_single_goal;
          test_case "multiple goals" `Quick
            Goal_table_test.test_to_string_multiple_goals;
        ] );
      ( "Proof: step",
        [
          test_case "reduces single connective" `Quick
            Proof_test.test_step_reduces_single_connective;
          test_case "solves via reduction" `Quick
            Proof_test.test_step_solves_via_reduction;
          test_case "empty goal table" `Quick
            Proof_test.test_step_error_on_empty_goal_table;
        ] );
      ( "Proof: steps",
        [
          test_case "multiple reductions" `Quick
            Proof_test.test_steps_multiple_reductions;
          test_case "zero steps is noop" `Quick
            Proof_test.test_steps_zero_is_noop;
        ] );
      ( "Proof: integration",
        [
          test_case "P → P" `Quick Proof_test.test_prove_implication_reflexivity;
          test_case "P → ¬¬P" `Quick Proof_test.test_prove_double_negation_intro;
          test_case "contrapositive" `Quick Proof_test.test_prove_contrapositive;
          test_case "De Morgan 1" `Quick Proof_test.test_prove_de_morgan_1;
          test_case "excluded middle" `Quick
            Proof_test.test_prove_excluded_middle;
          test_case "distributivity" `Quick
            Proof_test.test_prove_distrib_from_file;
          test_case "iff associativity" `Quick Proof_test.test_prove_assoc_iff;
        ] );
      ( "Proof: cut rule",
        [
          test_case "cut produces two subgoals" `Quick Proof_test.test_cut_rule;
        ] );
      ( "Proof: depth limit",
        [
          test_case "run with default limit" `Quick
            Proof_test.test_run_with_default_limit;
          test_case "run with small limit" `Quick
            Proof_test.test_run_with_small_limit;
        ] );
      ( "Proof: backtracking",
        [
          test_case "alternative unifier" `Quick
            Proof_test.test_backtracking_alternative_unifier;
          test_case "quantifier" `Quick Proof_test.test_backtracking_quantifier;
        ] );
      ( "Proof: trace",
        [ test_case "records proof steps" `Quick Proof_test.test_proof_trace ]
      );
      ( "Proof: apply rule",
        [
          test_case "applies specific rule" `Quick
            Proof_test.test_apply_rule_specific;
          test_case "fails on wrong rule" `Quick
            Proof_test.test_apply_rule_wrong_rule;
        ] );
      ( "Rule: of_string",
        [ test_case "parses rule names" `Quick Proof_test.test_rule_of_string ]
      );
      ( "Proof: lemma cache",
        [
          test_case "caches repeated subgoals" `Quick
            Proof_test.test_lemma_cache;
        ] );
      ( "Parser: negative",
        [
          test_case "invalid syntax" `Quick Proof_test.test_parse_invalid_syntax;
          test_case "unclosed paren" `Quick Proof_test.test_parse_unclosed_paren;
          test_case "empty input" `Quick Proof_test.test_parse_empty_input;
          test_case "dangling connective" `Quick
            Proof_test.test_parse_dangling_connective;
          test_case "with -- comments" `Quick
            Proof_test.test_parse_with_comments;
          test_case "with # comments" `Quick
            Proof_test.test_parse_with_hash_comments;
        ] );
      ( "Proof tree",
        [
          test_case "simple P → P" `Quick Proof_test.test_proof_tree_simple;
          test_case "conjunction swap" `Quick Proof_test.test_proof_tree_conj;
          test_case "None when empty" `Quick
            Proof_test.test_proof_tree_none_when_empty;
        ] );
      ( "Proof tree: LaTeX",
        [ test_case "LaTeX export" `Quick Proof_test.test_latex_export ] );
      ( "Proof trace: principal",
        [
          test_case "trace has principal formula" `Quick
            Proof_test.test_trace_has_principal;
          test_case "trace records num_subgoals" `Quick
            Proof_test.test_trace_num_subgoals;
        ] );
      ( "Rule: num_subgoals",
        [ test_case "subgoal counts" `Quick Proof_test.test_rule_num_subgoals ]
      );
      ( "Proof: error messages",
        [
          test_case "depth limit error" `Quick
            Proof_test.test_run_depth_limit_error;
        ] );
    ]
