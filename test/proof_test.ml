open FolderolLib

let goal_table_testable =
  let goal_entry_testable =
    Alcotest.(
      triple int
        (testable Formula.pp_side Formula.equal_side)
        (testable Formula.pp Formula.equal))
  in
  Alcotest.(list (list goal_entry_testable))

(* step *)

let test_step_reduces_single_connective () =
  Symbol.reset ();
  let open Formula in
  (* |- P ∧ Q should split into |- P and |- Q *)
  let formula = Conn (Conj, [ Pred ("P", []); Pred ("Q", []) ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.step () in
  match result with
  | Ok table ->
      (* After ∧R we should have 2 goals: |- P and |- Q *)
      Alcotest.(check int) "two subgoals after ∧R" 2 (List.length table)
  | Error err -> Alcotest.fail ("step failed: " ^ err)

let test_step_solves_via_reduction () =
  Symbol.reset ();
  let open Formula in
  (* |- ¬P ∨ P should be provable:
     ∨R gives |- ¬P, P then ¬R gives P |- P which solves *)
  let p = Pred ("P", []) in
  let formula = Conn (Disj, [ Conn (Not, [ p ]); p ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  (* After step 1: ∨R => |- ¬P, P *)
  let result = Proof.steps 3 in
  match result with
  | Ok table -> Alcotest.(check goal_table_testable) "¬P ∨ P proved" [] table
  | Error err -> Alcotest.fail ("step failed: " ^ err)

let test_step_error_on_empty_goal_table () =
  Proof.init (Goal_table.empty ());
  let result = Proof.step () in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable) "empty table stays empty" [] table
  | Error _ -> Alcotest.fail "should succeed with empty table"

(* steps *)

let test_steps_multiple_reductions () =
  Symbol.reset ();
  let open Formula in
  (* |- (P → P) should be provable: →R gives P |- P, then solve *)
  let formula = Conn (Impl, [ Pred ("P", []); Pred ("P", []) ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.steps 10 in
  match result with
  | Ok table -> Alcotest.(check goal_table_testable) "P → P proved" [] table
  | Error err -> Alcotest.fail ("steps failed: " ^ err)

let test_steps_zero_is_noop () =
  Symbol.reset ();
  let open Formula in
  let formula = Conn (Conj, [ Pred ("P", []); Pred ("Q", []) ]) in
  let initial = Goal_table.mk ([], [ formula ]) in
  Proof.init initial;
  let result = Proof.steps 0 in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable) "0 steps = no change" initial table
  | Error err -> Alcotest.fail ("steps failed: " ^ err)

(* Integration tests: full proofs of known theorems *)

let test_prove_implication_reflexivity () =
  Symbol.reset ();
  let open Formula in
  (* P → P *)
  let formula = Conn (Impl, [ Pred ("P", []); Pred ("P", []) ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.steps 100 in
  match result with
  | Ok table -> Alcotest.(check goal_table_testable) "P → P proved" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err)

let test_prove_double_negation_intro () =
  Symbol.reset ();
  let open Formula in
  (* |- P → ¬¬P *)
  let p = Pred ("P", []) in
  let formula = Conn (Impl, [ p; Conn (Not, [ Conn (Not, [ p ]) ]) ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.steps 100 in
  match result with
  | Ok table -> Alcotest.(check goal_table_testable) "P → ¬¬P proved" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err)

let test_prove_contrapositive () =
  Symbol.reset ();
  let open Formula in
  (* |- (P → Q) → (¬Q → ¬P) *)
  let p = Pred ("P", []) in
  let q = Pred ("Q", []) in
  let formula =
    Conn
      ( Impl,
        [
          Conn (Impl, [ p; q ]);
          Conn (Impl, [ Conn (Not, [ q ]); Conn (Not, [ p ]) ]);
        ] )
  in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.steps 100 in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable) "contrapositive proved" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err)

let test_prove_de_morgan_1 () =
  Symbol.reset ();
  let open Formula in
  (* |- ¬(P ∧ Q) → (¬P ∨ ¬Q) *)
  let p = Pred ("P", []) in
  let q = Pred ("Q", []) in
  let formula =
    Conn
      ( Impl,
        [
          Conn (Not, [ Conn (Conj, [ p; q ]) ]);
          Conn (Disj, [ Conn (Not, [ p ]); Conn (Not, [ q ]) ]);
        ] )
  in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.steps 100 in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable) "De Morgan 1 proved" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err)

let test_prove_excluded_middle () =
  Symbol.reset ();
  let open Formula in
  (* |- P ∨ ¬P *)
  let p = Pred ("P", []) in
  let formula = Conn (Disj, [ p; Conn (Not, [ p ]) ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.steps 100 in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable) "excluded middle proved" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err)

let test_prove_distrib_from_file () =
  Symbol.reset ();
  let open Formula in
  (* ((P | Q) & (P | R)) --> (P | (Q & R)) *)
  let p = Pred ("P", []) in
  let q = Pred ("Q", []) in
  let r = Pred ("R", []) in
  let formula =
    Conn
      ( Impl,
        [
          Conn (Conj, [ Conn (Disj, [ p; q ]); Conn (Disj, [ p; r ]) ]);
          Conn (Disj, [ p; Conn (Conj, [ q; r ]) ]);
        ] )
  in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.steps 100 in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable) "distributivity proved" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err)

let test_prove_assoc_iff () =
  Symbol.reset ();
  let open Formula in
  (* ((P <-> Q) <-> R) |- (P <-> (Q <-> R)) *)
  let p = Pred ("P", []) in
  let q = Pred ("Q", []) in
  let r = Pred ("R", []) in
  let lhs = Conn (Iff, [ Conn (Iff, [ p; q ]); r ]) in
  let rhs = Conn (Iff, [ p; Conn (Iff, [ q; r ]) ]) in
  Proof.init (Goal_table.mk ([ lhs ], [ rhs ]));
  let result = Proof.steps 1000 in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable) "iff associativity proved" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err)

(* Cut rule *)

let test_cut_rule () =
  Symbol.reset ();
  let open Formula in
  (* Goal: |- P ∧ Q with cut formula P.
     Cut produces: |- P, P ∧ Q and P |- P ∧ Q *)
  let pq = Conn (Conj, [ Pred ("P", []); Pred ("Q", []) ]) in
  let goal = [ Goal_entry.mk R pq ] in
  let result = Goal.cut goal (Pred ("P", [])) in
  match result with
  | Ok (rule, subgoals) ->
      Alcotest.(check (testable Rule.pp Rule.equal)) "cut rule" Rule.Cut rule;
      Alcotest.(check int) "two subgoals" 2 (List.length subgoals)
  | Error err -> Alcotest.fail ("cut failed: " ^ err)

(* Depth limit *)

let test_run_with_default_limit () =
  Symbol.reset ();
  let open Formula in
  let formula = Conn (Impl, [ Pred ("P", []); Pred ("P", []) ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.run () in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable)
        "run with default limit proves P → P" [] table
  | Error err -> Alcotest.fail ("run failed: " ^ err)

let test_run_with_small_limit () =
  Symbol.reset ();
  let open Formula in
  (* Simple formula that needs just 1 step *)
  let formula = Conn (Impl, [ Pred ("P", []); Pred ("P", []) ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.run ~limit:1 () in
  match result with
  | Ok table ->
      (* With limit=1, →R is applied, then P |- P is solved by insert_goals *)
      Alcotest.(check goal_table_testable) "limit=1 proves P → P" [] table
  | Error err -> Alcotest.fail ("run failed: " ^ err)

(* Parser negative tests *)

let test_parse_invalid_syntax () =
  let lexbuf = Lexing.from_string "P &&&& Q" in
  let result =
    try
      let _ = Parser.main Lexer.read lexbuf in
      Ok ()
    with
    | Lexer.LexingError _ -> Error "lexer"
    | Parser.Error -> Error "parser"
  in
  match result with
  | Error _ -> () (* expected *)
  | Ok _ -> Alcotest.fail "should fail on invalid syntax"

let test_parse_unclosed_paren () =
  let lexbuf = Lexing.from_string "(P & Q" in
  let result =
    try
      let _ = Parser.main Lexer.read lexbuf in
      Ok ()
    with
    | Lexer.LexingError _ -> Error "lexer"
    | Parser.Error -> Error "parser"
  in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on unclosed paren"

let test_parse_empty_input () =
  let lexbuf = Lexing.from_string "" in
  let result =
    try
      let _ = Parser.main Lexer.read lexbuf in
      Ok ()
    with
    | Lexer.LexingError _ -> Error "lexer"
    | Parser.Error -> Error "parser"
  in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on empty input"

let test_parse_with_comments () =
  let open Formula in
  let input = "-- this is a comment\nP & Q" in
  let lexbuf = Lexing.from_string input in
  let result = Parser.main Lexer.read lexbuf in
  let expected = Conn (Conj, [ Pred ("P", []); Pred ("Q", []) ]) in
  Alcotest.(check (testable Formula.pp Formula.equal))
    "parse with comments" expected result

let test_parse_with_hash_comments () =
  let open Formula in
  let input = "# comment\nP | Q" in
  let lexbuf = Lexing.from_string input in
  let result = Parser.main Lexer.read lexbuf in
  let expected = Conn (Disj, [ Pred ("P", []); Pred ("Q", []) ]) in
  Alcotest.(check (testable Formula.pp Formula.equal))
    "parse with hash comments" expected result

(* Backtracking *)

let test_backtracking_alternative_unifier () =
  (* A formula where multiple atomic predicates are present on both sides
     and the first unifier choice may not lead to a complete proof.
     |- (P ∧ Q) → (Q ∧ P) requires matching P with P and Q with Q,
     not P with Q. The backtracking mechanism should find the right match. *)
  Symbol.reset ();
  let open Formula in
  let p = Pred ("P", []) in
  let q = Pred ("Q", []) in
  let formula = Conn (Impl, [ Conn (Conj, [ p; q ]); Conn (Conj, [ q; p ]) ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.run () in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable)
        "(P ∧ Q) → (Q ∧ P) proved with backtracking" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err)

let test_proof_trace () =
  Symbol.reset ();
  let open Formula in
  (* P → P should produce a trace with one step: →R *)
  let formula = Conn (Impl, [ Pred ("P", []); Pred ("P", []) ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let _result = Proof.run () in
  let trace = Proof.get_proof_trace () in
  Alcotest.(check bool)
    "trace is non-empty after proof" true
    (not (List.is_empty trace));
  match trace with
  | first :: _ ->
      Alcotest.(check (testable Rule.pp Rule.equal))
        "first step is →R" Rule.ImplR first.Proof.rule
  | [] -> Alcotest.fail "trace should have at least one step"

let test_apply_rule_specific () =
  Symbol.reset ();
  let open Formula in
  (* |- P ∧ Q, apply ∧R should split into two goals *)
  let formula = Conn (Conj, [ Pred ("P", []); Pred ("Q", []) ]) in
  let table = Goal_table.mk ([], [ formula ]) in
  Proof.init table;
  let result = Proof.apply_rule Rule.ConjR table in
  match result with
  | Ok table' ->
      Alcotest.(check int) "∧R splits into 2 goals" 2 (List.length table')
  | Error err -> Alcotest.fail ("apply_rule failed: " ^ err)

let test_apply_rule_wrong_rule () =
  Symbol.reset ();
  let open Formula in
  (* |- P ∧ Q, try to apply ∨R — should fail *)
  let formula = Conn (Conj, [ Pred ("P", []); Pred ("Q", []) ]) in
  let table = Goal_table.mk ([], [ formula ]) in
  let result = Proof.apply_rule Rule.DisjR table in
  match result with
  | Ok _ -> Alcotest.fail "should fail when rule doesn't match"
  | Error _ -> ()

let test_rule_of_string () =
  Alcotest.(check (option (testable Rule.pp Rule.equal)))
    "ConjR" (Some Rule.ConjR) (Rule.of_string "ConjR");
  Alcotest.(check (option (testable Rule.pp Rule.equal)))
    "∧R" (Some Rule.ConjR) (Rule.of_string "∧R");
  Alcotest.(check (option (testable Rule.pp Rule.equal)))
    "&R" (Some Rule.ConjR) (Rule.of_string "&R");
  Alcotest.(check (option (testable Rule.pp Rule.equal)))
    "unknown" None (Rule.of_string "xyz")

let test_lemma_cache () =
  (* |- (P → P) ∧ (P → P) — the subgoal P → P appears twice.
     The second occurrence should be resolved via lemma cache. *)
  Symbol.reset ();
  let open Formula in
  let p_impl_p = Conn (Impl, [ Pred ("P", []); Pred ("P", []) ]) in
  let formula = Conn (Conj, [ p_impl_p; p_impl_p ]) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.run () in
  (match result with
  | Ok table -> Alcotest.(check goal_table_testable) "proved" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err));
  let hits = Proof.get_lemma_cache_hits () in
  Alcotest.(check bool) "lemma cache was used" true (hits > 0)

let test_backtracking_quantifier () =
  (* |- ∀x. P(x) → P(x) — simple quantified tautology *)
  Symbol.reset ();
  let open Formula in
  let px = Pred ("P", [ Term.Bound 0 ]) in
  let formula = Quant (Forall, "x", Conn (Impl, [ px; px ])) in
  Proof.init (Goal_table.mk ([], [ formula ]));
  let result = Proof.run () in
  match result with
  | Ok table ->
      Alcotest.(check goal_table_testable) "∀x. P(x) → P(x) proved" [] table
  | Error err -> Alcotest.fail ("proof failed: " ^ err)

let test_parse_dangling_connective () =
  let lexbuf = Lexing.from_string "P &" in
  let result =
    try
      let _ = Parser.main Lexer.read lexbuf in
      Ok ()
    with
    | Lexer.LexingError _ -> Error "lexer"
    | Parser.Error -> Error "parser"
  in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on dangling connective"
