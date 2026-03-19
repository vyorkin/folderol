open FolderolLib

(** Generate a natural-language explanation of a single proof step. *)
let explain_step i (step : Proof.proof_step) =
  let rule_str = Rule.to_string step.rule in
  let _, side, principal_formula = step.principal in
  let formula_str = Formula.to_string principal_formula in
  let side_str =
    match side with Formula.L -> "the left" | Formula.R -> "the right"
  in
  let action =
    match step.rule with
    | Rule.NotR ->
        Printf.sprintf
          "Applied %s to move '%s' from the right to the left by removing its \
           negation"
          rule_str formula_str
    | Rule.NotL ->
        Printf.sprintf
          "Applied %s to move '%s' from the left to the right by removing its \
           negation"
          rule_str formula_str
    | Rule.ConjR ->
        Printf.sprintf
          "Applied %s to split '%s' on the right into two subgoals, one for \
           each conjunct"
          rule_str formula_str
    | Rule.ConjL ->
        Printf.sprintf
          "Applied %s to decompose '%s' on the left, adding both conjuncts to \
           the left side"
          rule_str formula_str
    | Rule.DisjR ->
        Printf.sprintf
          "Applied %s to decompose '%s' on the right, adding both disjuncts to \
           the right side"
          rule_str formula_str
    | Rule.DisjL ->
        Printf.sprintf
          "Applied %s to split '%s' on the left into two subgoals, one for \
           each disjunct"
          rule_str formula_str
    | Rule.ImplR ->
        Printf.sprintf
          "Applied %s to decompose '%s' on the right: moved the antecedent to \
           the left side and kept the consequent on the right"
          rule_str formula_str
    | Rule.ImplL ->
        Printf.sprintf
          "Applied %s to split '%s' on the left into two subgoals: prove the \
           antecedent (on the right) and use the consequent (on the left)"
          rule_str formula_str
    | Rule.IffR ->
        Printf.sprintf
          "Applied %s to split '%s' on the right into two subgoals, proving \
           the biconditional in both directions"
          rule_str formula_str
    | Rule.IffL ->
        Printf.sprintf
          "Applied %s to split '%s' on the left into two subgoals: one with \
           both sides assumed, one with both sides to prove"
          rule_str formula_str
    | Rule.ForallR ->
        Printf.sprintf
          "Applied %s to '%s' on %s: introduced a fresh parameter for the \
           universally quantified variable"
          rule_str formula_str side_str
    | Rule.ForallL ->
        Printf.sprintf
          "Applied %s to '%s' on %s: introduced a fresh meta-variable (the \
           original quantified formula is retained for further instantiation)"
          rule_str formula_str side_str
    | Rule.ExistsL ->
        Printf.sprintf
          "Applied %s to '%s' on %s: introduced a fresh parameter for the \
           existentially quantified variable"
          rule_str formula_str side_str
    | Rule.ExistsR ->
        Printf.sprintf
          "Applied %s to '%s' on %s: introduced a fresh meta-variable (the \
           original quantified formula is retained for further instantiation)"
          rule_str formula_str side_str
    | Rule.Cut ->
        Printf.sprintf
          "Applied Cut on '%s': created two subgoals, one proving the cut \
           formula and one assuming it"
          formula_str
  in
  let solved_note =
    match step.solved with
    | [] -> ""
    | fs ->
        let fs_str = String.concat ", " (List.map Formula.to_string fs) in
        Printf.sprintf "\n   Solved by axiom (identity): %s" fs_str
  in
  Printf.sprintf "%d. %s%s" (i + 1) action solved_note

(** Print a full natural-language explanation of the proof. *)
let explain () =
  let trace = Proof.get_proof_trace () in
  if List.length trace = 0 then print_endline "No proof steps to explain."
  else (
    print_endline "Proof explanation:";
    print_endline (String.make 40 '-');
    List.iteri (fun i step -> print_endline (explain_step i step)) trace;
    print_endline (String.make 40 '-');
    let n = List.length trace in
    Printf.printf "Total: %d step%s\n" n (if n = 1 then "" else "s"))
