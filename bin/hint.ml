open FolderolLib

(** Describe what a rule does for the hint. *)
let describe_rule rule formula =
  let formula_str = Formula.to_string formula in
  match rule with
  | Rule.NotR ->
      Printf.sprintf "Apply %s to move '%s' to the left (removing negation)"
        (Rule.to_string rule) formula_str
  | Rule.NotL ->
      Printf.sprintf "Apply %s to move '%s' to the right (removing negation)"
        (Rule.to_string rule) formula_str
  | Rule.ConjR ->
      Printf.sprintf "Apply %s to split '%s' into two subgoals"
        (Rule.to_string rule) formula_str
  | Rule.ConjL ->
      Printf.sprintf
        "Apply %s to decompose '%s' (both conjuncts join the left side)"
        (Rule.to_string rule) formula_str
  | Rule.DisjR ->
      Printf.sprintf
        "Apply %s to decompose '%s' (both disjuncts join the right side)"
        (Rule.to_string rule) formula_str
  | Rule.DisjL ->
      Printf.sprintf "Apply %s to split '%s' into two subgoals"
        (Rule.to_string rule) formula_str
  | Rule.ImplR ->
      Printf.sprintf
        "Apply %s to '%s' (move antecedent left, keep consequent right)"
        (Rule.to_string rule) formula_str
  | Rule.ImplL ->
      Printf.sprintf "Apply %s to split '%s' (prove antecedent, use consequent)"
        (Rule.to_string rule) formula_str
  | Rule.IffR ->
      Printf.sprintf "Apply %s to '%s' (prove both directions)"
        (Rule.to_string rule) formula_str
  | Rule.IffL ->
      Printf.sprintf "Apply %s to decompose '%s' on the left"
        (Rule.to_string rule) formula_str
  | Rule.ForallR ->
      Printf.sprintf "Apply %s to '%s' (introduce fresh parameter)"
        (Rule.to_string rule) formula_str
  | Rule.ForallL ->
      Printf.sprintf
        "Apply %s to '%s' (introduce fresh variable for instantiation)"
        (Rule.to_string rule) formula_str
  | Rule.ExistsL ->
      Printf.sprintf "Apply %s to '%s' (introduce fresh parameter)"
        (Rule.to_string rule) formula_str
  | Rule.ExistsR ->
      Printf.sprintf
        "Apply %s to '%s' (introduce fresh variable for instantiation)"
        (Rule.to_string rule) formula_str
  | Rule.Cut -> Printf.sprintf "Apply Cut on '%s'" formula_str

(** Inspect the current goal table and suggest the next rule to apply. *)
let hint () =
  Proof.with_goal_table (fun table ->
      match table with
      | [] -> print_endline "No goals remaining. The proof is complete!"
      | [] :: _ -> print_endline "Empty goal (this shouldn't happen)."
      | (entry :: rest) :: _ -> (
          let cost, side, formula = entry in
          let side_str =
            match side with Formula.L -> "left" | Formula.R -> "right"
          in
          (* Try reducing to see what rule applies *)
          match Goal.reduce rest entry with
          | Ok (rule, _subgoals) ->
              Printf.printf
                "Hint: The cheapest formula (cost=%d) is '%s' on the %s.\n" cost
                (Formula.to_string formula)
                side_str;
              Printf.printf "  -> %s\n" (describe_rule rule formula);
              Printf.printf
                "  (Use 'step' to apply, or 'apply %s' to be explicit)\n"
                (Rule.to_string rule)
          | Error _ ->
              Printf.printf
                "The cheapest formula (cost=%d) is '%s' on the %s, but it is \
                 atomic.\n"
                cost
                (Formula.to_string formula)
                side_str;
              print_endline
                "  -> No rule can reduce it. The proof may need backtracking \
                 or a different approach."))
