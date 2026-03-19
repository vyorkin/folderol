open Core
open Core.Result.Let_syntax

let default_depth_limit = 100
let goal_table = ref (Goal_table.empty ())
let proof_trees : Proof_tree.t list ref = ref []

type proof_step = {
  goal : Goal.t;
  rule : Rule.t;
  principal : Goal_entry.t;
  solved : Formula.t list;
  num_subgoals : int;
}
(** A single proof step recording what rule was applied, the principal formula
    that was decomposed, and what was solved. *)

let proof_trace : proof_step list ref = ref []

(** Lemma cache: goals whose string representation matches a previously proven
    goal can be skipped. A goal is cached when reducing it produces no new
    unsolved subgoals (all subgoals were immediately solved by axiom detection).
*)
let lemma_cache : (string, unit) Hashtbl.t = Hashtbl.create (module String)

let lemma_cache_hits = ref 0

let is_cached goal =
  let key = Goal.to_string goal in
  Hashtbl.mem lemma_cache key

let cache_goal goal =
  let key = Goal.to_string goal in
  Hashtbl.set lemma_cache ~key ~data:()

let init table =
  goal_table := table;
  proof_trees := [];
  proof_trace := [];
  Hashtbl.clear lemma_cache;
  lemma_cache_hits := 0

let clear () = init (Goal_table.empty ())

let print_step rule goal_entry formulas =
  let goal_entry_str = Goal_entry.to_string goal_entry in
  let rule_str = Rule.to_string rule in
  match formulas with
  | [] -> print_endline (Printf.sprintf "[%s]\n%s" rule_str goal_entry_str)
  | _ ->
      print_endline
        (Printf.sprintf "[%s]\n%s:\n%s" rule_str goal_entry_str
           (String.concat ~sep:", " (List.map ~f:Formula.to_string formulas)))

let run_step = function
  | [] -> Ok []
  | [] :: _ -> Error "Empty goal"
  | (goal_entry :: goal) :: table ->
      let full_goal = goal_entry :: goal in
      (* Check lemma cache *)
      if is_cached full_goal then (
        Int.incr lemma_cache_hits;
        Ok table)
      else
        let%bind rule, subgoals = Goal.reduce goal goal_entry in
        let num_subgoals = List.length subgoals in
        let formulas, table' = Goal_table.insert_goals table (subgoals, []) in
        print_step rule goal_entry formulas;
        proof_trace :=
          {
            goal = full_goal;
            rule;
            principal = goal_entry;
            solved = formulas;
            num_subgoals;
          }
          :: !proof_trace;
        (* Cache if all subgoals were immediately solved *)
        if num_subgoals = List.length formulas then cache_goal full_goal;
        Ok table'

(** Like [run_step] but returns alternative goal tables from different unifier
    choices. The first element is the primary result; the rest are alternatives
    for backtracking. *)
let run_step_with_alternatives = function
  | [] -> (Ok [], [])
  | [] :: _ -> (Error "Empty goal", [])
  | (goal_entry :: goal) :: table -> (
      let full_goal = goal_entry :: goal in
      (* Check lemma cache *)
      if is_cached full_goal then (
        Int.incr lemma_cache_hits;
        (Ok table, []))
      else
        match Goal.reduce goal goal_entry with
        | Error e -> (Error e, [])
        | Ok (rule, subgoals) ->
            let num_subgoals = List.length subgoals in
            let all = Goal_table.insert_goals_all table (subgoals, []) in
            let primary, alternatives =
              match all with
              | [] -> (([], table), [])
              | first :: rest -> (first, rest)
            in
            let formulas, table' = primary in
            print_step rule goal_entry formulas;
            proof_trace :=
              {
                goal = full_goal;
                rule;
                principal = goal_entry;
                solved = formulas;
                num_subgoals;
              }
              :: !proof_trace;
            if num_subgoals = List.length formulas then cache_goal full_goal;
            (Ok table', List.map alternatives ~f:snd))

type choice_point = { table : Goal_table.t; remaining : int }
(** A choice point saves an alternative goal table and how many steps remain. *)

let rec run_steps table n =
  match (table, n) with
  | [], _ -> Ok []
  | _, 0 -> Ok table
  | _, i ->
      let%bind table' = run_step table in
      run_steps table' (i - 1)

(** Proof search failure reasons. *)
type failure_reason = Depth_limit_reached of int | Exhausted_alternatives

let failure_reason_to_string = function
  | Depth_limit_reached limit ->
      Printf.sprintf
        "Depth limit reached (%d steps). Try increasing the limit with 'run \
         <N>'."
        limit
  | Exhausted_alternatives ->
      "Proof search exhausted all alternatives (backtracking found no \
       solution)."

(** Run proof search with backtracking. When a branch fails (reduce error),
    backtracks to try alternative unifier choices from previous steps. *)
let rec run_steps_bt table n limit choice_points =
  match (table, n) with
  | [], _ -> Ok []
  | _, 0 -> Error (Depth_limit_reached limit)
  | _, i -> (
      let primary, alternatives = run_step_with_alternatives table in
      (* Save alternatives as choice points *)
      let choice_points' =
        List.fold alternatives ~init:choice_points ~f:(fun acc alt_table ->
            { table = alt_table; remaining = i - 1 } :: acc)
      in
      match primary with
      | Ok table' -> run_steps_bt table' (i - 1) limit choice_points'
      | Error _ -> backtrack limit choice_points')

and backtrack limit = function
  | [] -> Error Exhausted_alternatives
  | { table; remaining } :: rest -> run_steps_bt table remaining limit rest

(** Apply a specific rule to the current goal. Finds the first goal entry that
    reduces with the given rule and applies it. *)
let apply_rule target_rule = function
  | [] -> Error "No goals"
  | [] :: _ -> Error "Empty goal"
  | (first_entry :: goal) :: table ->
      (* Try each entry in the goal to find one that matches the target rule *)
      let all_entries = first_entry :: goal in
      let rec try_entries tried = function
        | [] ->
            Error
              (Printf.sprintf "No formula in current goal matches rule %s"
                 (Rule.to_string target_rule))
        | entry :: rest -> (
            let remaining = List.rev tried @ rest in
            match Goal.reduce remaining entry with
            | Ok (rule, subgoals) when Rule.equal rule target_rule ->
                let num_subgoals = List.length subgoals in
                let formulas, table' =
                  Goal_table.insert_goals table (subgoals, [])
                in
                print_step rule entry formulas;
                proof_trace :=
                  {
                    goal = all_entries;
                    rule;
                    principal = entry;
                    solved = formulas;
                    num_subgoals;
                  }
                  :: !proof_trace;
                Ok table'
            | _ -> try_entries (entry :: tried) rest)
      in
      try_entries [] all_entries

let step () =
  let%bind table = run_step !goal_table in
  goal_table := table;
  Ok table

let steps n =
  let%bind table = run_steps !goal_table n in
  goal_table := table;
  Ok table

let run ?(limit = default_depth_limit) () =
  match run_steps_bt !goal_table limit limit [] with
  | Ok table ->
      goal_table := table;
      Ok table
  | Error reason -> Error (failure_reason_to_string reason)

let with_goal_table f = f !goal_table
let get_proof_trees () = !proof_trees
let get_proof_trace () = List.rev !proof_trace
let get_lemma_cache_hits () = !lemma_cache_hits
let print_goal_table () = print_endline (Goal_table.to_string !goal_table)

let print_proof_trace () =
  let trace = get_proof_trace () in
  if List.is_empty trace then print_endline "No proof steps recorded."
  else (
    List.iteri trace ~f:(fun i { goal; rule; principal; solved; _ } ->
        let goal_str = Goal.to_string goal in
        let rule_str = Rule.to_string rule in
        let _, _, principal_formula = principal in
        let principal_str = Formula.to_string principal_formula in
        Printf.printf "%d. [%s] %s  {on: %s}" (i + 1) rule_str goal_str
          principal_str;
        (match solved with
        | [] -> ()
        | fs ->
            Printf.printf " => %s"
              (String.concat ~sep:", " (List.map ~f:Formula.to_string fs)));
        Out_channel.newline stdout);
    let hits = !lemma_cache_hits in
    if hits > 0 then
      Printf.printf "(%d lemma cache hit%s)\n" hits
        (if hits = 1 then "" else "s"))

(** Reconstruct a proof tree from the proof trace. Uses a stack-based approach
    that assumes approximately depth-first processing order. *)
let build_proof_tree () =
  let trace = get_proof_trace () in
  if List.is_empty trace then None
  else
    let trace_arr = Array.of_list trace in
    let pos = ref 0 in
    let len = Array.length trace_arr in
    let rec build () =
      if !pos >= len then None
      else
        let step = trace_arr.(!pos) in
        Int.incr pos;
        let num_unsolved = step.num_subgoals - List.length step.solved in
        let axiom_children =
          List.map step.solved ~f:(fun f -> Proof_tree.Axiom (step.goal, f))
        in
        let unsolved_children = build_n num_unsolved in
        Some
          (Proof_tree.Step
             (step.goal, step.rule, axiom_children @ unsolved_children))
    and build_n n =
      if n <= 0 then []
      else
        match build () with
        | None -> []
        | Some tree ->
            let rest = build_n (n - 1) in
            tree :: rest
    in
    let tree = build () in
    (match tree with Some t -> proof_trees := [ t ] | None -> ());
    tree

let print_proof_tree () =
  match build_proof_tree () with
  | None -> print_endline "No proof tree available."
  | Some tree -> print_endline (Proof_tree.to_string_ascii tree)
