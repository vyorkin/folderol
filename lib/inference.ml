type side = L | R [@@deriving eq, show { with_path = false }]
type cost = int
type goal_entry = cost * side * Formula.t
type goal = goal_entry list
type goal_table = goal list

let unify env (f1, f2) = Unification.unify env (f1, f2) |> Result.to_option

let rec inst_term env = function
  | Term.Function (name, args) ->
      let args' = List.map (inst_term env) args in
      Term.Function (name, args')
  | Term.Param (name, var_names) ->
      let vars =
        var_names
        |> List.map (fun v -> inst_term env (Term.Var v))
        |> List.fold_left Term.variable_names []
      in
      Term.Param (name, vars)
  | Term.Var name -> (
      match Env.find env name with
      | Some term -> inst_term env term
      | None -> Term.Var name)
  | term -> term

let rec inst_formula env = function
  | Formula.Pred (name, terms) ->
      let terms' = List.map (inst_term env) terms in
      Formula.Pred (name, terms')
  | Formula.Conn (conn, subformulas) ->
      let subformulas' = List.map (inst_formula env) subformulas in
      Formula.Conn (conn, subformulas')
  | Formula.Quant (quant, var_name, body) ->
      let body' = inst_formula env body in
      Formula.Quant (quant, var_name, body')

let inst_goal env = List.map (fun (c, s, f) -> (c, s, inst_formula env f))
let inst_goals env = List.map (inst_goal env)

let split_goal goal =
  let rec split (ls, rs) = function
    | [] -> (ls, rs)
    | (_, L, x) :: goal -> split (x :: ls, rs) goal
    | (_, R, y) :: goal -> split (ls, y :: rs) goal
  in
  goal |> List.rev |> split ([], [])

let solve_goal goal =
  let filter_pred = List.filter Formula.is_pred in
  let lfs, rfs = split_goal goal in
  let lps, rps = (filter_pred lfs, filter_pred rfs) in
  let rec solve = function
    | [], _ -> []
    | lp :: lps', rps ->
        let rec find_unifier = function
          | [] -> solve (lps', rps)
          | rp :: rps' -> (
              match unify Env.empty (lp, rp) with
              | Some u -> [ (lp, u) ] (* return formula and the unifier *)
              | None -> find_unifier rps' (* keep searching in ∆ *))
        in
        find_unifier rps
  in
  solve (lps, rps)

let rec insert_goals goal_table = function
  | [], fs -> (fs, goal_table)
  | g :: gs, fs -> (
      match solve_goal g with
      | (f, u) :: _ ->
          let goal_table' = inst_goals u goal_table in
          let f' = inst_formula u f in
          let gs' = inst_goals u gs in
          insert_goals goal_table' (gs', f' :: fs)
      | [] ->
          (* not solvable, add as is *)
          insert_goals (g :: goal_table) (gs, fs))

let cost (side, connective) =
  let open Formula in
  match (side, connective) with
  (* 1 subgoal *)
  | _, Conn (Not, _) -> 1
  | L, Conn (Conj, _) -> 1
  | R, Conn (Disj, _) -> 1
  | R, Conn (Impl, _) -> 1
  | R, Quant (Forall, _, _) -> 1
  | L, Quant (Exists, _, _) -> 1
  (* 2 subgoals *)
  | R, Conn (Conj, _) -> 2
  | L, Conn (Disj, _) -> 2
  | L, Conn (Impl, _) -> 2
  | _, Conn (Iff, _) -> 2
  (* quantifier expansion *)
  | L, Quant (Forall, _, _) -> 3
  | R, Quant (Exists, _, _) -> 3
  (* no reductions *)
  | _, _ -> 4

let add_estimation (side, connective) =
  (cost (side, connective), side, connective)
