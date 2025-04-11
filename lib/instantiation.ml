module List = Core.List

let rec instantiate_term env = function
  | Term.Function (name, args) ->
      let args' = List.map ~f:(instantiate_term env) args in
      Term.Function (name, args')
  | Term.Param (name, var_names) ->
      let vars =
        var_names
        |> List.map ~f:(fun v -> instantiate_term env (Var v))
        |> List.fold_left ~f:Term.variable_names ~init:[]
      in
      Param (name, vars)
  | Term.Var name -> (
      match Env.find env name with
      | Some term -> instantiate_term env term
      | None -> Var name)
  | term -> term

let rec instantiate_formula env = function
  | Formula.Pred (name, terms) ->
      let terms' = List.map ~f:(instantiate_term env) terms in
      Formula.Pred (name, terms')
  | Formula.Conn (conn, subformulas) ->
      let subformulas' = List.map ~f:(instantiate_formula env) subformulas in
      Formula.Conn (conn, subformulas')
  | Formula.Quant (quant, var_name, body) ->
      let body' = instantiate_formula env body in
      Formula.Quant (quant, var_name, body')

let instantiate_goal env =
  List.map ~f:(fun (c, s, f) -> (c, s, instantiate_formula env f))

let instantiate_goals env = List.map ~f:(instantiate_goal env)
