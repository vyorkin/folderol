open Instantiation

type t = Goal.t list

let rec insert_goals goal_table = function
  | [], fs -> (fs, goal_table)
  | g :: gs, fs -> (
      match Goal.solve g with
      | (f, u) :: _ ->
          (* instantiate other goals with the resulting unifier *)
          let goal_table' = instantiate_goals u goal_table in
          let f' = instantiate_formula u f in
          (* instantiate remaining goals *)
          let gs' = instantiate_goals u gs in
          insert_goals goal_table' (gs', f' :: fs)
      | [] ->
          (* not solvable, add as is *)
          insert_goals (g :: goal_table) (gs, fs))
