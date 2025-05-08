open Core
open Pretty_printing
open Instantiation

type t = Goal.t list

let empty () = []

let mk (l, r) =
  let mk_goals side = List.map ~f:(Goal_entry.mk side) in
  let gamma = mk_goals L l in
  let delta = mk_goals R r in
  let goal = Goal.mk @@ gamma @ delta in
  [ goal ]

let rec insert_goals table = function
  | [], fs -> (fs, table)
  | g :: gs, fs -> (
      match Goal.solve g with
      | (f, u) :: _ ->
          (* Instantiate other goals with the resulting unifier. *)
          let goal_table' = instantiate_goals u table in
          let f' = instantiate_formula u f in
          (* Instantiate remaining goals. *)
          let gs' = instantiate_goals u gs in
          insert_goals goal_table' (gs', f' :: fs)
      | [] ->
          (* Not solvable, add as is. *)
          insert_goals (g :: table) (gs, fs))

let pp fmt goal_table =
  let open Format in
  fprintf fmt "@[<v>";
  if List.is_empty goal_table then fprintf fmt "∅"
  else
    List.iteri goal_table ~f:(fun i goal ->
        if i > 0 then fprintf fmt "@,";
        fprintf fmt "%d: %a" i Goal.pp goal);
  fprintf fmt "@]"

let to_string = format_to_string pp
