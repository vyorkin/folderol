open Pretty_printing

type t = Axiom of Goal.t * Formula.t | Step of Goal.t * Rule.t * t list
[@@deriving show { with_path = false }]

let rec equal a b =
  match (a, b) with
  | Axiom (g1, f1), Axiom (g2, f2) ->
      List.for_all2 Goal_entry.equal g1 g2 && Formula.equal f1 f2
  | Step (g1, r1, cs1), Step (g2, r2, cs2) ->
      List.for_all2 Goal_entry.equal g1 g2
      && Rule.equal r1 r2
      && List.length cs1 = List.length cs2
      && List.for_all2 equal cs1 cs2
  | _ -> false

let goal = function Axiom (g, _) -> g | Step (g, _, _) -> g

let rec pp fmt = function
  | Axiom (goal, formula) ->
      Format.fprintf fmt "@[<v>%a (axiom: %a)@]" Goal.pp goal Formula.pp_formula
        formula
  | Step (goal, rule, children) ->
      Format.fprintf fmt "@[<v 2>%a [%a]" Goal.pp goal Rule.pp rule;
      List.iter (fun child -> Format.fprintf fmt "@,%a" pp child) children;
      Format.fprintf fmt "@]"

let to_string = format_to_string pp
