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

(** Pretty-print the proof tree as an indented ASCII tree. *)
let pp_ascii fmt tree =
  let open Format in
  let rec pp_node prefix is_last = function
    | Axiom (goal, formula) ->
        let connector = if is_last then "└── " else "├── " in
        fprintf fmt "%s%s%a  (axiom: %a)@," prefix connector Goal.pp goal
          Formula.pp_formula formula
    | Step (goal, rule, children) ->
        let connector = if is_last then "└── " else "├── " in
        fprintf fmt "%s%s[%a] %a@," prefix connector Rule.pp rule Goal.pp goal;
        let child_prefix = prefix ^ if is_last then "    " else "│   " in
        let n = List.length children in
        List.iteri
          (fun i child -> pp_node child_prefix (i = n - 1) child)
          children
  in
  fprintf fmt "@[<v>";
  (match tree with
  | Axiom (goal, formula) ->
      fprintf fmt "%a  (axiom: %a)@," Goal.pp goal Formula.pp_formula formula
  | Step (goal, rule, children) ->
      fprintf fmt "[%a] %a@," Rule.pp rule Goal.pp goal;
      let n = List.length children in
      List.iteri (fun i child -> pp_node "" (i = n - 1) child) children);
  fprintf fmt "@]"

let to_string_ascii = format_to_string pp_ascii

(** Emit a LaTeX proof tree using the bussproofs package. *)
let to_latex tree =
  let buf = Buffer.create 256 in
  let add = Buffer.add_string buf in
  let rec emit = function
    | Axiom (goal, _formula) ->
        add (Printf.sprintf "\\AxiomC{$%s$}\n" (Goal.to_string goal))
    | Step (goal, rule, children) -> (
        match children with
        | [] -> add (Printf.sprintf "\\AxiomC{$%s$}\n" (Goal.to_string goal))
        | _ ->
            List.iter emit children;
            let cmd =
              match List.length children with
              | 1 -> "\\UnaryInfC"
              | 2 -> "\\BinaryInfC"
              | 3 -> "\\TrinaryInfC"
              | _ -> "\\UnaryInfC"
            in
            add
              (Printf.sprintf "\\RightLabel{\\scriptsize %s}\n"
                 (Rule.to_string rule));
            add (Printf.sprintf "%s{$%s$}\n" cmd (Goal.to_string goal)))
  in
  add "\\begin{prooftree}\n";
  emit tree;
  add "\\end{prooftree}\n";
  Buffer.contents buf
