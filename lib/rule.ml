open Pretty_printing

(** Gentzen's LK sequent calculus rule. *)
type t =
  (* Propositional logic rules. *)
  | NotR
  | NotL
  | ConjR
  | ConjL
  | DisjR
  | DisjL
  | ImplR
  | ImplL
  | IffR
  | IffL
  (* Quantifier rules. *)
  | ForallR
  | ForallL
  | ExistsL
  | ExistsR
  (* Structural rule. *)
  | Cut
[@@deriving eq, show { with_path = false }]

let pp fmt rule =
  let open Format in
  match rule with
  | NotR -> fprintf fmt "¬R"
  | NotL -> fprintf fmt "¬L"
  | ConjR -> fprintf fmt "∧R"
  | ConjL -> fprintf fmt "∧L"
  | DisjR -> fprintf fmt "∨R"
  | DisjL -> fprintf fmt "∨L"
  | ImplR -> fprintf fmt "→R"
  | ImplL -> fprintf fmt "→L"
  | IffR -> fprintf fmt "↔R"
  | IffL -> fprintf fmt "↔L"
  | ForallR -> fprintf fmt "∀R"
  | ForallL -> fprintf fmt "∀L"
  | ExistsL -> fprintf fmt "∃L"
  | ExistsR -> fprintf fmt "∃R"
  | Cut -> fprintf fmt "Cut"

let to_string = format_to_string pp

(** Parse a rule name from a string. Accepts both Unicode and ASCII forms. *)
let of_string = function
  | "¬R" | "~R" | "NotR" -> Some NotR
  | "¬L" | "~L" | "NotL" -> Some NotL
  | "∧R" | "&R" | "ConjR" -> Some ConjR
  | "∧L" | "&L" | "ConjL" -> Some ConjL
  | "∨R" | "|R" | "DisjR" -> Some DisjR
  | "∨L" | "|L" | "DisjL" -> Some DisjL
  | "→R" | "ImplR" -> Some ImplR
  | "→L" | "ImplL" -> Some ImplL
  | "↔R" | "IffR" -> Some IffR
  | "↔L" | "IffL" -> Some IffL
  | "∀R" | "ForallR" -> Some ForallR
  | "∀L" | "ForallL" -> Some ForallL
  | "∃R" | "ExistsR" -> Some ExistsR
  | "∃L" | "ExistsL" -> Some ExistsL
  | "Cut" -> Some Cut
  | _ -> None
